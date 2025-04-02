{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit mmpProgramUpdates;

interface

uses
  system.zip,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  IProgramUpdates = interface
    ['{6C5FC828-6B3A-49C5-A610-657C954953D0}']
    function getReleaseNotesFilePath(const aReleaseTag: string): string;
    function getReleaseTag: string;
    function hasReleaseNotes(const aReleaseTag: string): boolean;
    property releaseTag:    string read getReleaseTag; // has a couple of side-effects ;)
  end;

function newProgramUpdates: IProgramUpdates;

implementation

uses
  idHTTP, idSSLOpenSSL, idComponent,
  system.json, system.classes, system.sysUtils, system.strUtils,
  vcl.forms,
  mmpConsts, mmpFileUtils, mmpFuncProcs, mmpUtils,
  view.mmpFormDownload, view.mmpProgressBar,
  model.mmpConfigFile,
  _debugWindow;

type
  TProgramUpdates = class(TInterfacedObject, IProgramUpdates)
  strict private
    FReleaseNotes: string;
    FReleaseTag: string;
  protected
    function  analyseReleaseNotes(const aReleaseTag: string): boolean;
    function  downloadAsset(const aURL, aFilePath: string; const aSuccess: string = ''): string;
    function  downloadRelease(const aReleaseTag: string): string;
    function  extractRelease(const aReleaseTag: string): boolean;
    function  getJSONReleaseTag: string;
    function  saveReleaseNotes(const aReleaseTag: string): boolean;
    procedure zipOnProgress(sender: TObject; aFileName: string; aHeader: TZipHeader; aPosition: Int64);
  public
    function  getReleaseNotesFilePath(const aReleaseTag: string): string;
    function  getReleaseNotesFolder: string;
    function  getReleaseTag: string;
    function  hasReleaseNotes(const aReleaseTag: string): boolean;
  end;

  TWorkProgress = class(TObject)  // only because IdHttp requires these callbacks to be procedure of object
    procedure idHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
    procedure idHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
    procedure idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

var
  gWP:            TWorkProgress;
  gProgressBar:   IProgressBar;
  gDownloadForm:  TDownloadForm;

function newProgramUpdates: IProgramUpdates;
begin
  result := TProgramUpdates.create;
end;

function cleanTag(const aReleaseTag: string): string;
begin
  result := replaceStr(aReleaseTag, '.', '_');
end;

function fetchURL(const aURL: string; aFileStream: TStream = NIL; const aSuccess: string = ''): string;
var
  http:       TidHTTP;
  sslHandler: TidSSLIOHandlerSocketOpenSSL;
begin
  result        := aSuccess;
  gWP           := NIL;
  gProgressBar  := NIL;
  gDownloadForm := NIL;

  http := TidHTTP.create(nil);
  http.request.contentEncoding := 'UTF-8';

  sslHandler := TidSSLIOHandlerSocketOpenSSL.create(nil);
  sslHandler.sslOptions.sslVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

  http.IOHandler       := sslHandler;
  http.handleRedirects := TRUE;

  try
    try
      case aFileStream = NIL of  TRUE:  result := http.get(aUrl);     // just get the JSON release data
                                FALSE:  begin
                                          gWP := TWorkProgress.create;
                                          http.OnWorkBegin := gWP.idHTTPWorkBegin;
                                          http.OnWork      := gWP.idHTTPWork;
                                          http.OnWorkEnd   := gWP.idHTTPEnd;

                                          gDownloadForm := TDownloadForm.create(NIL);
                                          gProgressBar  := newProgressBar.initProgressBar(gDownloadForm, 0, PB_COLOR_DELTA);

                                          try
                                            gDownloadForm.show;
                                            http.get(aURL, aFileStream); // download the file
                                          finally
                                            case gWP           <> NIL of TRUE: freeAndNIL(gWP); end;
                                            case gDownloadForm <> NIL of TRUE: freeAndNIL(gDownloadForm); end;
                                          end;end;end;
    except
      on e:exception do result := e.Message; // if there's an error (e.g. 404), report it back to the About Box via the result, overriding aSuccess.
    end;
  finally
    sslHandler.free;
    http.free;
  end;
end;

function updateFile(const aReleaseTag: string): string;
begin
  result := mmpExePath + 'update_' + cleanTag(aReleaseTag) + '.zip';
end;

{ TWorkProgress }

procedure TWorkProgress.idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
//
end;

procedure TWorkProgress.idHTTPWork(aSender: TObject; aWorkMode: TWorkMode; aWorkCount: int64);
begin
  gProgressBar.notify(mmpDo(evPBPosition, aWorkCount));
  gDownloadForm.byteLabel.caption := format('%n of %n', [aWorkCount * 1.0, gProgressBar.notify(mmpDo(evPBReqMax)).integer * 1.0]);
  gDownloadForm.byteLabel.Refresh;
  mmpProcessMessages;
end;

procedure TWorkProgress.idHTTPWorkBegin(aSender: TObject; aWorkMode: TWorkMode; aWorkCountMax: int64);
begin
  gProgressBar.notify(mmpDo(evPBMax, aWorkCountMax));
end;

{ TProgramUpdates }

function TProgramUpdates.analyseReleaseNotes(const aReleaseTag: string): boolean;
// download any and all images in the release notes
begin
  case fileExists(getReleaseNotesFilePath(aReleaseTag)) of FALSE: EXIT; end;
  var vNotes := TStringList.create;
  try
    vNotes.loadFromFile(getReleaseNotesFilePath(aReleaseTag));
    for var i := 0 to vNotes.count - 1 do begin
      var vPos1 := pos('(https://github.com/', vNotes[i]);
      case vPos1 = 0 of TRUE: CONTINUE; end;

      var vPos4 := pos('/assets/', vNotes[i]);
      case vPos4 = 0 of TRUE: CONTINUE; end;

      var vAssetURL := copy(vNotes[i], vPos1 + 1, 255);
      var vPos2 := pos(')', vAssetURL);
      case vPos2 = 0 of TRUE: CONTINUE; end;

      delete(vAssetURL, vPos2, 255);

      var vPos3 := LastDelimiter('/', vAssetURL);
      case vPos3 = 0 of TRUE: CONTINUE; end;

      var vFileName := copy(vAssetURL, vPos3 + 1, 255);

      downloadAsset(vAssetURL, getReleaseNotesFolder + vFileName);

      vNotes[i] := replaceStr(vNotes[i], vAssetURL, 'releaseNotes\' + vFileName);
    end;

    vNotes.saveToFile(getReleaseNotesFilePath(aReleaseTag));
  finally
    vNotes.free;
  end;
end;

function TProgramUpdates.downloadAsset(const aURL: string; const aFilePath: string; const aSuccess: string = ''): string;
begin
  case fileExists(aFilePath) of TRUE: EXIT; end;

  var fs := TFileStream.create(aFilePath, fmCreate);
  try
    result := fetchURL(aURL, fs, aSuccess);
  finally
    fs.free;
  end;
end;

function TProgramUpdates.downloadRelease(const aReleaseTag: string): string;
begin
  result := aReleaseTag;
//  EXIT; // for testing release notes only without affecting the download stats

  case  aReleaseTag = ''                                                                of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> '') AND (mmpFileVersionFmt('', 'v%d.%d.%d') = aReleaseTag)       of TRUE: EXIT; end; // we're running the latest release
  case (aReleaseTag <> '') AND (fileExists(updateFile(aReleaseTag)))                    of TRUE: EXIT; end; // we've already downloaded the release file

  result := downloadAsset('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/download/' + aReleaseTag + '/MinimalistMediaPlayer_' + cleanTag(aReleaseTag) + '.full.zip', updateFile(aReleaseTag), aReleaseTag);
end;

function TProgramUpdates.extractRelease(const aReleaseTag: string): boolean;
  function backupName: string;
  begin
    result := 'MinimalistMediaPlayer ' + mmpFileVersionFmt('', 'v%d_%d_%d');
  end;
begin
  result := FALSE;
  case  aReleaseTag = ''                                                             of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> '') AND (mmpFileVersionFmt('', 'v%d.%d.%d') = aReleaseTag)    of TRUE: EXIT; end; // we're running the latest release

  case fileExists(mmpExePath + backupName + '.exe')    of FALSE: mmpRenameFile(paramStr(0), backupName); end;
  case fileExists(paramStr(0))                         of FALSE: with TZipFile.create do begin
                                                                    OnProgress := zipOnProgress;
                                                                    open(updateFile(aReleaseTag), zmRead);
//                                                                    extractAll(mmpExePath + backupName);
                                                                    extract('MinimalistMediaPlayer.exe', mmpExePath);
                                                                    free;
                                                                    result := fileExists(paramStr(0));
                                                                  end;end;
end;

function TProgramUpdates.getJSONReleaseTag: string;
var
  json: string;
  obj:  TJSONObject;

  function getDevJson(const aTag: string): string;
  begin
    with TStringList.create do begin
      loadFromFile(getReleaseNotesFilePath(aTag)); // you need to create this file manually
      result := text;
      free;
    end;
  end;

//  json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/tags/v2.0.0'); // for DEV only

begin
  result := '';
  json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/latest');

////=== DEV ONLY ===
//  json := getDevJson('v3.0.0');
////=== DEV ONLY ===

  try
    obj := TJSONObject.ParseJSONValue(json) as TJSONObject;
    try
      case obj = NIL of FALSE: result        := obj.values['tag_name'].value; end;
      case obj = NIL of FALSE: FReleaseNotes := obj.values['body'].value; end;
    except
    end;
  finally
    case obj = NIL of FALSE: obj.free; end;
  end;

////=== DEV ONLY ===
//  result := 'v3.0.0';
////=== DEV ONLY ===
end;

function TProgramUpdates.getReleaseNotesFilePath(const aReleaseTag: string): string;
begin
  result := format('%s%s%s%s', [getReleaseNotesFolder, 'releaseNotes ', cleanTag(aReleaseTag), '.md'])
end;

function TProgramUpdates.getReleaseNotesFolder: string;
begin
  result := mmpExePath + 'releaseNotes\';
  forceDirectories(result);
end;

function TProgramUpdates.getReleaseTag: string;
begin
  result := FReleaseTag;
  case result <> '' of TRUE: EXIT; end;

  result := '(autoUpdate=no)';
  case CF.asBoolean[CONF_AUTO_UPDATE] of FALSE: EXIT; end;

  result := '(not available)';
  FReleaseTag := getJSONReleaseTag;
  case FReleaseTag = '' of TRUE: EXIT; end;

  saveReleaseNotes(FReleaseTag);
  analyseReleaseNotes(FReleaseTag);

  result := downloadRelease(FReleaseTag); // if there's an error, report it back to the About Box via the result

  case (result = FReleaseTag) and fileExists(updateFile(FReleaseTag)) of TRUE: case extractRelease(FReleaseTag) of TRUE: result := result + ' Restart_Required'; end;end;
end;

function TProgramUpdates.hasReleaseNotes(const aReleaseTag: string): boolean;
begin
  result := fileExists(getReleaseNotesFilePath(aReleaseTag));
end;

function TProgramUpdates.saveReleaseNotes(const aReleaseTag: string): boolean;
begin
  case FReleaseNotes = '' of TRUE: EXIT; end;

  with TStringList.create do begin
    text := FReleaseNotes;
    saveToFile(getReleaseNotesFilePath(aReleaseTag));
    free;
  end;
end;

procedure TProgramUpdates.zipOnProgress(sender: TObject; aFileName: string; aHeader: TZipHeader; aPosition: Int64);
begin
//
end;

initialization
  gWP            := NIL;
  gProgressBar   := NIL;
  gDownloadForm  := NIL;

end.
