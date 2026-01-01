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
    function getReleaseTag: string;
    function hasReleaseNotes(const aReleaseTag: string): boolean;
    property releaseTag:    string read getReleaseTag; // has a couple of side-effects ;)
  end;

function newProgramUpdates: IProgramUpdates;
function mmpCleanTag(const aReleaseTag: string): string;
function mmpReleaseNotesFilePath(const aReleaseTag: string): string;
function mmpReleaseNotesFolder: string;

implementation

uses
  idHTTP, idSSLOpenSSL, idComponent,
  system.json, system.classes, system.sysUtils, system.strUtils,
  vcl.forms,
  mmpConsts, mmpFileUtils, mmpCmd, mmpUtils,
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
    function  downloadAsset(const aURL, aFilePath: string; const aSuccess: string = EMPTY): string;
    function  downloadRelease(const aReleaseTag: string): string;
    function  extractRelease(const aReleaseTag: string): boolean;
    function  getJSONReleaseTag: string;
    function  saveReleaseNotes(const aReleaseTag: string): boolean;
    procedure zipOnProgress(sender: TObject; aFileName: string; aHeader: TZipHeader; aPosition: Int64);
  public
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

function mmpCleanTag(const aReleaseTag: string): string;
begin
  result := replaceStr(aReleaseTag, '.', '_');
end;

function mmpReleaseNotesFilePath(const aReleaseTag: string): string;
begin
  result := format('%s%s%s%s', [mmpReleaseNotesFolder, 'releaseNotes ', mmpCleanTag(aReleaseTag), '.md'])
end;

function mmpReleaseNotesFolder: string;
begin
  result := mmpExePath + 'releaseNotes\';
  forceDirectories(result);
end;

function newProgramUpdates: IProgramUpdates;
begin
  result := TProgramUpdates.create;
end;

function fetchURL(const aURL: string; aFileStream: TStream = NIL; const aSuccess: string = EMPTY): string;
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
  result := mmpExePath + 'update_' + mmpCleanTag(aReleaseTag) + '.zip';
end;

{ TWorkProgress }

procedure TWorkProgress.idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
//
end;

procedure TWorkProgress.idHTTPWork(aSender: TObject; aWorkMode: TWorkMode; aWorkCount: int64);
begin
  gProgressBar.notify(newNotice(evPBPosition, aWorkCount));
  gDownloadForm.byteLabel.caption := format('%n of %n', [aWorkCount * 1.0, gProgressBar.notify(newNotice(evPBReqMax)).integer * 1.0]);
  gDownloadForm.byteLabel.Refresh;
  mmpProcessMessages;
end;

procedure TWorkProgress.idHTTPWorkBegin(aSender: TObject; aWorkMode: TWorkMode; aWorkCountMax: int64);
begin
  gProgressBar.notify(newNotice(evPBMax, aWorkCountMax));
end;

{ TProgramUpdates }

function TProgramUpdates.analyseReleaseNotes(const aReleaseTag: string): boolean;
// download any and all images in the release notes (nope: GitHub broke direct access and redirect access - see comment below)
// As of v4.1.3 this just modifies the release notes so each URL for an image is replaced with a local path to the file in the releaseNotes folder
// The image files themselves are now included in the zip file
// v5.1.7: GitHub changed image URLS again:
// <img width="576" height="275" alt="2025-12-01_181634 png recyclebin2" src="https://github.com/user-attachments/assets/73576cbd-5151-40ea-b20e-bbfab3bf6b97" />
begin
  case fileExists(mmpReleaseNotesFilePath(aReleaseTag)) of FALSE: EXIT; end;
  var vNotes := TStringList.create;
  try
    vNotes.loadFromFile(mmpReleaseNotesFilePath(aReleaseTag));
    for var i := 0 to vNotes.count - 1 do begin

      var vPos1 := pos('<img ', vNotes[i]);
      case vPos1 = 0 of TRUE: CONTINUE; end;

      var vPos2 := pos('" />', vNotes[i]); // could have used lastDelimiter('"', vNotes[i])
      case vPos2 = 0 of TRUE: CONTINUE; end;

      var vAssetURL := vNotes[i];

      delete(vAssetURL, vPos2, 255); // remove all characters after the filename

      var vPos3 := lastDelimiter('/', vAssetURL); // the slash before the filename
      case vPos3 = 0 of TRUE: CONTINUE; end;

      var vFileName := copy(vAssetURL, vPos3 + 1, 255);

// GitHub broke direct access to the images in release notes. We now have to distribute the images with the release
//      downloadAsset(vAssetURL, getReleaseNotesFolder + vFileName); // this now gives 404 for the latest images on github which redirect to amazonaws

      vNotes[i] := format('%s%s%s', ['![] (releaseNotes\', vFileName, ')']); // markdown e.g. ![] (releaseNotes\73576cbd-5151-40ea-b20e-bbfab3bf6b97)
    end;

    vNotes.saveToFile(mmpReleaseNotesFilePath(aReleaseTag));
  finally
    vNotes.free;
  end;
end;

function TProgramUpdates.downloadAsset(const aURL: string; const aFilePath: string; const aSuccess: string = EMPTY): string;
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

  case  aReleaseTag =  EMPTY                                                               of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> EMPTY) AND (mmpFileVersionFmt(EMPTY, 'v%d.%d.%d') = aReleaseTag)    of TRUE: EXIT; end; // we're running the latest release
  case (aReleaseTag <> EMPTY) AND (fileExists(updateFile(aReleaseTag)))                    of TRUE: EXIT; end; // we've already downloaded the release file

  result := downloadAsset('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/download/' + aReleaseTag + '/MinimalistMediaPlayer_' + mmpCleanTag(aReleaseTag) + '.full.zip', updateFile(aReleaseTag), aReleaseTag);
end;

function TProgramUpdates.extractRelease(const aReleaseTag: string): boolean;
  function backupName: string;
  begin
    result := 'MinimalistMediaPlayer ' + mmpFileVersionFmt(EMPTY, 'v%d_%d_%d');
  end;
begin
  result := FALSE;
  case  aReleaseTag =  EMPTY                                                            of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> EMPTY) AND (mmpFileVersionFmt(EMPTY, 'v%d.%d.%d') = aReleaseTag) of TRUE: EXIT; end; // we're running the latest release

  case fileExists(mmpExePath + backupName + '.exe')    of FALSE: mmpRenameFile(paramStr(0), backupName); end;
  case fileExists(paramStr(0))                         of FALSE: with TZipFile.create do begin
                                                                    OnProgress := zipOnProgress;
                                                                    open(updateFile(aReleaseTag), zmRead);
                                                                    try
                                                                      extract('MinimalistMediaPlayer.exe', mmpExePath, TRUE);
                                                                      for var i := 0 to fileCount - 1 do case startsText('releaseNotes/', fileName[i]) of TRUE: extract(i, mmpExePath, TRUE); end; // accompanying images
                                                                    finally
                                                                      free;
                                                                    end;
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
      loadFromFile(mmpReleaseNotesFilePath(aTag)); // you need to create this file manually
      result := text;
      free;
    end;
  end;

//  json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/tags/v2.0.0'); // for DEV only

begin
  result := EMPTY;
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

function TProgramUpdates.getReleaseTag: string;
begin
  result := FReleaseTag;
  case result <> EMPTY of TRUE: EXIT; end;

  result := '(autoUpdate=no)';
  case CF.asBoolean[CONF_AUTO_UPDATE] of FALSE: EXIT; end;

  result := '(not available)';
  FReleaseTag := getJSONReleaseTag;
  case FReleaseTag = EMPTY of TRUE: EXIT; end;

  saveReleaseNotes(FReleaseTag);
  analyseReleaseNotes(FReleaseTag);

  result := downloadRelease(FReleaseTag); // if there's an error, report it back to the About Box via the result

  case (result = FReleaseTag) and fileExists(updateFile(FReleaseTag)) of TRUE: case extractRelease(FReleaseTag) of TRUE: result := result + ' Restart_Required'; end;end;
end;

function TProgramUpdates.hasReleaseNotes(const aReleaseTag: string): boolean;
begin
  result := fileExists(mmpReleaseNotesFilePath(aReleaseTag));
end;

function TProgramUpdates.saveReleaseNotes(const aReleaseTag: string): boolean;
begin
  case FReleaseNotes = EMPTY of TRUE: EXIT; end;

  with TStringList.create do begin
    text := FReleaseNotes;
    saveToFile(mmpReleaseNotesFilePath(aReleaseTag));
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
