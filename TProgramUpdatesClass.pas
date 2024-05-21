{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit TProgramUpdatesClass;

interface

uses
  system.zip;

type
  TProgramUpdates = class(TObject)
  strict private
    FReleaseNotes: string;
    FReleaseTag: string;
  private
    function analyseReleaseNotes(const aReleaseTag: string): boolean;
    function downloadAsset(const aURL, aFileName: string): string;
    function downloadRelease(const aReleaseTag: string): string;
    function extractRelease(const aReleaseTag: string): boolean;
    function getJSONReleaseTag: string;
    function getReleaseNotesFolder: string;
    function getReleaseNotesFilePath(const aReleaseTag: string): string;
    function getReleaseTag: string;
    function saveReleaseNotes(const aReleaseTag: string): boolean;
    function getHasReleaseNotes: boolean;
  protected
    procedure zipOnProgress(sender: TObject; aFileName: string; aHeader: TZipHeader; aPosition: Int64);
  public
    property hasReleaseNotes: boolean read getHasReleaseNotes;
    property releaseNotes: string read FReleaseNotes;
    property releaseTag:   string read getReleaseTag;
  end;

function PU: TProgramUpdates;

implementation

uses
  idHTTP, idSSLOpenSSL, idComponent,
  system.json, system.classes, system.sysUtils, system.strUtils,
  vcl.forms,
  mmpConsts, formDownload,
  TConfigFileClass, TCommonUtilsClass, TProgressBarClass, _debugWindow;

type
  TWorkProgress = class(TObject)  // only because IdHttp requires these callbacks to be procedure of object
    procedure idHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
    procedure idHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
    procedure idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

var
  gPU: TProgramUpdates;

  gWP: TWorkProgress;
  gProgressBar: TProgressBar;
  gDownloadForm: TDownloadForm;

function PU: TProgramUpdates;
begin
  case gPU = NIL of TRUE: gPU := TProgramUpdates.create; end;
  result := gPU;
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
                                          gProgressBar  := TProgressBar.create;
                                          gProgressBar.initProgressBar(gDownloadForm, PB_COLOR_DELTA);

                                          try
                                            gDownloadForm.show;
                                            http.get(aURL, aFileStream); // download the file
                                          finally
                                            case gWP           <> NIL of TRUE: freeAndNIL(gWP); end;
                                            case gProgressBar  <> NIL of TRUE: freeAndNIL(gProgressBar); end;
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
  result := CU.getExePath + 'update_' + cleanTag(aReleaseTag) + '.zip';
end;

{ TWorkProgress }

procedure TWorkProgress.idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
//
end;

procedure TWorkProgress.idHTTPWork(aSender: TObject; aWorkMode: TWorkMode; aWorkCount: int64);
begin
  gProgressBar.position := aWorkCount;
  gDownloadForm.byteLabel.caption := format('%n of %n', [gProgressBar.position * 1.0, gProgressBar.max * 1.0]);
  gDownloadForm.byteLabel.Refresh;
  application.processMessages;
end;

procedure TWorkProgress.idHTTPWorkBegin(aSender: TObject; aWorkMode: TWorkMode; aWorkCountMax: int64);
begin
  gProgressBar.max := aWorkCountMax;
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
      var vPos := pos('(https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/', vNotes[i]);
      case vPos = 0 of TRUE: CONTINUE; end;

      var vAssetURL := copy(vNotes[i], vPos + 1, 255);
      var vPos2 := pos(')', vAssetURL);
      case vPos2 = 0 of TRUE: CONTINUE; end;

      delete(vAssetURL, vPos2, 255);

      var vPos3 := LastDelimiter('/', vAssetURL);
      case vPos3 = 0 of TRUE: CONTINUE; end;

      var vFileName := copy(vAssetURL, vPos3 + 1, 255);

      downloadAsset(vAssetURL, vFileName);

      vNotes[i] := replaceStr(vNotes[i], vAssetURL, 'releaseNotes\' + vFileName);
    end;

    vNotes.saveToFile(getReleaseNotesFilePath(aReleaseTag));
  finally
    vNotes.free;
  end;
end;

function TProgramUpdates.downloadAsset(const aURL: string; const aFileName: string): string;
begin
  var vDestFile := getReleaseNotesFolder + aFileName;
  case fileExists(vDestFile) of TRUE: EXIT; end;

  var fs := TFileStream.create(vDestFile, fmCreate);
  try
    result := fetchURL(aURL, fs);
  finally
    fs.free;
  end;
end;

function TProgramUpdates.extractRelease(const aReleaseTag: string): boolean;
  function backupName: string;
  begin
    result := 'MinimalistMediaPlayer ' + CU.getFileVersionFmt('', 'v%d_%d_%d');
  end;
begin
  result := FALSE;
  case  aReleaseTag = ''                                                                of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> '') AND (CU.getFileVersionFmt('', 'v%d.%d.%d') = aReleaseTag)    of TRUE: EXIT; end; // we're running the latest release

  case fileExists(CU.getExePath + backupName + '.exe') of FALSE:  CU.renameFile(paramStr(0), backupName); end;
  case fileExists(paramStr(0))                         of FALSE:  with TZipFile.create do begin
                                                                    OnProgress := zipOnProgress;
                                                                    open(updateFile(aReleaseTag), zmRead);
//                                                                    extractAll(CU.getExePath);
                                                                    extract('MinimalistMediaPlayer.exe', CU.getExePath);
                                                                    free;
                                                                    result := fileExists(paramStr(0));
                                                                  end;end;
end;

function TProgramUpdates.getHasReleaseNotes: boolean;
begin
  result := FReleaseNotes <> '';
end;

function TProgramUpdates.getJSONReleaseTag: string;
var
  json: string;
  obj:  TJSONObject;
begin
  result := '';
  json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/latest');
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
end;

function TProgramUpdates.downloadRelease(const aReleaseTag: string): string;
begin
  result := aReleaseTag;

  case  aReleaseTag = ''                                                                of TRUE: EXIT; end; // couldn't obtain latest release tag
  case (aReleaseTag <> '') AND (CU.getFileVersionFmt('', 'v%d.%d.%d') = aReleaseTag)    of TRUE: EXIT; end; // we're running the latest release
  case (aReleaseTag <> '') AND (fileExists(updateFile(aReleaseTag)))                    of TRUE: EXIT; end; // we've already downloaded the release file

  var fs := TFileStream.create(updateFile(aReleaseTag), fmCreate);
  try
    result := fetchURL('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/download/' + aReleaseTag + '/MinimalistMediaPlayer_' + cleanTag(aReleaseTag) + '.full.zip', fs, aReleaseTag);
  finally
    fs.free;
  end;
end;

function TProgramUpdates.getReleaseNotesFilePath(const aReleaseTag: string): string;
begin
  result := format('%s%s%s%s', [getReleaseNotesFolder, 'releaseNotes ', cleanTag(aReleaseTag), '.md'])
end;

function TProgramUpdates.getReleaseNotesFolder: string;
begin
  result := CU.getExePath + 'releaseNotes\';
  forceDirectories(result);
end;

function TProgramUpdates.getReleaseTag: string;
begin
  result := FReleaseTag;
  case result <> '' of TRUE: EXIT; end;

  result := '(autoUpdate=no)';
  case lowerCase(CF.value['autoUpdate']) = 'yes' of FALSE: EXIT; end;

  FReleaseTag := getJSONReleaseTag;
  saveReleaseNotes(FReleaseTag);
  analyseReleaseNotes(FReleaseTag);

  result := downloadRelease(FReleaseTag); // if there's an error, report it back to the About Box via the result

  case (result = FReleaseTag) and fileExists(updateFile(FReleaseTag)) of TRUE: case extractRelease(FReleaseTag) of TRUE: result := result + ' Restart_Required'; end;end;
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
  gPU := NIL;
  gWP            := NIL;
  gProgressBar   := NIL;
  gDownloadForm  := NIL;

finalization
  case gPU <> NIL of TRUE: gPU.free; end;

end.
