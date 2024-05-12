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

function getLatestVersion: string;

implementation

uses
  idHTTP, idSSLOpenSSL, idComponent,
  system.json, system.classes, system.sysUtils, system.strUtils,
  vcl.forms,
  consts, formDownload,
  TConfigFileClass, TCommonUtilsClass, TProgressBarClass, _debugWindow;

type
  TWorkProgress = class(TObject)  // only because IdHttp requires these callbacks to be procedure of object
    procedure idHTTPWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: int64);
    procedure idHTTPWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: int64);
    procedure idHTTPEnd(ASender: TObject; AWorkMode: TWorkMode);
  end;

var
  gLatestVersion: string = '';
  gWP: TWorkProgress;
  gProgressBar: TProgressBar;
  gDownloadForm: TDownloadForm;

function fetchURL(const aURL: string; aFileStream: TStream = NIL): string;
var
  http:       TidHTTP;
  sslHandler: TidSSLIOHandlerSocketOpenSSL;
begin
  result        := '';
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
      on e:exception do debug(e.Message);
    end;
  finally
    sslHandler.free;
    http.free;
  end;
end;

function getLatestVersion: string;
var
  json: string;
  obj:  TJSONObject;
  jp:   TJSONPair;

  function cleanTag: string;
  begin
    result := replaceStr(gLatestVersion, '.', '_');
  end;

  function updateFile: string;
  begin
    result := CU.getExePath + 'update_' + cleanTag + '.zip';
  end;
begin
  result := '(autoUpdate=no)';
  case lowerCase(CF.value['autoUpdate']) = 'yes' of FALSE: EXIT; end;

try

  case gLatestVersion = '' of TRUE: begin
                                      json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/latest');
                                      try
                                        obj := TJSONObject.ParseJSONValue(json) as TJSONObject;
                                        try
                                          case obj = NIL of FALSE: gLatestVersion := obj.values['tag_name'].value; end;

{ this works but we don't need to do it because we can determine the url of each release file using the tag }
//                                          case obj = NIL of FALSE:  begin
//                                                                      jp := obj.get('assets');
//                                                                      case jp = NIL of FALSE: for var element in TJSONArray(jp.JsonValue) do
//                                                                                                case element is TJSONObject of TRUE: debug(TJSONObject(element).values['browser_download_url'].value); end;end;end;end;

                                          case  gLatestVersion = ''                                                                of TRUE: EXIT; end; // couldn't obtain latest release tag
                                          case (gLatestVersion <> '') AND (CU.getFileVersionFmt('', 'v%d.%d.%d') = gLatestVersion) of TRUE: EXIT; end; // we're running the latest release
                                          case (gLatestVersion <> '') AND (fileExists(updateFile))                                 of TRUE: EXIT; end; // we've already downloaded the release file

                                          var fs := TFileStream.create(updateFile, fmCreate);
                                          try
                                            fetchURL('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/download/' + gLatestVersion + '/MinimalistMediaPlayer_' + cleanTag + '.full.zip', fs);
                                          finally
                                            fs.free;
                                          end;

                                        except
                                        end;
                                      finally
                                        case obj = NIL of FALSE: obj.free; end;
                                      end;end;end;
finally
  result := gLatestVersion;
end;
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

initialization
  gLatestVersion := '';
  gWP            := NIL;
  gProgressBar   := NIL;
  gDownloadForm  := NIL;

end.
