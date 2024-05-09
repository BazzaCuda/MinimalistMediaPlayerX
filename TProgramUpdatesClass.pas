{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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

function currentReleaseVersion: string;

implementation

uses
  system.json, system.classes, system.sysUtils, IdHTTP, idSSLOpenSSL, commonUtils, _debugWindow;

var
  gLatestVersion: string = '';

function fetchURL(const aURL: string): string;
var
  http:       TIdHTTP;
  sslHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  http := TIdHTTP.Create(nil);
  http.Request.ContentEncoding := 'UTF-8';

  sslHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  sslHandler.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];

  http.IOHandler := sslHandler;

  try
    result := http.get(aUrl);
  finally
    sslHandler.Free;
    http.Free;
  end;
end;

function currentReleaseVersion: string;
var
  json: string;
  obj: TJSONObject;
  url: string;
begin
  case gLatestVersion = '' of TRUE: begin
                                      json := fetchURL('https://api.github.com/repos/bazzacuda/minimalistmediaplayerx/releases/latest');
                                      try
                                        obj := TJSONObject.ParseJSONValue(json) as TJSONObject;
                                        case obj = NIL of  FALSE: gLatestVersion := obj.Values['tag_name'].value; end;
                                      finally
                                        obj.Free;
                                      end;end;end;

  result := gLatestVersion;
end;



end.
