{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit params;

interface

type
  TPS = class(TObject)
  private
    function getFileFolder: string;
    function getFileName: string;
    function getNoFile: boolean;
    function getFileFolderAndName: string;
  public
    property fileFolder: string read getFileFolder;
    property fileFolderAndName: string read getFileFolderAndName;
    property fileName: string read getFileName;
    property noFile: boolean read getNoFile;
  end;

function PS: TPS; // Param Strings

implementation

uses
  system.sysUtils;

var
  gPS : TPS;

function PS: TPS;
begin
  case gPS = NIL of TRUE: gPS := TPS.create; end;
  result := gPS;
end;

{ TPS }

function TPS.getFileFolder: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFilePath(paramStr(1)); end;
end;

function TPS.getFileFolderAndName: string;
begin
  result := fileFolder + fileName;
end;

function TPS.getFileName: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFileName(paramStr(1)); end;
end;

function TPS.getNoFile: boolean;
begin
  result := fileFolderAndName = '';
end;

initialization
  gPS := NIL;

finalization
  case gPS <> NIL of TRUE: gPS.free; end;

end.
