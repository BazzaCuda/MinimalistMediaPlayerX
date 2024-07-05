{   MMP: Minimalist Media Player
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
unit model.mmpParamStrings;

interface

type
  IParamStrings = interface
    ['{0F0C2795-ED4D-4FE1-917F-5BB980FC9481}']
    function getFileFolder:         string;
    function getFileFolderAndName:  string;
    function getNoFile:             boolean;
    property fileFolder:            string      read getFileFolder;
    property fileFolderAndName:     string      read getFileFolderAndName;
    property noFile:                boolean     read getNoFile;
  end;

  TParamStrings = class(TInterfacedObject, IParamStrings)
  private
    function getFileFolder:         string;
    function getFileName:           string;
    function getNoFile:             boolean;
    function getFileFolderAndName:  string;
  public
    property fileFolder:            string      read getFileFolder;
    property fileFolderAndName:     string      read getFileFolderAndName;
    property fileName:              string      read getFileName;
  end;

function PS: IParamStrings;

implementation

uses
  system.sysUtils;

var
  gPS: IParamStrings;
function PS: IParamStrings;
begin
  case gPS = NIL of TRUE: gPS := TParamStrings.create; end;
  result := gPS;
end;

{ TPS }

function TParamStrings.getFileFolder: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFilePath(paramStr(1)); end;
end;

function TParamStrings.getFileFolderAndName: string;
begin
  result := fileFolder + fileName;
end;

function TParamStrings.getFileName: string;
begin
  case paramCount = 0 of  TRUE: result := '';
                         FALSE: result := extractFileName(paramStr(1)); end;
end;

function TParamStrings.getNoFile: boolean;
begin
  result := trim(fileFolderAndName) = '';
end;

initialization
  gPS := NIL;

end.
