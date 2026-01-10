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
unit model.mmpParamStrings;

interface

type
  IParamStrings = interface
    function getFileFolder:         string;
    function getFileFolderAndName:  string;
    function getNoFile:             boolean;
    function getNoPlaylist:         boolean;
    property fileFolder:            string      read getFileFolder;
    property fileFolderAndName:     string      read getFileFolderAndName;
    property noFile:                boolean     read getNoFile;
    property noPlaylist:            boolean     read getNoPlaylist;
  end;

  TParamStrings = class(TInterfacedObject, IParamStrings)
  private
    function getFileFolder:         string;
    function getFileName:           string;
    function getNoFile:             boolean;
    function getNoPlaylist:         boolean;
    function getFileFolderAndName:  string;
  public
    property fileFolder:            string      read getFileFolder;
    property fileFolderAndName:     string      read getFileFolderAndName;
    property fileName:              string      read getFileName;
  end;

function PS: IParamStrings;

implementation

uses
  system.sysUtils,
  mmpConsts;

function PS: IParamStrings;
{$J+} const gPS: IParamStrings = NIL; {$J-}
begin
  case gPS = NIL of TRUE: gPS := TParamStrings.create; end;
  result := gPS;
end;

{ TPS }

function TParamStrings.getFileFolder: string;
begin
  result := extractFilePath(paramStr(1));  // paramStr returns '' if index is out of range
end;

function TParamStrings.getFileFolderAndName: string;
begin
  result := fileFolder + fileName;
end;

function TParamStrings.getFileName: string;
begin
  result := extractFileName(paramStr(1)); // paramStr returns '' if index is out of range
end;

function TParamStrings.getNoFile: boolean;
begin
  result := (trim(fileFolderAndName) = EMPTY) or NOT (fileExists(trim(fileFolderAndName)));
end;

function TParamStrings.getNoPlaylist: boolean;
begin
  result := lowerCase(trim(paramStr(2))) = CMDLINE_OPTION_NOPLAYLIST;
end;

end.
