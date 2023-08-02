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
unit configFile;

interface

uses
  system.classes;

type
  TConfigFile = class(TObject)
  strict private
    FFileContents: TStringList;
    FFilePath: string;
    function saveConfigFile: boolean;
  private
    function getValue(const aName: string): string;
    procedure setValue(const aName, aValue: string);
    function getAsInteger(const aName: string): integer;
  public
    constructor create;
    destructor destroy; override;
    function deleteName(aName: string): boolean;
    function initConfigFile(const aFilePath: string): boolean;
    property asInteger[const aName: string]: integer read getAsInteger;
    property value[const aName: string]: string read getValue write setValue;
  end;

function CF: TConfigFile;

implementation

uses
  system.sysUtils;

var
  gCF: TConfigFile;

function CF: TConfigFile;
begin
  case gCF = NIL of TRUE: gCF := TConfigFile.create; end;
  result := gCF;
end;

{ TConfigFile }

constructor TConfigFile.create;
begin
  inherited;
  FFileContents := TStringList.create;
end;

function TConfigFile.deleteName(aName: string): boolean;
begin
  FFileContents.delete(FFileContents.indexOfName(aName));
  saveConfigFile;
end;

destructor TConfigFile.destroy;
begin
  case FFileContents <> NIL of TRUE: FFileContents.free; end;
  inherited;
end;

function TConfigFile.getAsInteger(const aName: string): integer;
begin
  try result := strToIntDef(FFileContents.values[aName], 0); except result := 0; end;
end;

function TConfigFile.getValue(const aName: string): string;
begin
  result := FFileContents.values[aName];
end;

function TConfigFile.initConfigFile(const aFilePath: string): boolean;
begin
  FFilePath := aFilePath;
  case fileExists(FFilePath) of TRUE: FFileContents.loadFromFile(FFilePath); end;
end;

function TConfigFile.saveConfigFile: boolean;
begin
  FFileContents.saveToFile(FFilePath);
end;

procedure TConfigFile.setValue(const aName, aValue: string);
begin
  FFileContents.values[aName] := aValue;
  saveConfigFile;
end;

initialization
  gCF := NIL;

finalization
  case gCF <> NIL of TRUE: gCF.free; end;

end.
