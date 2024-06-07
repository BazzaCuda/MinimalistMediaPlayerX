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
unit TConfigFileClass;

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
    procedure setValue(const aName: string; const aValue: string);
    function getAsBoolean(const aName: string): boolean;
    function getAsInteger(const aName: string): integer;
  public
    constructor create;
    destructor destroy; override;
    function deleteName(const aName: string): boolean;
    function initConfigFile(const aFilePath: string): boolean;
    function toHex(const aInteger: integer): string;
    property asBoolean[const aName: string]: boolean read getAsBoolean;
    property asInteger[const aName: string]: integer read getAsInteger;
    property value[const aName: string]: string read getValue write setValue;
  end;

function CF: TConfigFile;

implementation

uses
  system.sysUtils,
  _debugWindow;

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
  FFileContents                 := TStringList.create;
  FFileContents.DefaultEncoding := TEncoding.UTF8;
  FFileContents.CaseSensitive   := FALSE;
end;

function TConfigFile.deleteName(const aName: string): boolean;
begin
  var vIx := FFileContents.indexOfName(aName);
  case vIx <> -1 of TRUE: begin
                            FFileContents.delete(vIx);
                            saveConfigFile; end;end;
end;

destructor TConfigFile.destroy;
begin
  case FFileContents <> NIL of TRUE: FFileContents.free; end;
  inherited;
end;

function TConfigFile.getAsBoolean(const aName: string): boolean;
begin
  result := lowerCase(FFileContents.values[aName]) = 'yes';
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
  try
    FFileContents.saveToFile(FFilePath);
  except // trap rapid-fire write errors, e.g. when user holds down ctrl-B
  end;
end;

procedure TConfigFile.setValue(const aName: string; const aValue: string);
begin
  FFileContents.values[aName] := aValue;
  saveConfigFile;
end;

function TConfigFile.toHex(const aInteger: integer): string;
begin
  result := '$' + intToHex(aInteger);
end;

initialization
  gCF := NIL;

finalization
  case gCF <> NIL of TRUE: gCF.free; end;

end.
