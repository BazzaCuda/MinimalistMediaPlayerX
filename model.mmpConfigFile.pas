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
unit model.mmpConfigFile;

interface

uses
  system.classes, system.ioUtils,
  mmpConsts;

type
  IConfigFile = interface
    ['{32AB825C-CC48-47F2-8717-8F471C059BAD}']
    function  getAsBoolean      (const aName: string): boolean;
    procedure setAsBoolean      (const aName: string; const aValue: boolean);
    function  getAsDeleteMethod (const aName: string): TDeleteMethod;
    procedure setAsDeleteMethod (const aName: string; const aDeleteMethod: TDeleteMethod);
    function  getAsInteger      (const aName: string): integer;
    procedure setAsInteger      (const aName: string; const aInteger: integer);
    function  getAsMediaType    (const aName: string): TMediaType;
    procedure setAsMediaType    (const aName: string; const aMediaType: TMediaType);
    function  getValue          (const aName: string): string;

    procedure setValue          (const aName: string; const aValue: string);

    function  deleteConfig      (const aName: string):      boolean;
    function  initConfigFile    (const aFilePath: string):  boolean;
    function  toHex             (const aInteger: integer):  string;

    property  asBoolean         [const aName: string]:  boolean       read getAsBoolean       write setAsBoolean;
    property  asDeleteMethod    [const aName: string]:  TDeleteMethod read getAsDeleteMethod  write setAsDeleteMethod;
    property  asInteger         [const aName: string]:  integer       read getAsInteger       write setAsInteger;
    property  asMediaType       [const aName: string]:  TMediaType    read getAsMediaType     write setAsMediaType;
    property  value             [const aName: string]:  string        read getValue           write setValue; default;
  end;

function CF: IConfigFile;

implementation

uses
  system.sysUtils,
  mmpFuncProg,
  _debugWindow;

type
  TConfigFile = class(TInterfacedObject, IConfigFile)
  strict private
    FFileContents:  TStringList;
    FFilePath:      string;
    FLastWriteTime: TDateTime;
  private
    function    checkForManualEdits: boolean;
    function    saveConfigFile: boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    function    getAsBoolean      (const aName: string): boolean;
    procedure   setAsBoolean      (const aName: string; const aValue: boolean);
    function    getAsDeleteMethod (const aName: string): TDeleteMethod;
    procedure   setAsDeleteMethod (const aName: string; const aDeleteMethod: TDeleteMethod);
    function    getAsInteger      (const aName: string): integer;
    procedure   setAsInteger      (const aName: string; const aInteger: integer);
    function    getAsMediaType    (const aName: string): TMediaType;
    procedure   setAsMediaType    (const aName: string; const aMediaType: TMediaType);
    function    getValue          (const aName: string): string;

    procedure   setValue          (const aName: string; const aValue: string);

    function    deleteConfig      (const aName: string): boolean;
    function    initConfigFile    (const aFilePath: string): boolean;
    function    toHex             (const aInteger: integer): string;
  end;

function CF: IConfigFile;
{$J+} const gCF: IConfigFile = NIL; {$J-}
begin
  case gCF = NIL of TRUE: gCF := TConfigFile.create; end;
  result := gCF;
end;

{ TConfigFile }

constructor TConfigFile.Create;
begin
  inherited;
  FFileContents                 := TStringList.create;
  FFileContents.DefaultEncoding := TEncoding.UTF8;
  FFileContents.CaseSensitive   := FALSE;
end;

function TConfigFile.checkForManualEdits: boolean;
begin
  result := FALSE;
  case fileExists(FFilePath) of FALSE: EXIT; end;

  var vLastWriteTime := TFile.getLastWriteTime(FFilePath);

  mmp.cmd(vLastWriteTime > FLastWriteTime, procedure begin try FFileContents.loadFromFile(FFilePath); except end;end);
  FLastWriteTime := vLastWriteTime;

  result := TRUE;
end;

function TConfigFile.deleteConfig(const aName: string): boolean;
begin
  result := FALSE;
  checkForManualEdits;
  var vIx := FFileContents.indexOfName(aName);
  case vIx <> -1 of TRUE: begin
                            FFileContents.delete(vIx);
                            saveConfigFile; end;end;
  result := TRUE;
end;

destructor TConfigFile.Destroy;
begin
  mmp.free(FFileContents <> NIL, FFileContents);
  inherited;
end;

function TConfigFile.getAsBoolean(const aName: string): boolean;
begin
  result := FALSE;
  checkForManualEdits;
  result := lowerCase(FFileContents.values[aName]) = 'yes';
end;

function TConfigFile.getAsDeleteMethod(const aName: string): TDeleteMethod;
begin
  checkForManualEdits;
  var vDeleteMethod := lowercase(FFileContents.values[aName]);
  result := dmRecycle;
  case vDeleteMethod = 'recycle'  of TRUE: result := dmRecycle;   end;
  case vDeleteMethod = 'delete'   of TRUE: result := dmStandard;  end;
  case vDeleteMethod = 'shred'    of TRUE: result := dmShred;     end;
end;

function TConfigFile.getAsInteger(const aName: string): integer;
begin
  checkForManualEdits;
  try result := strToIntDef(FFileContents.values[aName], 0); except result := 0; end;
end;

function TConfigFile.getAsMediaType(const aName: string): TMediaType;
begin
  checkForManualEdits;
  var vPlaylistFormat := lowercase(FFileContents.values[aName]);
  result := mtUnk;
  case vPlaylistFormat = 'audio'      of TRUE: result := mtAudio;       end;
  case vPlaylistFormat = 'video'      of TRUE: result := mtVideo;       end;
  case vPlaylistFormat = 'image'      of TRUE: result := mtImage;       end;
  case vPlaylistFormat = 'audiovideo' of TRUE: result := mtAudioVideo;  end;
end;

function TConfigFile.getValue(const aName: string): string;
begin
  checkForManualEdits;
  result := FFileContents.values[aName]; // don't trim!
end;

function TConfigFile.initConfigFile(const aFilePath: string): boolean;
begin
  result    := FALSE;
  FFilePath := aFilePath;
  result    := TRUE;
end;

function TConfigFile.saveConfigFile: boolean;
begin
  result := FALSE;
  checkForManualEdits;
  try
    FFileContents.saveToFile(FFilePath);
  except // trap rapid-fire write errors, e.g. when user holds down ctrl-B
  end;
  result := TRUE;
end;

procedure TConfigFile.setAsBoolean(const aName: string; const aValue: boolean);
begin
  case aValue of   TRUE: setValue(aName, 'yes');
                  FALSE: setValue(aName, 'no'); end;
end;

procedure TConfigFile.setAsDeleteMethod(const aName: string; const aDeleteMethod: TDeleteMethod);
begin
  case aDeleteMethod of
    dmRecycle:  setValue(aName, 'recycle');
    dmStandard: setValue(aName, 'delete');
    dmShred:    setValue(aName, 'shred');
  end;
end;

procedure TConfigFile.setAsInteger(const aName: string; const aInteger: integer);
begin
  setValue(aName, intToStr(aInteger));
end;

procedure TConfigFile.setAsMediaType(const aName: string; const aMediaType: TMediaType);
begin
  case aMediaType of
    mtUnk:        setValue(aName, 'all');
    mtAudio:      setValue(aName, 'audio');
    mtVideo:      setValue(aName, 'video');
    mtImage:      setValue(aName, 'image');
    mtAudioVideo: setValue(aName, 'audiovideo');
  end;
end;

procedure TConfigFile.setValue(const aName: string; const aValue: string);
begin
  checkForManualEdits;
  FFileContents.values[aName] := aValue;
  saveConfigFile;
end;

function TConfigFile.toHex(const aInteger: integer): string;
begin
  result := '$' + intToHex(aInteger);
end;

end.
