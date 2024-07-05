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
unit viewModel.mmpSnapshot;

interface

uses
  system.Classes,
  mmpConsts;

type
  ISnapshot = interface
    ['{227B7FBF-F98F-4400-BF47-8A86BECDAE70}']
    function  getHandled:          boolean;
    function  getKey:              WORD;
    function  getShiftState:       TShiftState;
    function  getKeyDirection:     TKeyDirection;
    function  getKeyOp:            TKeyOp;
    function  getScreenshotFolder: string;
    procedure setHandled(const aValue: boolean);
    procedure setKey(const aValue: WORD);
    procedure setkeyDirection(const aValue: TKeyDirection);
    procedure setKeyOp(const aValue: TKeyOp);
    procedure setShiftState(const aValue: TShiftState);
    property handled:             boolean       read getHandled         write setHandled;
    property key:                 WORD          read getKey             write setKey;
    property keyOp:               TKeyOp        read getKeyOp           write setKeyOp;
    property shiftState:          TShiftState   read getShiftState      write setShiftState;
    property keyDirection:        TKeyDirection read getKeyDirection    write setKeyDirection;
    property screenshotFolder:    string        read getScreenshotFolder;
  end;

function newSnapshot(const aKey: WORD; aShiftState: TShiftState; aKeyDirection: TKeyDirection; aScreenshotFolder: string): ISnapshot;

implementation

type
  TSnapshot = class(TInterfacedObject, ISnapshot)
  strict private
    FHandled:           boolean;
    FKey:               WORD;
    FShiftState:        TShiftState;
    FKeyDirection:      TKeyDirection;
    FKeyOp:             TKeyOp;
    FScreenshotFolder:  string;
  protected
    function  getHandled:          boolean;
    function  getKey:              WORD;
    function  getKeyDirection:     TKeyDirection;
    function  getKeyOp:            TKeyOp;
    function  getScreenshotFolder: string;
    function  getShiftState:       TShiftState;
    procedure setHandled(const aValue: boolean);
    procedure setKey(const aValue: WORD);
    procedure setkeyDirection(const aValue: TKeyDirection);
    procedure setKeyOp(const aValue: TKeyOp);
    procedure setShiftState(const aValue: TShiftState);
  public
    constructor create(const aKey: WORD; aShiftState: TShiftState; aKeyDirection: TKeyDirection; aScreenshotFolder: string);
  end;

function newSnapshot(const aKey: WORD; aShiftState: TShiftState; aKeyDirection: TKeyDirection; aScreenshotFolder: string): ISnapshot;
begin
  result := TSnapshot.create(aKey, aShiftState, aKeyDirection, aScreenshotFolder);
end;

{ TSnapshot }

constructor TSnapshot.create(const aKey: WORD; aShiftState: TShiftState; aKeyDirection: TKeyDirection; aScreenshotFolder: string);
begin
  inherited create;
  FKey                := aKey;
  FShiftState         := aShiftState;
  FKeyDirection       := aKeyDirection;
  FScreenshotFolder   := aScreenshotFolder;
end;

function TSnapshot.getHandled: boolean;
begin
  result := FHandled;
end;

function TSnapshot.getKey: WORD;
begin
  result := FKey;
end;

function TSnapshot.getKeyDirection: TKeyDirection;
begin
  result := FKeyDirection;
end;

function TSnapshot.getKeyOp: TKeyOp;
begin
  result := FKeyOp;
end;

function TSnapshot.getScreenshotFolder: string;
begin
  result := FScreenshotFolder;
end;

function TSnapshot.getShiftState: TShiftState;
begin
  result := FShiftState;
end;

procedure TSnapshot.setHandled(const aValue: boolean);
begin
  FHandled := aValue;
end;

procedure TSnapshot.setKey(const aValue: WORD);
begin
  FKey := aValue;
end;

procedure TSnapshot.setkeyDirection(const aValue: TKeyDirection);
begin
  FKeyDirection := aValue;
end;

procedure TSnapshot.setKeyOp(const aValue: TKeyOp);
begin
  FKeyOp := aValue;
end;

procedure TSnapshot.setShiftState(const aValue: TShiftState);
begin
  FShiftState := aValue;
end;

end.

