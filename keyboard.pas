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
unit keyboard;

interface

uses
  system.classes;

type
  TKeyOp = (koNone, koCloseApp, koVolUp, koVolDown);
  TKeyDirection = (kdDown, kdUp);

  TKeyboard = class(TObject)
  strict private
    FKey:         word;
    FShiftState:  TShiftState;
    FUpDn:        TKeyDirection;
  private
    function keyIs(aChar: char): boolean; overload;
    function keyIs(aKeyCode: word): boolean; overload;
    function getKeyOp: TKeyOp;
    function getKeyUp: boolean;
    function getKeyDn: boolean;
    function getCtrl: boolean;
    function getShift: boolean;
    function getAlt: boolean;
  public
    function processKeyStroke(var aKey: word; aShiftState: TShiftState; upDn: TKeyDirection): boolean;

    property alt:         boolean     read getAlt;
    property ctrl:        boolean     read getCtrl;
    property key:         word        read FKey         write FKey;
    property shift:       boolean     read getShift;
    property shiftState:  TShiftState read FShiftState  write FShiftState;
    property keyDn:       boolean     read getKeyDn;
    property keyUp:       boolean     read getKeyUp;
  end;

function KB: TKeyboard;

implementation

uses
  sysCommands, globalVars, winApi.windows, mediaPlayer, _debugWindow;

const
A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';

var
  gKB: TKeyboard;

function KB: TKeyboard;
begin
  case gKB = NIL of TRUE: gKB := TKeyboard.create; end;
  result := gKB;
end;

{ TKeyboard }

function TKeyboard.getKeyOp: TKeyOp;
begin
  result := koNone;
  case keyUp and keyIs(X) of TRUE: result := koCloseApp; end;
  case keyDn and keyIs(VK_DOWN) of TRUE: result := koVolDown; end;
  case keyDn and keyIs(VK_UP) of TRUE: result := koVolUp; end;
end;

function TKeyboard.getAlt: boolean;
begin
  result := ssAlt in shiftState;
end;

function TKeyboard.getCtrl: boolean;
begin
  result := ssCtrl in shiftState;
end;

function TKeyboard.getKeyDn: boolean;
begin
  result := FUpDn = kdDown;
end;

function TKeyboard.getKeyUp: boolean;
begin
  result := FUpDn = kdUp;
end;

function TKeyboard.getShift: boolean;
begin
  result := ssShift in shiftState;
end;

function TKeyboard.keyIs(aChar: char): boolean;
begin
  result := key = ord(aChar);
end;

function TKeyboard.keyIs(aKeyCode: word): boolean;
begin
  result := key = aKeyCode;
end;

function TKeyboard.processKeyStroke(var aKey: word; aShiftState: TShiftState; upDn: TKeyDirection): boolean;
begin
  result      := FALSE;

  FKey        := aKey;
  FShiftState := aShiftState;
  FUpDn       := upDn;

  case getKeyOp of
    koNone: EXIT; // key not processed. bypass setting result to TRUE
    koCloseApp: sendSysCommandClose(GV.mainWnd);
    koVolUp:    MP.volUp;
    koVolDown:  MP.volDown;
  end;

  result := TRUE;
end;

initialization
  gKB := NIL;

finalization
  case gKB <> NIL of TRUE: gKB.free; end;

end.
