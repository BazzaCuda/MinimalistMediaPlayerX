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
  TKeyOp = (koNone, koCloseApp);
  TKeyDirection = (kdDown, kdUp);

  TKeyboard = class(TObject)
  strict private
    FUpDn: TKeyDirection;
  private
    function getKeyOp(var Key: word; shiftState: TShiftState): TKeyOp;
    function getKeyUp: boolean;
    function getKeyDn: boolean;
  public
    function processKeyStroke(var Key: word; shiftState: TShiftState; upDn: TKeyDirection): boolean;
    property keyDn: boolean read getKeyDn;
    property keyUp: boolean read getKeyUp;
  end;

function KB: TKeyboard;

implementation

uses
  sysCommands, globalVars, _debugWindow;

var
  gKB: TKeyboard;

function KB: TKeyboard;
begin
  case gKB = NIL of TRUE: gKB := TKeyboard.create; end;
  result := gKB;
end;

{ TKeyboard }

function TKeyboard.getKeyOp(var key: word; shiftState: TShiftState): TKeyOp;
begin
  result := koNone;
  case keyUp and (key = ord('X')) of TRUE: result := koCloseApp; end;
end;

function TKeyboard.getKeyDn: boolean;
begin
  result := FUpDn = kdDown;
end;

function TKeyboard.getKeyUp: boolean;
begin
  result := FUpDn = kdUp;
end;

function TKeyboard.processKeyStroke(var key: word; shiftState: TShiftState; upDn: TKeyDirection): boolean;
begin
  FUpDn := upDn;
  case getKeyOp(key, shiftState) of
    koNone: EXIT;
    koCloseApp: sendSysCommandClose(GV.mainWnd);
  end;
  key := 0;
end;

initialization
  gKB := NIL;

finalization
  case gKB <> NIL of TRUE: gKB.free; end;

end.
