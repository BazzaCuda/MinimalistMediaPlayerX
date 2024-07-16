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
unit mmpKeyboardUtils;

interface

uses
  winApi.windows,
  system.classes;

function mmpCapsLockOn:     boolean;       
function mmpCtrlKeyDown:    boolean;       
function mmpKBState:        TKeyboardState;
function mmpNumLockOn:      boolean;       
function mmpShiftState:     TShiftState;    // so we don't have to pull vcl.forms into every unit that needs this
function mmpToggleNumlock:  boolean;       

implementation

uses
  vcl.forms;

function mmpCapsLockOn: boolean;
begin
  result := getKeyState(VK_CAPITAL) <> 0;
end;

function mmpCtrlKeyDown: boolean;
begin
  result := getKeyState(VK_CONTROL) < 0;
end;

function mmpKBState: TKeyboardState;
begin
  getKeyboardState(result);
end;

function mmpNumLockOn: boolean;
begin
  result := getKeyState(VK_NUMLOCK) <> 0;
end;

function mmpShiftState: TShiftState; // so we don't have to pull vcl.forms into every unit that needs this
var
  vKeyboardState: TKeyBoardState;
begin
  getKeyboardState(vKeyboardState);
  result := keyboardStateToShiftState(vKeyboardState);

  // I don't know why this is necessary but it is.
  // Ctrl-[G] on multiple windows proves it.
  // The "other" windows don't know the Ctrl key is down without this.
  // In other words, getKeyboardState only registers it in the window in which is was pressed.
  // This isn't the case with the Shift keys.
  case getKeyState(VK_CONTROL) < 0 of TRUE: include(result, ssCtrl); end;
end;

function mmpToggleNumlock: boolean;
begin
  result      := FALSE;
  var kbState := mmpKBState;
  keybd_event(VK_NUMLOCK, kbState[VK_NUMLOCK] xor 1, KEYEVENTF_EXTENDEDKEY or 0, 0);
  keybd_event(VK_NUMLOCK, kbState[VK_NUMLOCK] xor 1, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  result      := TRUE;
end;

end.
