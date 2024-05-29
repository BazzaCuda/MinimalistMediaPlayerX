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
unit mmpWindowCtrls;

interface
uses
  winApi.windows,
  system.classes;

function mmpCentreWindow(const aWnd: HWND): boolean;
function mmpFocusWindow(const aWnd: HWND): boolean;
function mmpGreaterWindow(const aWnd: HWND; const shift: TShiftState): boolean;

implementation

uses
  mmpDesktopUtils,
  mmpUtils, // EXPERIMENTAL
  TGlobalVarsClass;

function mmpFocusWindow(const aWnd: HWND): boolean;
begin
  SetForegroundWindow(aWnd);
end;

function mmpCentreWindow(const aWnd: HWND): boolean;
var
  vR: TRect;
  vHPos: integer;
  vVPos: integer;

  function alreadyCentred: boolean;
  begin
    vHPos := (mmpScreenWidth  - vR.width) div 2;
    vVPos := (mmpScreenHeight - vR.height) div 2;
    result := (vR.left = vHPos) and (vR.top = vVPos);
  end;

begin
  getWindowRect(aWnd, vR);

  case alreadyCentred of TRUE: EXIT; end;

  case (vHPos > 0) and (vVPos > 0) of TRUE: setWindowPos(aWnd, HWND_TOP, vHPos, vVPos, 0, 0, SWP_NOSIZE); end;
  GV.autoCentre := TRUE;
end;

function mmpGreaterWindow(const aWnd: HWND; const shift: TShiftState): boolean;
const
  dx = 170; // this will need to be changed when the user can alter the size of the thumbnails
  dy = 170;
var
  newW: integer;
  newH: integer;
  vR:   TRect;

  function calcDimensions: boolean;
  begin
    case ssCtrl in shift of
      TRUE: begin
              newW := newW - dx;
              newH := newH - dy;
            end;
     FALSE: begin
              newW := newW + dx;
              newH := newH + dy;
            end;
    end;
  end;

begin
  getWindowRect(aWnd, vR);
  newW := vR.Width;
  newH := vR.height;

  calcDimensions; // do what the user requested

  SetWindowPos(aWnd, HWND_TOP, 0, 0, newW, newH, SWP_NOMOVE); // resize the window
end;

end.
