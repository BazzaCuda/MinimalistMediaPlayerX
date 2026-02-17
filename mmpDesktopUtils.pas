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
unit mmpDesktopUtils;

interface

uses
  winApi.windows;

function mmpBorderWidth: integer;
function mmpCaptionHeight: integer;
function mmpOffScreen(const aHWND: HWND): boolean;
function mmpScreenCentre: integer;
function mmpScreenHeight: integer;
function mmpScreenMinHeight: integer;
function mmpScreenMinWidth: integer;
function mmpScreenWidth: integer;
function mmpWithinScreenLimits(const aWidth: integer; const aHeight: integer): boolean;
function mmpWndWidthHeight(const aWnd: HWND; var aWidth: integer; var aHeight: integer): boolean;

implementation

uses
  vcl.forms,
  mmpGlobalState,
  _debugWindow;

function mmpBorderWidth: integer;
begin
  result := getSystemMetrics(SM_CXSIZEFRAME);
end;

function mmpCaptionHeight: integer;
begin
  result := getSystemMetrics(SM_CYCAPTION);
end;

function mmpOffScreen(const aHWND: HWND): boolean;
var
  vR: TRect;
begin
  getWindowRect(aHWND, vR);
  result := (vR.bottom > mmpScreenHeight) or (vR.right > mmpScreenWidth) or (vR.left < 0) or (vR.top < 0);
end;

function mmpScreenCentre: integer;
begin
  result := mmpScreenWidth div 2;
end;

function mmpScreenHeight: integer;
begin
  var vRect := screen.WorkAreaRect; // the screen minus the taskbar
  result    := vRect.height - GS.timelineHeight;
end;

function mmpScreenMinHeight: integer;
begin
  result := getSystemMetrics(SM_CYMINTRACK);
end;

function mmpScreenMinWidth: integer;
begin
  result := getSystemMetrics(SM_CXMINTRACK);
end;

function mmpScreenWidth: integer;
// this was getting the width of the virtual desktop not the main monitor!
begin
  result := getSystemMetrics(SM_CXSCREEN); // we'll assume that the taskbar is in it's usual place at the bottom of the screen
end;

function mmpWithinScreenLimits(const aWidth: integer; const aHeight: integer): boolean;
begin
  var vR    := screen.workAreaRect; // the screen minus the taskbar, which we assume is at the bottom of the desktop
  vR.height := vR.height - GS.timelineHeight;
  result    := (aWidth <= vR.width) AND (aHeight <= vR.height);
end;

function mmpWndWidthHeight(const aWnd: HWND; var aWidth: integer; var aHeight: integer): boolean;
var
  vR: TRect;
begin
  getWindowRect(aWnd, vR);
  aWidth    := vR.width;
  aHeight   := vR.height - GS.timelineHeight;
  result    := TRUE;
end;

end.
