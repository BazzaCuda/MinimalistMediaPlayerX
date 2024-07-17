{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit TStatusBarHelperClass;

interface

uses
  winApi.commCtrl, winApi.windows,
  vcl.comCtrls;

type
  TStatusBarHelper = class helper for TStatusBar
  public
    function getPanelAt(const X: integer; const Y: Integer):  TStatusPanel; overload;
    function getPanelAt(const aPt: TPoint):                   TStatusPanel; overload;
  end;

implementation

uses
  system.classes;

function TStatusBarHelper.getPanelAt(const X: integer; const Y: Integer): TStatusPanel;
begin
  result := getPanelAt(point(X, Y));
end;

function TStatusBarHelper.getPanelAt(const aPt: TPoint): TStatusPanel;
var
  vIx:    integer;
  vArr:   array of integer;
  vPanel: TStatusPanel;
begin
  result := NIL;

  case PtInRect(SELF.clientRect, aPt) of FALSE: EXIT; end;

  setLength(vArr, sendMessage(SELF.handle, SB_GETPARTS, 0, 0));
  sendMessage(SELF.handle, SB_GETPARTS, length(vArr), LPARAM(pInteger(vArr)));

  vIx := 0;
  while vIx < length(vArr) do case (aPt.X <= vArr[vIx]) or (vArr[vIx] = -1) of   TRUE: begin result := SELF.panels[vIx]; BREAK; end;
                                                                                FALSE: inc(vIx); end;
end;

end.
