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
unit mmpPanelCtrls;

interface

uses
  vcl.comCtrls,
  mmpUtils;

type
  TPanelName = (pnName, pnNumb, pnSize, pnHint, pnSave, pnVers);

function mmpInitStatusBar(const aStatusBar: TStatusBar): boolean;
function mmpResizeStatusBar(const aStatusBar: TStatusBar): boolean;
function mmpSetPanelText(const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aText: string): boolean;

implementation

const
  PANEL_NAME = 0;
  PANEL_NUMB = 1;
  PANEL_SIZE = 2;
  PANEL_HINT = 3;
  PANEL_SAVE = 4;
  PANEL_VERS = 5;

function mmpInitStatusBar(const aStatusBar: TStatusBar): boolean;
begin
  aStatusBar.panels[PANEL_VERS].text := 'Help = Ctrl-[H]'; // won't currently be seen
end;

function mmpResizeStatusBar(const aStatusBar: TStatusBar): boolean;
var
  fixedWidths:  integer;
  availWidth:   integer;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  fixedWidths := aStatusBar.panels[PANEL_NUMB].width
               + aStatusBar.panels[PANEL_SIZE].width
               + aStatusBar.panels[PANEL_HINT].width
               + aStatusBar.panels[PANEL_VERS].width;

  availWidth := aStatusBar.width - fixedWidths;

  aStatusBar.panels[PANEL_NAME].width := availWidth div 2;
  aStatusBar.panels[PANEL_SAVE].width := availWidth div 2;
end;

function mmpSetPanelText(const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aText: string): boolean;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  case aPanelName of
    pnName: aStatusBar.panels[PANEL_NAME].text := aText;
    pnNumb: aStatusBar.panels[PANEL_NUMB].text := aText;
    pnSize: aStatusBar.panels[PANEL_SIZE].text := aText;
    pnHint: aStatusBar.panels[PANEL_HINT].text := aText;
    pnSave: aStatusBar.panels[PANEL_SAVE].text := aText;
    pnVers: aStatusBar.panels[PANEL_VERS].text := aText;
  end;
  aStatusBar.refresh;
end;

end.
