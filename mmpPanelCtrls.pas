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
  mmpConsts, mmpUtils;

type
  TPanelName = (pnName, pnNumb, pnSize, pnXXYY, pnTick, pnSave, pnHelp);

function mmpInitStatusBar(const aStatusBar: TStatusBar): boolean;
function mmpResetPanelVers(const aStatusBar: TStatusBar): boolean;
function mmpResizeStatusBar(const aStatusBar: TStatusBar): boolean;
function mmpSetPanelText(const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aText: string): boolean;

implementation

const
  PANEL_NAME = 0;
  PANEL_NUMB = 1;
  PANEL_SIZE = 2;
  PANEL_XXYY = 3;
  PANEL_HINT = 4;
  PANEL_SAVE = 5;
  PANEL_HELP = 6;

function mmpInitStatusBar(const aStatusBar: TStatusBar): boolean;
begin
  mmpResetPanelVers(aStatusBar);
  mmpSetPanelText(aStatusBar, pnTick, '');

  mmpSetPanelText(aStatusBar, pnName, THUMB_NO_IMAGES);
  mmpSetPanelText(aStatusBar, pnNumb, '');
  mmpSetPanelText(aStatusBar, pnSize, '');
  mmpSetPanelText(aStatusBar, pnXXYY, '');
  mmpSetPanelText(aStatusBar, pnTick, '');
  mmpSetPanelText(aStatusBar, pnSave, '');
  mmpSetPanelText(aStatusBar, pnHelp, '');
end;

function mmpResetPanelVers(const aStatusBar: TStatusBar): boolean;
begin
  aStatusBar.panels[PANEL_HELP].text := 'Help = Ctrl-[H]';
end;

function mmpResizeStatusBar(const aStatusBar: TStatusBar): boolean;
var
  fixedWidths:  integer;
  availWidth:   integer;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  fixedWidths := aStatusBar.panels[PANEL_NUMB].width
               + aStatusBar.panels[PANEL_SIZE].width
               + aStatusBar.panels[PANEL_XXYY].width
               + aStatusBar.panels[PANEL_HINT].width
               + aStatusBar.panels[PANEL_HELP].width;

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
    pnXXYY: aStatusBar.panels[PANEL_XXYY].text := aText;
    pnTick: aStatusBar.panels[PANEL_HINT].text := aText;
    pnSave: aStatusBar.panels[PANEL_SAVE].text := aText;
    pnHelp: aStatusBar.panels[PANEL_HELP].text := aText;
  end;
  aStatusBar.refresh;
end;

end.
