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
unit mmpPanelCtrls;

interface

uses
  winApi.windows,
  vcl.comCtrls,
  bazAction,
  mmpConsts, mmpUtils,
  _debugWindow;

const
  PANEL_NAME = 0;
  PANEL_NUMB = 1;
  PANEL_SIZE = 2;
  PANEL_XXYY = 3;
  PANEL_DDXY = 4;
  PANEL_TICK = 5;
  PANEL_AVAV = 6;
  PANEL_FOLD = 7;
  PANEL_HELP = 8;

type
  TPanelName = (pnName, pnNumb, pnSize, pnXXYY, pnDDXY, pnTick, pnAVAV, pnFold, pnHelp);

function mmpInitStatusBar         (const aStatusBar: TStatusBar): TVoid;

function mmpIsPanelAt             (const aStatusBar: TStatusBar; const aPanel: integer; const aPt: TPoint): boolean;
function mmpMousePoint            (const aStatusBar: TStatusBar): TPoint;
function mmpPartClearStatusBar    (const aStatusBar: TStatusBar): TVoid;
function mmpResetPanelHelp        (const aStatusBar: TStatusBar): TVoid;
function mmpResizeStatusBar       (const aStatusBar: TStatusBar): TVoid;
function mmpSetPanelOwnerDraw     (const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aOwnerDraw: boolean): TVoid;
function mmpSetPanelText          (const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aText: string): TVoid;

implementation

uses
  system.types,
  TStatusBarHelperClass;

function mmpInitStatusBar(const aStatusBar: TStatusBar): TVoid;
begin
  mmpResetPanelHelp(aStatusBar);

  mmpSetPanelText(aStatusBar, pnName, THUMB_NO_IMAGES);
  mmpSetPanelText(aStatusBar, pnNumb, EMPTY);
  mmpSetPanelText(aStatusBar, pnSize, EMPTY);
  mmpSetPanelText(aStatusBar, pnXXYY, EMPTY);
  mmpSetPanelText(aStatusBar, pnDDXY, EMPTY);
  mmpSetPanelText(aStatusBar, pnTick, EMPTY);
  mmpSetPanelText(aStatusBar, pnAVAV, EMPTY);
  mmpSetPanelText(aStatusBar, pnFold, EMPTY);
  mmpSetPanelText(aStatusBar, pnHelp, EMPTY);
  mmpResetPanelHelp(aStatusBar);
end;

function mmpIsPanelAt(const aStatusBar: TStatusBar; const aPanel: integer; const aPt: TPoint): boolean;
begin
  result := aStatusBar.getPanelAt(aPt) = aStatusBar.panels[aPanel];
end;

function mmpMousePoint(const aStatusBar: TStatusBar): TPoint;
begin
  result := smallPointToPoint(TSmallPoint(getMessagePos()));
  result := aStatusBar.screenToClient(result);
end;

function mmpResetPanelHelp(const aStatusBar: TStatusBar): TVoid;
begin
  aStatusBar.panels[PANEL_HELP].style := psText;
  aStatusBar.panels[PANEL_HELP].text  := 'Help = Ctrl-[H]';
end;

function mmpPartClearStatusBar(const aStatusBar: TStatusBar): TVoid;
// don't include pnAVAV: it must stay set until the folder changes
begin
  mmpSetPanelText(aStatusBar, pnName, THUMB_NO_IMAGES);
  mmpSetPanelText(aStatusBar, pnNumb, EMPTY);
  mmpSetPanelText(aStatusBar, pnSize, EMPTY);
  mmpSetPanelText(aStatusBar, pnXXYY, EMPTY);
  mmpSetPanelText(aStatusBar, pnDDXY, EMPTY);
  mmpSetPanelText(aStatusBar, pnTick, EMPTY);
  mmpResetPanelHelp(aStatusBar);
end;

function mmpResizeStatusBar(const aStatusBar: TStatusBar): TVoid;
var
  fixedWidths:  integer;
  availWidth:   integer;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  fixedWidths := aStatusBar.panels[PANEL_NUMB].width
               + aStatusBar.panels[PANEL_SIZE].width
               + aStatusBar.panels[PANEL_XXYY].width
               + aStatusBar.panels[PANEL_DDXY].width
               + aStatusBar.panels[PANEL_TICK].width
               + aStatusBar.panels[PANEL_AVAV].width
               + aStatusBar.panels[PANEL_HELP].width;

  availWidth := aStatusBar.width - fixedWidths;

  aStatusBar.panels[PANEL_NAME].width := availWidth div 2;
  aStatusBar.panels[PANEL_FOLD].width := availWidth div 2;

  aStatusBar.invalidate;
  mmpProcessMessages;
end;

function mmpSetPanelOwnerDraw (const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aOwnerDraw: boolean): TVoid;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  case aOwnerDraw of  TRUE: aStatusBar.panels[ord(aPanelName)].style := psOwnerDraw;
                     FALSE: aStatusBar.panels[ord(aPanelName)].style := psText; end;

  mmpProcessMessages;
end;

function mmpSetPanelText(const aStatusBar: TStatusBar; const aPanelName: TPanelName; const aText: string): TVoid;
begin
  case aStatusBar = NIL of TRUE: EXIT; end;

  case aPanelName of
    pnName: aStatusBar.panels[PANEL_NAME].text := aText;
    pnNumb: aStatusBar.panels[PANEL_NUMB].text := aText;
    pnSize: aStatusBar.panels[PANEL_SIZE].text := aText;
    pnXXYY: aStatusBar.panels[PANEL_XXYY].text := aText;
    pnDDXY: aStatusBar.panels[PANEL_DDXY].text := aText;
    pnTick: aStatusBar.panels[PANEL_TICK].text := aText;
    pnAVAV: aStatusBar.panels[PANEL_AVAV].text := aText;
    pnFold: aStatusBar.panels[PANEL_FOLD].text := aText;
    pnHelp: aStatusBar.panels[PANEL_HELP].text := aText;
  end;
  aStatusBar.invalidate;
  mmpProcessMessages;
end;

end.
