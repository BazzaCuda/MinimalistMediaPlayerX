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
unit globalVars;

interface

uses  winAPI.windows, system.classes, vcl.forms;

type
  TGlobalVars = class(TObject)
  strict private
    FMainWnd: HWND;
  private
    FMainForm: TForm;
    FMainLeft: integer;
    FMainTop: integer;
    FMainWidth: integer;
    function getMainTopRightPt: TPoint;
  public
    property mainForm: TForm read FMainForm write FMainForm;
    property mainWnd: HWND read FMainWnd write FMainWnd;
    property mainLeft: integer read FMainLeft write FMainLeft;
    property mainTopRightPt: TPoint read getMainTopRightPt;
    property mainTop: integer read FMainTop write FMainTop;
    property mainWidth: integer read FMainWidth write FMainWidth;
  end;

function GV: TGlobalVars;

implementation

uses
  vcl.controls;

var
  gGV: TGlobalVars;

function GV: TGlobalVars;
begin
  case gGV = NIL of TRUE: gGV := TGlobalVars.create; end;
  result := gGV;
end;

{ TGlobalVars }

function TGlobalVars.getMainTopRightPt: TPoint;
begin
  with TForm.create(NIL) do begin
  result := ClientToScreen(point(FMainLeft + FMainWidth - 17, FMainTop + 1)); // screen position of the top right corner of the application window, roughly.
  result := ClientToScreen(point(FMainLeft, FMainTop)); // screen position of the top right corner of the application window, roughly.
  free;
  end;
end;

initialization
  gGV := NIL;

finalization
  case gGV <> NIL of TRUE: gGV.free; end;

end.
