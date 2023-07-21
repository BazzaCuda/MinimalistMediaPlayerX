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

uses  winAPI.windows, system.classes, vcl.forms, vcl.extCtrls;

type
  TGlobalVars = class(TObject)
  strict private
    FMainWnd: HWND;
    FCloseTimer: TTimer;
  private
    FDragging: boolean;
    FMainForm: TForm;
    FMainLeft: integer;
    FMainTop: integer;
    FMainWidth: integer;
    FUIWnd: HWND;
    FMouseDown: boolean;
    function getMainTopRightPt: TPoint;
    procedure onCloseTimerEvent(sender: TObject);
    procedure setCloseApp(const Value: boolean);
  public
    constructor create;
    destructor  destroy;
    property closeApp: boolean write setCloseApp;
    property dragging: boolean read FDragging write FDragging;
    property mainForm: TForm read FMainForm write FMainForm;
    property mainWnd: HWND read FMainWnd write FMainWnd;
    property mainLeft: integer read FMainLeft write FMainLeft;
    property mainTopRightPt: TPoint read getMainTopRightPt;
    property mainTop: integer read FMainTop write FMainTop;
    property mainWidth: integer read FMainWidth write FMainWidth;
    property mouseDown: boolean read FMouseDown write FMouseDown;
    property UIWnd: HWND read FUIWnd write FUIWnd;
  end;

function GV: TGlobalVars;

implementation

uses
  vcl.controls, sysCommands;

var
  gGV: TGlobalVars;

function GV: TGlobalVars;
begin
  case gGV = NIL of TRUE: gGV := TGlobalVars.create; end;
  result := gGV;
end;

{ TGlobalVars }

constructor TGlobalVars.create;
begin
  inherited;
  FCloseTimer := TTimer.create(NIL);
  FCloseTimer.enabled := FALSE;
  FCloseTimer.interval := 100;
  FCloseTimer.OnTimer := onCloseTimerEvent;
end;

destructor TGlobalVars.destroy;
begin
  case FCloseTimer <> NIL of TRUE: FCloseTimer.free; end;
end;

function TGlobalVars.getMainTopRightPt: TPoint;
begin
  with TForm.create(NIL) do begin  // WHAT!!???
    result := ClientToScreen(point(FMainLeft + FMainWidth - 17, FMainTop + 1)); // screen position of the top right corner of the application window, roughly.
    result := ClientToScreen(point(FMainLeft, FMainTop)); // screen position of the top right corner of the application window, roughly.
    free;
  end;
end;

procedure TGlobalVars.onCloseTimerEvent(sender: TObject);
begin
  FCloseTimer.enabled := FALSE;
  sendSysCommandClose(UIWnd);
end;

procedure TGlobalVars.setCloseApp(const Value: boolean);
begin
  FCloseTimer.enabled := value;
end;

initialization
  gGV := NIL;

finalization
  case gGV <> NIL of TRUE: gGV.free; end;

end.
