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
unit TProgressBarClass;

interface

uses
  winApi.windows,
  system.classes,
  vcl.controls, vcl.extCtrls, vcl.forms,
  ALProgressBar;

type
  TProgressBar = class(TObject)
  strict private
    FPB: TALProgressBar;
    FX: integer;
  private
    FInitialized: boolean;
    FShowProgressBar: boolean;
    procedure progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function  getPosition: integer;
    procedure setPosition(const Value: integer);
    function  getMax: integer;
    procedure setMax(const Value: integer);
    procedure setShowProgressBar(const Value: boolean);
  protected
    procedure onHintShow(var message: TCMHintShow); message CM_HINTSHOW;
  public
    constructor create;
    destructor  Destroy; override;
    function brighter: integer;
    function centreCursor: boolean;
    function darker: integer;
    function formResize: boolean;
    function initProgressBar(const aForm: TForm; const colorDelta: integer = 1): boolean;
    function resetColor: integer;
    function setNewPosition(const x: integer): integer;
    property initialized: boolean read FInitialized;
    property max: integer read getMax write setMax;
    property position: integer read getPosition write setPosition;
    property showProgressBar: boolean read FShowProgressBar write setShowProgressBar;
  end;

function PB: TProgressBar;

implementation

uses
  system.sysUtils,
  vcl.graphics,
  mmpConsts, mmpMPVFormatting,
  TConfigFileClass, TGlobalVarsClass, TKeyboardClass, TMediaPlayerClass,
  _debugWindow;

var
  gPB: TProgressBar;

function PB: TProgressBar;
begin
  case gPB = NIL of TRUE: gPB := TProgressBar.create; end;
  result := gPB;
end;

{ TProgressBar }

function TProgressBar.brighter: integer;
begin
  result := FPB.barColor1;
  case FPB.barColor1 = $FFFFFF of TRUE: EXIT; end;
  FPB.barColor1 := FPB.barColor1 + $010101;
  result := FPB.barColor1;
end;

function TProgressBar.centreCursor: boolean;
begin
  var vPoint := FPB.clientToScreen(point(FPB.width div 2, FPB.height div 2));
  setCursorPos(vPoint.x, vPoint.y);
end;

constructor TProgressBar.create;
begin
  inherited;
  FPB := TALProgressBar.create(NIL);
  FPB.onMouseMove := progressBarMouseMove;
  FPB.onMouseUp   := progressBarMouseUp;

  FShowProgressBar := TRUE;
end;

function TProgressBar.darker: integer;
begin
  result := FPB.barColor1;
  case FPB.barColor1 = $010101 of TRUE: EXIT; end;
  FPB.barColor1 := FPB.barColor1 - $010101;
  result := FPB.barColor1;
end;

destructor TProgressBar.Destroy;
begin
//  FPB.parent := NIL;
//  case FPB <> NIL of TRUE: FPB.free; end;
  inherited;
end;

function TProgressBar.formResize: boolean;
begin
  FPB.Repaint;
end;

function TProgressBar.getMax: integer;
begin
  result := FPB.max;
end;

function TProgressBar.getPosition: integer;
begin
  result := FPB.position;
end;

function TProgressBar.initProgressBar(const aForm: TForm; const colorDelta: integer = 1): boolean;
begin
  FPB.parent           := aForm;
  FPB.align            := alBottom;
  FPB.height           := 10;
  FPB.backgroundColor  := clBlack + 1; // just enough to be different from the clBlack transparent color.
  FPB.borderColor1     := clBlack + 1;
  FPB.borderColor2     := clBlack + 1;
  FPB.showBorder       := FALSE;
  FPB.showPosText      := FALSE;
  FPB.barColor1        := PB_DEFAULT_COLOR + colorDelta;
  FPB.barColorStyle    := cs1Color;
  FPB.onHintShow       := onHintShow;
  FPB.showHint         := TRUE;

  case (colorDelta = 1) AND (CF.asInteger['progressBar'] <> 0) of TRUE: FPB.barColor1 := CF.asInteger['progressBar']; end;

  FPB.max      := 100;
  FPB.Position := 0;
  FInitialized := TRUE;
end;

procedure TProgressBar.onHintShow(var message: TCMHintShow);
begin
  with message.hintInfo^ do
  begin
    hintStr    := formatTime(trunc(MP.duration * (cursorPos.X / FPB.width)));
    cursorRect := rect(cursorPos.X, cursorPos.Y, cursorPos.X, cursorPos.Y);
  end;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var vPoint: TPoint;
begin
  screen.cursor := crHandPoint;
  FPB.hint := formatTime(trunc(MP.duration * (X / FPB.width)));

  case X = FX of TRUE: EXIT; end;
  FX := X;

  FPB.hint := formatTime(trunc(MP.duration * (X / FPB.width)));

  vPoint.X := X;
  vPOint.Y := Y;
  application.activateHint(FPB.clientToScreen(vPoint));

  case (NOT KB.numLock) and NOT (ssCtrl in shift) of TRUE: EXIT; end;
  setNewPosition(x);
  postMessage(GV.appWnd, WM_PROGRESSBAR_CLICK, 0, 0);
end;

procedure TProgressBar.progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// calculate a new video position based on where the progress bar is clicked
begin
  setNewPosition(x);
end;

function TProgressBar.resetColor: integer;
begin
  FPB.barColor1 := PB_DEFAULT_COLOR;
  result        := PB_DEFAULT_COLOR;
end;

procedure TProgressBar.setMax(const Value: integer);
begin
  FPB.max := Value;
end;

function TProgressBar.setNewPosition(const x: integer): integer;
begin
  FPB.Position := round(x * FPB.max / FPB.clientWidth);
  postMessage(GV.appWnd, WM_PROGRESSBAR_CLICK, 0, 0); // change the video position
  postMessage(GV.appWnd, WM_TICK, 0, 0); // update the time display immediately
end;

procedure TProgressBar.setPosition(const Value: integer);
begin
  FPB.Position := Value;
end;

procedure TProgressBar.setShowProgressBar(const Value: boolean);
begin
  FShowProgressBar := Value;
  FPB.Visible := FShowProgressBar;
end;

initialization
  gPB := NIL;

finalization
  case gPB <> NIL of TRUE: gPB.free; end;

end.
