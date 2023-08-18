{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit progressBar;

interface

uses
  ALProgressBar, vcl.forms, vcl.controls, system.classes, vcl.extCtrls, winApi.windows;

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
    constructor create;
    procedure onHintShow(var message: TCMHintShow); message CM_HINTSHOW;
    function  setNewPosition(const x: integer): integer;
  public
    destructor  Destroy; override;
    function brighter: boolean;
    function darker: boolean;
    function formResize: boolean;
    function initProgressBar(const aForm: TForm): boolean;
    property initialized: boolean read FInitialized;
    property max: integer read getMax write setMax;
    property position: integer read getPosition write setPosition;
    property showProgressBar: boolean read FShowProgressBar write setShowProgressBar;
  end;

function PB: TProgressBar;

implementation

uses
  vcl.graphics, consts, globalVars, keyboard, commonUtils, mediaPlayer, system.sysUtils, configFile, _debugWindow;

var
  gPB: TProgressBar;

function PB: TProgressBar;
begin
  case gPB = NIL of TRUE: gPB := TProgressBar.create; end;
  result := gPB;
end;

{ TProgressBar }

function TProgressBar.brighter: boolean;
begin
  FPB.barColor1 := FPB.barColor1 + $010101;
  CF.value['progressBar'] := intToStr(FPB.barColor1);
end;

constructor TProgressBar.create;
begin
  inherited;
  FPB := TALProgressBar.create(NIL);
  FPB.onMouseMove := progressBarMouseMove;
  FPB.onMouseUp   := progressBarMouseUp;

  FShowProgressBar := TRUE;
end;

function TProgressBar.darker: boolean;
begin
  FPB.barColor1 := FPB.barColor1 - $010101;
  CF.value['progressBar'] := intToStr(FPB.barColor1);
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

function TProgressBar.initProgressBar(const aForm: TForm): boolean;
begin
  FPB.parent           := aForm;
  FPB.align            := alBottom;
  FPB.height           := 10;
  FPB.backgroundColor  := clBlack + 1; // just enough to be different from the clBlack transparent color.
  FPB.borderColor1     := clBlack + 1;
  FPB.borderColor2     := clBlack + 1;
  FPB.showBorder       := FALSE;
  FPB.showPosText      := FALSE;
  FPB.barColor1        := $202020;
  FPB.barColorStyle    := cs1Color;
  FPB.onHintShow       := onHintShow;
  FPB.showHint         := TRUE;

  case CF.asInteger['progressBar'] <> 0 of TRUE: FPB.barColor1 := CF.asInteger['progressBar']; end;

  FPB.max      := 100;
  FPB.Position := 0;
  FInitialized := TRUE;
end;

procedure TProgressBar.onHintShow(var message: TCMHintShow);
begin
  with message.hintInfo^ do
  begin
    hintStr    := CU.formatTime(trunc(MP.duration * (cursorPos.X / FPB.width)));
    cursorRect := rect(cursorPos.X, cursorPos.Y, cursorPos.X, cursorPos.Y);
  end;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var vPoint: TPoint;
begin
  screen.cursor := crHandPoint;
  FPB.hint := CU.formatTime(trunc(MP.duration * (X / FPB.width)));

  case X = FX of TRUE: EXIT; end;
  FX := X;

  FPB.hint := CU.formatTime(trunc(MP.duration * (X / FPB.width)));

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
  postMessage(GV.appWnd, WM_PROGRESSBAR_CLICK, 0, 0); // change the video position
  postMessage(GV.appWnd, WM_TICK, 0, 0); // update the time display immediately
end;

procedure TProgressBar.setMax(const Value: integer);
begin
  FPB.max := Value;
end;

function TProgressBar.setNewPosition(const x: integer): integer;
begin
  FPB.Position := round(x * FPB.max / FPB.clientWidth);
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
