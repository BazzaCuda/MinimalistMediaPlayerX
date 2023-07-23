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
    FMoving: boolean;
//    FTimer: TTimer;
  private
    FShowProgressBar: boolean;
    procedure progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function  getPosition: integer;
    procedure setPosition(const Value: integer);
    function  getMax: integer;
    procedure setMax(const Value: integer);
//    procedure timerEvent(Sender: TObject);
    function getTop: integer;
    procedure setShowProgressBar(const Value: boolean);
  protected
    constructor create;
    function  setNewPosition(x: integer): integer;
  public
    destructor  Destroy; override;
    function initProgressBar(aForm: TForm): boolean;
    property max: integer read getMax write setMax;
    property position: integer read getPosition write setPosition;
    property showProgressBar: boolean read FShowProgressBar write setShowProgressBar;
  end;

function PB: TProgressBar;

implementation

uses
  vcl.graphics, consts, globalVars, keyboard, _debugWindow;

var
  gPB: TProgressBar;

function PB: TProgressBar;
begin
  case gPB = NIL of TRUE: gPB := TProgressBar.create; end;
  result := gPB;
end;

{ TProgressBar }

constructor TProgressBar.create;
begin
  inherited;
  FPB := TALProgressBar.create(NIL);
  FPB.onMouseMove := progressBarMouseMove;
  FPB.onMouseUp   := progressBarMouseUp;

//  FTimer          := TTimer.create(NIL);
//  FTimer.enabled  := FALSE;
//  FTimer.interval := 100;
//  FTimer.OnTimer  := timerEvent;

  FShowProgressBar := TRUE;
end;

destructor TProgressBar.Destroy;
begin
//  FPB.parent := NIL;
//  case FPB <> NIL of TRUE: FPB.free; end;
//  case FTimer <> NIL of TRUE: FTimer.free; end;
  inherited;
end;

function TProgressBar.getMax: integer;
begin
  result := FPB.max;
end;

function TProgressBar.getPosition: integer;
begin
  result := FPB.position;
end;

function TProgressBar.getTop: integer;
begin
  result := FPB.Top;
end;

function TProgressBar.initProgressBar(aForm: TForm): boolean;
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

  FPB.max      := 100;
  FPB.Position := 0;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case (not KB.numLock) AND not (ssCtrl in shift) of TRUE: EXIT; end;
//  case FMoving        of TRUE: EXIT; end; // only allow one drag operation every 100ms otherwise MMF gets upset - but MPV doesn't seem to have any problem!
  setNewPosition(x);
  postMessage(GV.appWnd, WM_PROGRESSBAR_CLICK, 0, 0);
//  FMoving         := TRUE;
//  FTimer.enabled  := TRUE;
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

function TProgressBar.setNewPosition(x: integer): integer;
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

//procedure TProgressBar.timerEvent(Sender: TObject);
//// limit dragging operations to one every 100ms otherwise MMF gets upset and crashes the video
//begin
//  FTimer.enabled := FALSE;
//  FMoving        := FALSE;
//end;

initialization
  gPB := NIL;

finalization
  case gPB <> NIL of TRUE: gPB.free; end;

end.
