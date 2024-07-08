{   MMP: Minimalist Media Player
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
unit view.mmpProgressBar;

interface

uses
  winApi.windows,
  system.classes,
  vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  model.mmpConfigFile,
  ALProgressBar;

type
  IProgressBar = interface
    ['{2CF981F3-CE41-48EA-A365-13E9F7986238}']
    function    getNotifier: INotifier;
    function    initProgressBar(const aForm: TForm; const aColor: TColor; const aColorDelta: TColor): IProgressBar;
    function    notify(const aNotice: INotice): INotice;
    property    notifier: INotifier read getNotifier;
  end;

function newProgressBar: IProgressBar;

implementation

uses
  system.sysUtils, system.types,
  mmpConsts,
  viewModel.mmpGlobalState, viewModel.mmpMPVFormatting,
  _debugWindow;

type
  TProgressBar = class(TInterfacedObject, IProgressBar)
  strict private
    FPB:              TALProgressBar;
    FX:               integer;
    FNotifier:        INotifier;
    FInitialized:     boolean;
    FShowProgressBar: boolean;
  private
    function  brighter:      integer;
    function  centerCursor:  boolean;
    function  darker:        integer;
    function  formResize:    boolean;
    function  resetColor: integer;
    function  onNotify(const aNotice: INotice): INotice;
  protected
    function  getPosition: integer;

    procedure setMax(const aValue: integer);
    function  setNewPosition(const x: integer): integer;
    procedure setShowProgressBar(const aValue: boolean);

    procedure onHintShow(var message: TCMHintShow); message CM_HINTSHOW;
    procedure progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    constructor create;
    destructor  Destroy; override;
    function    getNotifier: INotifier;
    function    initProgressBar(const aForm: TForm; const aColor: TColor; const aColorDelta: TColor): IProgressBar;
    function    notify(const aNotice: INotice): INotice;
  end;

function newProgressBar: IProgressBar;
begin
  result := TProgressBar.create;
end;

{ TProgressBar }

function TProgressBar.brighter: integer;
begin
  result            := FPB.barColor;
  case FPB.barColor = $FFFFFF of TRUE: EXIT; end;
  FPB.barColor      := FPB.barColor + $010101;
  result            := FPB.barColor;
  CF[CONF_PROGRESS_BAR] := CF.toHex(result);
end;

function TProgressBar.centerCursor: boolean;
begin
  result := FALSE;
  var vPoint := FPB.clientToScreen(point(FPB.width div 2, FPB.height div 2));
  setCursorPos(vPoint.x, vPoint.y);
  result := TRUE;
end;

constructor TProgressBar.create;
begin
  inherited;
  FPB             := TALProgressBar.create(NIL);
  FPB.onMouseMove := progressBarMouseMove;
  FPB.onMouseUp   := progressBarMouseUp;

  FShowProgressBar := TRUE;

  appNotifier.subscribe(newSubscriber(onNotify)); // appNotifier and PB.notifier Notices all go to the same method
end;

function TProgressBar.darker: integer;
begin
  result            := FPB.barColor;
  case FPB.barColor = $010101 of TRUE: EXIT; end;
  FPB.barColor      := FPB.barColor - $010101;
  result            := FPB.barColor;
  CF[CONF_PROGRESS_BAR] := CF.toHex(result);
end;

destructor TProgressBar.Destroy;
begin
//  FPB.parent := NIL;
//  case FPB <> NIL of TRUE: FPB.free; end;
  inherited;
end;

function TProgressBar.formResize: boolean;
begin
  result := FALSE;
  FPB.repaint;
  result := TRUE;
end;

function TProgressBar.getNotifier: INotifier;
begin
  case FNotifier = NIL of TRUE: FNotifier := newNotifier; end;
  result := FNotifier;
end;

function TProgressBar.getPosition: integer;
begin
  result := FPB.position;
end;

function TProgressBar.initProgressBar(const aForm: TForm; const aColor: TColor; const aColorDelta: TColor): IProgressBar;
begin
  result               := SELF;
  FPB.parent           := aForm;
  FPB.align            := alBottom;
  FPB.height           := 10;
  FPB.backgroundColor  := clBlack + 1; // just enough to be different from the clBlack transparent color.
  FPB.barColor         := PB_DEFAULT_COLOR + aColorDelta;
  FPB.onHintShow       := onHintShow;
  FPB.showHint         := TRUE;

  case aColor <> 0 of TRUE: FPB.barColor := aColor end;

  FPB.max      := 100;
  FPB.position := 0;
  FInitialized := TRUE;
end;

function TProgressBar.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TProgressBar.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evPBBrighter:             brighter;
    evPBDarker:               darker;
    evPBMax:                  setMax(aNotice.integer);
    evPBPosition:             FPB.position := aNotice.integer;
    evPBReset:                resetColor;
    evPBSetNewPosition:       aNotice.integer := setNewPosition(aNotice.integer);
    evPBToggleProgressBar:    setShowProgressBar(NOT FShowProgressBar);

    evPBReqMax:               aNotice.integer := FPB.max;
    evPBReqPosition:          aNotice.integer := FPB.position;

    evMPDuration:             setMax(aNotice.integer);
    evMPPosition:             FPB.position := aNotice.integer;

    evSTBlankInTimeCaption:   setShowProgressBar(TRUE);
    evSTBlankOutTimeCaption:  setShowProgressBar(FALSE);

    evWndResize:              formResize;
  end;
end;

procedure TProgressBar.onHintShow(var message: TCMHintShow);
begin
  with message.hintInfo^ do
  begin
    hintStr    := mmpFormatTime(trunc(FPB.max * (cursorPos.X / FPB.width)));
    cursorRect := rect(cursorPos.X, cursorPos.Y, cursorPos.X, cursorPos.Y);
  end;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var vPoint: TPoint;
begin
  screen.cursor := crHandPoint;
  FPB.hint := mmpFormatTime(trunc(FPB.max * (X / FPB.width)));

  case X = FX of TRUE: EXIT; end;
  FX := X;

  FPB.hint := mmpFormatTime(trunc(FPB.max * (X / FPB.width)));

  vPoint.X := X;
  vPOint.Y := Y;
  application.activateHint(FPB.clientToScreen(vPoint));

  case NOT (ssCtrl in shift) of TRUE: EXIT; end;
  setNewPosition(x);
end;

procedure TProgressBar.progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// calculate a new video position based on where the progress bar is clicked
begin
  setNewPosition(x);
end;

function TProgressBar.resetColor: integer;
begin
  FPB.barColor := PB_DEFAULT_COLOR;
  result       := PB_DEFAULT_COLOR;
  CF[CONF_PROGRESS_BAR] := CF.toHex(result);
end;

procedure TProgressBar.setMax(const aValue: integer);
begin
  case FPB.max <> aValue of TRUE: FPB.max := aValue; end;
end;

function TProgressBar.setNewPosition(const x: integer): integer;
// only call in response to a mouse click on the progress bar
// This includes dragging the timeline cursor, which is effectively the same thing.
begin
  FPB.Position := round(x * FPB.max / FPB.clientWidth);
  notifyApp(newNotice(evPBClick, FPB.position));
  result := FPB.position;
end;

procedure TProgressBar.setShowProgressBar(const aValue: boolean);
begin
  FShowProgressBar := aValue;
  FPB.visible := FShowProgressBar;
end;

end.
