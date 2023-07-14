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
  ALProgressBar, vcl.forms, vcl.controls, system.classes;

type
  TProgressBar = class(TObject)
  strict private
    FPB: TALProgressBar;
  private
    procedure progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function getPosition: integer;
    procedure setPosition(const Value: integer);
    function getMax: integer;
    procedure setMax(const Value: integer);
  protected
    constructor create;
  public
    destructor  Destroy; override;
    function initProgressBar(aForm: TForm): boolean;
    property max: integer read getMax write setMax;
    property position: integer read getPosition write setPosition;
  end;

function PB: TProgressBar;

implementation

uses
  vcl.graphics, _debugWindow;

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
end;

destructor TProgressBar.Destroy;
begin
  debug('TProgressBar.Destroy');
//  FPB.parent := NIL;
//  case FPB <> NIL of TRUE: FPB.free; end;
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

function TProgressBar.initProgressBar(aForm: TForm): boolean;
begin
  FPB.parent           := aForm;
  FPB.align            := alBottom;
  FPB.height           := 10;
  FPB.backgroundColor  := clBlack;
  FPB.borderColor1     := clBlack;
  FPB.borderColor2     := clBlack;
  FPB.showBorder       := FALSE;
  FPB.showPosText      := FALSE;
  FPB.barColor1        := $202020;
  FPB.barColorStyle    := cs1Color;

  FPB.max      := 100;
  FPB.Position := 50;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TProgressBar.progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// calculate a new video position based on where the progress bar is clicked
begin
  FPB.position := 75; // TEMPORARY TEST
end;

procedure TProgressBar.setMax(const Value: integer);
begin
  FPB.max := Value;
end;

procedure TProgressBar.setPosition(const Value: integer);
begin
  FPB.Position := Value;
end;

initialization
  gPB := NIL;

finalization
  case gPB <> NIL of TRUE: gPB.free; end;

end.
