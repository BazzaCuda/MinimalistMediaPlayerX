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
  protected
    constructor create;
    destructor  Destroy; override;
    property PB: TALProgressBar read FPB write FPB;
  end;

function PB: TALProgressBar;
function initProgressBar(aForm: TForm): boolean;

implementation

uses
  vcl.graphics;

var
  gPB: TProgressBar;

function PB: TALProgressBar;
begin
  case gPB = NIL of TRUE: gPB := TProgressBar.create; end;
  result := gPB.PB;
end;

function initProgressBar(aForm: TForm): boolean;
begin
  PB.parent           := aForm; // this first reference to PB causes the singleton to be created
  PB.align            := alBottom;
  PB.height           := 10;
  PB.backgroundColor  := clBlack;
  PB.borderColor1     := clBlack;
  PB.borderColor2     := clBlack;
  PB.showBorder       := FALSE;
  PB.showPosText      := FALSE;
  PB.barColor1        := $202020;
  PB.barColorStyle    := cs1Color;

  PB.max      := 100;
  PB.Position := 50;
end;

{ TProgressBar }

constructor TProgressBar.create;
begin
  inherited;
  FPB := TALProgressBar.create(NIL);
  FPB.onMouseMove := progressBarMouseMove;
  FPB.onMouseUp   := progressBarMouseUp;
end;

destructor TProgressBar.destroy;
begin
  case FPB <> NIL of TRUE: FPB.free; end;
  inherited;
end;

procedure TProgressBar.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TProgressBar.progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// calculate a new video position based on where the progress bar is clicked
begin
  PB.position := 75; // TEMPORARY TEST
end;

initialization
  gPB := NIL;

finalization
  case gPB <> NIL of TRUE: gPB.free; end;

end.
