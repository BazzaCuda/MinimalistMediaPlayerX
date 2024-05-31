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
unit mmpTransparentUtils;

interface

uses
  system.classes,
  vcl.controls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpConsts;

function mmpInitTransparentForm(const aForm: TForm): TForm;
function mmpInitTransparentLabel(const aLabel: TLabel): boolean;

implementation

function mmpInitTransparentForm(const aForm: TForm): TForm;
begin
  aForm.align                  := alBottom;
  aForm.styleElements          := []; // don't allow any theme alterations
  aForm.borderStyle            := bsNone;
  aForm.color                  := clBlack;
  aForm.ctl3D                  := FALSE;
//  aForm.doubleBuffered         := TRUE; // EXPERIMENTALLY COMMENTED OUT
  aForm.margins.bottom         := 0;
  aForm.formStyle              := fsStayOnTop; // Keep the form always on top - hmmm. How does this impact infoPanel?
  aForm.borderIcons            := [];
  aForm.alphaBlend             := True;
  aForm.alphaBlendValue        := 255;
  aForm.transparentColorValue  := clBlack;
  aForm.transparentColor       := TRUE;
  result := aForm;
end;

function mmpInitTransparentLabel(const aLabel: TLabel): boolean;
begin
  aLabel.align             := alClient;
  aLabel.alignment         := taCenter;
  aLabel.alignWithMargins  := TRUE;
  aLabel.color             := clBlack;
  aLabel.font.color        := ST_DEFAULT_COLOR;
  aLabel.font.size         := 14;
  aLabel.font.style        := [fsBold];
  aLabel.layout            := tlBottom;
  aLabel.margins.Bottom    := 6;
  aLabel.parentColor       := FALSE;
  aLabel.parentCustomHint  := FALSE;
  aLabel.parentFont        := FALSE;
  aLabel.ParentShowHint    := FALSE;
  aLabel.showAccelChar     := FALSE;
  aLabel.showHint          := FALSE;
  aLabel.transparent       := TRUE;
  aLabel.wordWrap          := FALSE;
end;

end.
