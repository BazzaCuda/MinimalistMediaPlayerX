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
unit view.mmpThemeUtils;

interface

uses
  winApi.shellApi, winApi.windows,
  system.classes,
  vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpConsts, mmpDesktopUtils;

function mmpInitTransparentForm(const aForm: TForm): TForm;
function mmpInitTransparentLabel(const aLabel: TLabel): boolean;
function mmpThemeCreateVideoPanel(const aForm: TForm): TPanel;
function mmpThemeInitForm(const aForm: TForm): boolean;

function mmpSetCustomTitleBar(const aForm: TForm; const aHeight: integer = 1): boolean;
function mmpSetGlassFrame(const aForm: TForm): boolean;

implementation

function mmpThemeCreateVideoPanel(const aForm: TForm): TPanel;
begin
  result              := TPanel.create(aForm);
  result.parent       := aForm;
  result.align        := alClient;
  result.color        := clBlack;
  result.bevelOuter   := bvNone;
  result.caption      := 'MMP: Minimalist Media Player';
  result.font.style   := [fsBold];
end;

function mmpInitTransparentForm(const aForm: TForm): TForm;
begin
  aForm.align                  := alBottom;
  aForm.styleElements          := []; // don't allow any theme alterations
  aForm.borderStyle            := bsNone;
  aForm.color                  := clBlack;
  aForm.ctl3D                  := FALSE;
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

function mmpSetCustomTitleBar(const aForm: TForm; const aHeight: integer = 1): boolean;
begin
  aForm.customTitleBar.enabled        := TRUE;
  aForm.customTitleBar.showCaption    := FALSE;
  aForm.customTitleBar.showIcon       := FALSE;
  aForm.customTitleBar.systemButtons  := FALSE;
  aForm.customTitleBar.systemColors   := FALSE;
  aForm.customTitleBar.systemHeight   := FALSE;
  aForm.customTitleBar.height         := aHeight; // systemHeight=FALSE must be set before this
end;

function mmpSetGlassFrame(const aForm: TForm): boolean;
begin
  aForm.glassFrame.enabled  := TRUE;
  aForm.glassFrame.top      := 1;
end;

function mmpSetWindowStyle(const aForm: TForm): boolean;
begin
  setWindowLong(aForm.handle, GWL_STYLE, getWindowLong(aForm.handle, GWL_STYLE) OR WS_CAPTION AND NOT WS_BORDER AND NOT WS_VISIBLE);
end;

function mmpThemeInitForm(const aForm: TForm): boolean;

  function copiedFromDFM: boolean;
  begin
    aForm.Left        := 0;
    aForm.Top         := 0;
    aForm.BorderIcons := [biSystemMenu, biMinimize, biMaximize, biHelp];
    aForm.Caption     := MMP_TITLE;
    aForm.Color       := clGray;
    aForm.CustomTitleBar.Height                         := 31;
    aForm.CustomTitleBar.SystemHeight                   := False;
    aForm.CustomTitleBar.ShowCaption                    := False;
    aForm.CustomTitleBar.ShowIcon                       := False;
    aForm.CustomTitleBar.SystemColors                   := False;
    aForm.CustomTitleBar.SystemButtons                  := False;
    aForm.CustomTitleBar.BackgroundColor                := clBlack;
    aForm.CustomTitleBar.ForegroundColor                := clWhite;
    aForm.CustomTitleBar.InactiveBackgroundColor        := clBlack;
    aForm.CustomTitleBar.InactiveForegroundColor        := clWhite;
    aForm.CustomTitleBar.ButtonForegroundColor          := clWhite;
    aForm.CustomTitleBar.ButtonBackgroundColor          := clBlack;
    aForm.CustomTitleBar.ButtonHoverForegroundColor     := clWhite;
    aForm.CustomTitleBar.ButtonHoverBackgroundColor     := 1381653;
    aForm.CustomTitleBar.ButtonPressedForegroundColor   := clWhite;
    aForm.CustomTitleBar.ButtonPressedBackgroundColor   := 3487029;
    aForm.CustomTitleBar.ButtonInactiveForegroundColor  := clWhite;
    aForm.CustomTitleBar.ButtonInactiveBackgroundColor  := clBlack;
    aForm.Font.Charset  := DEFAULT_CHARSET;
    aForm.Font.Color    := clWindowText;
    aForm.Font.Height   := -11;
    aForm.Font.Name     := FONT_TAHOMA;
    aForm.Font.Style    := [];
  end;

begin
  copiedFromDFM;
  aForm.position      := poScreenCenter;
  aForm.borderIcons   := [biSystemMenu];
  aForm.styleElements := [];
  mmpSetGlassFrame(aForm);
  mmpSetCustomTitleBar(aForm);
  mmpSetWindowStyle(aForm);
  dragAcceptFiles(aForm.handle, TRUE);
  aForm.color         := clBlack; // background color of the window's client area, so zooming-out doesn't show the design-time color
//  aForm.width         := trunc((mmpScreenHeight - 100) * 1.5);
//  aForm.height        := mmpScreenHeight - 100;
end;

end.
