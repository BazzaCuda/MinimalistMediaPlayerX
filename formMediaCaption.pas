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
unit formMediaCaption;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls;

type
  TMediaCaptionForm = class(TForm) // single caption at the top of the window
  strict private
    FVideoPanel:  TPanel;
    FCaption:     TLabel;
    FInitialized: boolean;
    FOpInfoTimer: TTimer;
  private
    procedure timerEvent(sender: TObject);
    procedure setCaption(const Value: string);
    function startOpInfoTimer: boolean;
  public
    constructor create;
    procedure formResize(Sender: TObject);
    function brighter: integer;
    function darker: integer;
    function toggleCaption: boolean;
    function initCaption(const aVideoPanel: TPanel): boolean;
    function resetColor: integer;
    property caption: string write setCaption;
  end;

implementation

uses
  mmpConsts, mmpSingletons, mmpTransparentUtils,
  _debugWindow;

{$R *.dfm}

{ TCaptionForm }

function TMediaCaptionForm.brighter: integer;
begin
  result := FCaption.font.color;
  case FCaption.font.color = $FFFFFF of TRUE: EXIT; end;
  FCaption.font.color := FCaption.font.color + $010101;
  result := FCaption.font.color;
end;

constructor TMediaCaptionForm.create;
begin
  inherited create(GV.mainForm);
  height := 80;
  FCaption := TLabel.create(SELF);

  FOpInfoTimer := NIL;
end;

function TMediaCaptionForm.darker: integer;
begin
  result := FCaption.font.color;
  case FCaption.font.color = $010101 of TRUE: EXIT; end;
  FCaption.font.color := FCaption.font.color - $010101;
  result := FCaption.font.color;
end;

procedure TMediaCaptionForm.formResize(Sender: TObject);
begin
  SELF.HEIGHT := 80;
  FCaption.caption := 'resize';
  FCaption.left := SELF.width - FCaption.width;
  FCaption.top  := SELF.height - FCaption.height;
end;

function TMediaCaptionForm.initCaption(const aVideoPanel: TPanel): boolean;
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    aLabel.font.name      := 'Tahoma';
    aLabel.font.color     := ST_DEFAULT_COLOR;
    aLabel.font.size      := 10;
    aLabel.font.style     := [fsBold];
    aLabel.margins.top    := 3;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 0;
    aLabel.wordWrap       := FALSE;
    aLabel.caption        := '';
  end;
begin
  case FInitialized of TRUE: EXIT; end;
  FVideoPanel := aVideoPanel;

  SELF.parent := aVideoPanel;
  mmpInitTransparentForm(SELF);
  SELF.align := alTop;

  FCaption.parent := SELF;
  mmpInitTransparentLabel(FCaption);
  defaultFontEtc(FCaption);
  FCaption.align         := alTop;
  FCaption.alignment     := taLeftJustify;
  FCaption.StyleElements := [];

  case CF.asInteger['caption'] <> 0 of TRUE: FCaption.font.color := CF.asInteger['caption']; end;

//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));

  FInitialized := TRUE;
  SELF.show;
end;

function TMediaCaptionForm.resetColor: integer;
begin
  FCaption.font.color := ST_DEFAULT_COLOR;
  result              := ST_DEFAULT_COLOR;
end;

procedure TMediaCaptionForm.setCaption(const Value: string);
begin
  case FOpInfoTimer = NIL of FALSE: FOpInfoTimer.enabled := FALSE; end; // cancel any currently running timer
  FCaption.caption      := Value;
  startOpInfoTimer;
end;

function TMediaCaptionForm.startOpInfoTimer: boolean;
begin
  case FOpInfoTimer = NIL of TRUE: FOpInfoTimer := TTimer.create(SELF); end; // SELF: if the timer is waiting to fire when we close the app, the form will free the timer
  FOpInfoTimer.enabled  := FALSE;
  FOpInfoTimer.interval := 5000;
  FOpInfoTimer.onTimer  := timerEvent;
  FOpInfoTimer.enabled  := TRUE;
end;


procedure TMediaCaptionForm.timerEvent(sender: TObject);
begin
  FOpInfoTimer.enabled := FALSE;
  FCaption.caption := '';
  FOpInfoTimer.free;
  FOpInfoTimer := NIL;
end;


function TMediaCaptionForm.toggleCaption: boolean;
begin
  FCaption.visible := NOT FCaption.visible;
end;

end.
