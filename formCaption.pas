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
unit formCaption;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.extCtrls, vcl.stdCtrls;

type
  TCaptionForm = class(TForm)
  strict private
    FVideoPanel: TPanel;
    FCaption: TLabel;
    FInitialized: boolean;
    FOpInfoTimer: TTimer;
  private
    procedure timerEvent(sender: TObject);
    procedure setCaption(const Value: string);
  public
    constructor create;
    destructor Destroy; override;
    procedure formResize(Sender: TObject);
    function initCaption(const aVideoPanel: TPanel): boolean;
    property caption: string write setCaption;
  end;

function MC: TCaptionForm; // Media Caption

implementation

uses
  mediaPlayer, commonUtils, uiCtrls, _debugWindow;

var
  gMC: TCaptionForm;

function MC: TCaptionForm;
begin
  case gMC = NIL of TRUE: gMC := TCaptionForm.create; end;
  result := gMC;
end;

{$R *.dfm}

{ TCaptionForm }

constructor TCaptionForm.create;
begin
  inherited create(NIL);
  height := 80;
  FCaption := TLabel.create(NIL);

  FOpInfoTimer := TTimer.create(NIL);
  FOpInfoTimer.interval := 5000;
  FOpInfoTimer.enabled  := FALSE;
  FOpInfoTimer.onTimer  := timerEvent;
end;

destructor TCaptionForm.Destroy;
begin
  case FCaption       <> NIL of TRUE: FCaption.free; end;
  case FOpInfoTimer   <> NIL of TRUE: FOpInfoTimer.free; end;
  inherited;
end;

procedure TCaptionForm.formResize(Sender: TObject);
begin
  SELF.HEIGHT := 80;
  FCaption.caption := 'resize';
  FCaption.left := SELF.width - FCaption.width;
  FCaption.top  := SELF.height - FCaption.height;
end;

function TCaptionForm.initCaption(const aVideoPanel: TPanel): boolean;
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    aLabel.font.name      := 'Tahoma';
    aLabel.font.color     := clGray;
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
  CU.initTransparentForm(SELF);
  SELF.align := alTop;

  FCaption.parent := SELF;
  CU.initTransparentLabel(FCaption);
  defaultFontEtc(FCaption);
  FCaption.align := alTop;
  FCaption.alignment := taLeftJustify;

//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));

  FInitialized := TRUE;
  SELF.show;
end;

procedure TCaptionForm.setCaption(const Value: string);
begin
  FOpInfoTimer.enabled  := FALSE; // cancel any currently running timer
  FCaption.caption      := Value;
  FOpInfoTimer.enabled  := TRUE;
end;

procedure TCaptionForm.timerEvent(sender: TObject);
begin
  FOpInfoTimer.enabled := FALSE;
  FCaption.caption := '';
end;


initialization
  gMC := NIL;

finalization
  case gMC <> NIL of TRUE: gMC.free; end;

end.
