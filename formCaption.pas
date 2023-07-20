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
  private
    FVideoPanel: TPanel;
    FCaption: TLabel;
    FOpInfoTimer: TTimer;

    constructor create;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    procedure timerEvent(sender: TObject);
    procedure setCaption(const Value: string);
  public
    destructor Destroy; override;
    function initCaption(aVideoPanel: TPanel): boolean;
    property caption: string write setCaption;
  end;

function MC: TCaptionForm; // Media Caption

implementation

uses
  mediaPlayer, commonUtils, _debugWindow;

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

function TCaptionForm.initCaption(aVideoPanel: TPanel): boolean;
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    aLabel.font.color     := clGray;
    aLabel.font.size      := 10;
    aLabel.font.style     := [];
    aLabel.margins.top    := 0;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 0;
    aLabel.wordWrap       := FALSE;
    aLabel.caption        := '';
  end;
begin
  FVideoPanel := aVideoPanel;

  SELF.parent                 := aVideoPanel;
  initTransparentForm(SELF);
  SELF.align := alTop;

  FCaption.parent            := SELF;
  initTransparentLabel(FCaption);
  defaultFontEtc(FCaption);

  SELF.show;
end;

procedure TCaptionForm.setCaption(const Value: string);
begin
  FCaption.caption      := Value;
  FOpInfoTimer.enabled  := TRUE;
end;

procedure TCaptionForm.timerEvent(sender: TObject);
begin
  FOpInfoTimer.enabled := FALSE;
  FCaption.caption := '';
end;

procedure TCaptionForm.WMSize(var message: TWMSize);
begin
//  case FSubtitle = NIL of TRUE: EXIT; end;
//  FSubtitle.caption := '';
//  FSubtitle.caption := format('w: %d, h: %d', [FSubtitle.width, FSubtitle.height]);
end;

initialization
  gMC := NIL;

finalization
  case gMC <> NIL of TRUE: gMC.free; end;

end.
