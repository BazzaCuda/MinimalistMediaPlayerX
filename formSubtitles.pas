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
unit formSubtitles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.extCtrls, vcl.stdCtrls, MMFTimedTextNotifyClass;

type
  TSubtitlesForm = class(TForm)
  private
    FDataMemo: TMemo;
    FSubtitle: TLabel;
    FVideoPanel: TPanel;
    FInfoPanelL: TPanel;
    FInfoPanelR: TPanel;
    FSubtitlePanel: TPanel;

    FTimeLabel: TLabel;

    FOpInfo: TLabel;
    FOpInfoTimer: TTimer;

    constructor create;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    function  getHWND: HWND;
    procedure setSubtitle(const Value: string);
    procedure setDisplayTime(const Value: string);
    procedure setOpInfo(const Value: string);
    procedure timerEvent(sender: TObject);
  public
    destructor Destroy; override;
    function initSubtitles(aVideoPanel: TPanel): boolean;
    property dataMemo:      TMemo  read FDataMemo;
    property displayTime:   string                  write setDisplayTime;
    property HWND:          HWND   read getHWND;
    property opInfo:        string                  write setOpInfo;
    property subTitle:      string                  write setSubtitle;
  end;

function ST: TSubtitlesForm;

implementation

uses
  mediaPlayer, commonUtils, _debugWindow;

var
  gST: TSubtitlesForm;

function ST: TSubtitlesForm;
begin
  case gST = NIL of TRUE: gST := TSubtitlesForm.create; end;
  result := gST;
end;

{$R *.dfm}

{ TSubtitlesForm }

constructor TSubtitlesForm.create;
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    aLabel.font.color     := clGray;
    aLabel.font.size      := 8;
    aLabel.font.style     := [];
    aLabel.margins.top    := 0;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 0;
    aLabel.caption        := '';
  end;
begin
  inherited create(NIL);
  FSubtitle := TLabel.create(NIL);
  FSubtitle.margins.bottom := 6;

  FSubtitlePanel := TPanel.create(NIL);
  FSubtitlePanel.parent := SELF;
  FSubtitle.parent := FSubtitlePanel; // the other labels don't show without this!

  FInfoPanelL := TPanel.create(NIL);
  FInfoPanelL.parent := SELF;
  FInfoPanelL.align  := alLeft;
  FInfoPanelL.bevelOuter := bvNone;

  FInfoPanelR := TPanel.create(NIL);
  FInfoPanelR.parent := SELF;
  FInfoPanelR.align  := alRight;
  FInfoPanelR.bevelOuter := bvNone;

  FTimeLabel := TLabel.create(FInfoPanelR);
  FTimeLabel.parent := FInfoPanelR;
  initTransparentLabel(FTimeLabel);
  defaultFontEtc(FTimeLabel);
  FTimeLabel.align := alBottom;
  FTimeLabel.alignment := taRightJustify;
  FTimeLabel.margins.bottom := 0;
  FTimeLabel.caption := '00:00:00 / 99:99:99'; // used to set initial size and position of opInfo

  FOpInfo := TLabel.create(FInfoPanelR);
  FOpInfo.parent := FInfoPanelR;
  initTransparentLabel(FOpInfo);
  defaultFontEtc(FOpInfo);
  FOpInfo.alignment := taRightJustify;
  FOpInfo.autoSize  := FALSE;
  FOpInfo.width     := FTimeLabel.width;
  FOpInfo.top       := FTimeLabel.top - FTimeLabel.height;
  FOpInfo.left      := FTimeLabel.left;

  FDataMemo := TMemo.create(FInfoPanelL);
  FDataMemo.parent      := FInfoPanelL;
  FDataMemo.align       := alBottom;
  FDataMemo.bevelInner  := bvNone;
  FDataMemo.bevelOuter  := bvNone;
  FDataMemo.borderStyle := bsNone;
  FDataMemo.color       := clBlack;
  FDataMemo.height      := 110;
  FDataMemo.margins.bottom := 20; // otherwise the bottom line displays below the progressBar
  FDataMemo.readOnly    := TRUE;
  FDataMemo.tabStop     := FALSE;
  FDataMemo.font.color  := clGray;
  FDataMemo.font.height := -13;
  FDataMemo.styleElements := [];
  FDataMemo.clear;

  FOpInfoTimer := TTimer.create(NIL);
  FOpInfoTimer.interval := 1000;
  FOpInfoTimer.enabled  := FALSE;
  FOpInfoTimer.onTimer  := timerEvent;
end;

destructor TSubtitlesForm.Destroy;
begin
  case FSubtitle      <> NIL of TRUE: FSubtitle.free; end;
  case FSubtitlePanel <> NIL of TRUE: FSubtitlePanel.free; end;
  case FInfoPanelL    <> NIL of TRUE: FInfoPanelL.free; end;
  case FInfoPanelR    <> NIL of TRUE: FInfoPanelR.free; end;
  case FOpInfoTimer   <> NIL of TRUE: FOpInfoTimer.free; end;
  inherited;
end;

function TSubtitlesForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TSubtitlesForm.initSubtitles(aVideoPanel: TPanel): boolean;
begin
  FVideoPanel := aVideoPanel;

  SELF.parent                 := aVideoPanel;
  initTransparentForm(SELF);

  FSubtitle.parent            := SELF;
  initTransparentLabel(FSubtitle);

  SELF.show;
end;

procedure TSubtitlesForm.setDisplayTime(const Value: string);
begin
  FTimeLabel.caption := Value;
end;

procedure TSubtitlesForm.setOpInfo(const Value: string);
begin
  FOpInfo.caption       := Value;
  FOpInfoTimer.enabled  := TRUE;
end;

procedure TSubtitlesForm.setSubtitle(const Value: string);
begin
  FSubtitle.caption := Value;
end;

procedure TSubtitlesForm.timerEvent(sender: TObject);
begin
  FOpInfoTimer.enabled := FALSE;
  FOpInfo.caption := '';
end;

procedure TSubtitlesForm.WMSize(var message: TWMSize);
begin
//  case FSubtitle = NIL of TRUE: EXIT; end;
//  FSubtitle.caption := '';
//  FSubtitle.caption := format('w: %d, h: %d', [FSubtitle.width, FSubtitle.height]);
end;

initialization
  gST := NIL;

finalization
  case gST <> NIL of TRUE: gST.free; end;

end.
