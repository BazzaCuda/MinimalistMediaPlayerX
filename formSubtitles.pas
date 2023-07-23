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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.extCtrls, vcl.stdCtrls;

type
  TSubtitlesForm = class(TForm)
  private
    FDataMemo: TMemo;
    FSubtitle: TLabel;
    FVideoPanel: TPanel;
    FInfoPanel: TPanel;
    FInitialized: boolean;
    FSubtitlePanel: TPanel;

    FTimeLabel: TLabel;

    FOpInfo: TLabel;
    FOpInfoTimer: TTimer;
    FShowTime: boolean;
    FShowData: boolean;

    constructor create;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    function  getHWND: HWND;
    function  startOpInfoTimer: boolean;
    procedure setSubtitle(const Value: string);
    procedure setDisplayTime(const Value: string);
    procedure setOpInfo(const Value: string);
    procedure timerEvent(sender: TObject);
    procedure setShowData(const Value: boolean);
    procedure setShowTime(const Value: boolean);
  public
    destructor Destroy; override;
    function initSubtitles(aVideoPanel: TControl): boolean;
    property dataMemo:      TMemo   read FDataMemo;
    property displayTime:   string                  write setDisplayTime;
    property HWND:          HWND    read getHWND;
    property opInfo:        string                  write setOpInfo;
    property showData:      boolean read FShowData  write setShowData;
    property showTime:      boolean read FShowTime  write setShowTime;
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
    aLabel.font.name      := 'Segoe UI';
    aLabel.font.color     := clGray;
    aLabel.font.size      := 8;
    aLabel.font.style     := [];
    aLabel.margins.top    := 0;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 3;
    aLabel.caption        := '';
  end;
begin
  inherited create(NIL);
//  FSubtitle := TLabel.create(NIL);
//  FSubtitle.margins.bottom := 6;

//  FSubtitlePanel := TPanel.create(NIL);
//  FSubtitlePanel.parent := SELF;
//  FSubtitle.parent := FSubtitlePanel; // the other labels don't show without this!
//  FSubtitlePanel.color := clGreen;

  SELF.height := 150;
  // we must use align otherwise sibling controls don't get drawn over the video!

  FShowTime := TRUE;

  FInfoPanel := TPanel.create(NIL);
  FInfoPanel.parent := SELF;
  FInfoPanel.align  := alClient;
  FInfoPanel.bevelOuter := bvNone;

  FTimeLabel := TLabel.create(FInfoPanel);
  FTimeLabel.parent := FInfoPanel;
  initTransparentLabel(FTimeLabel);
  defaultFontEtc(FTimeLabel);
  FTimeLabel.align := alBottom;
  FTimeLabel.alignment := taRightJustify;
  FTimeLabel.margins.bottom := 0;
  FTimeLabel.caption := '00:00:00 / 99:99:99'; // used to set initial size and position of opInfo
  FTimeLabel.autoSize := FALSE;

  FOpInfo := TLabel.create(FInfoPanel);
  FOpInfo.parent := FInfoPanel;
  initTransparentLabel(FOpInfo);
  defaultFontEtc(FOpInfo);
  FOpInfo.alignment := taRightJustify;
  FOpInfo.autoSize  := FALSE;
  FOpInfo.width     := FTimeLabel.width;
  FOpInfo.top       := FTimeLabel.top - FTimeLabel.height;
  FOpInfo.left      := FTimeLabel.left;

  FDataMemo := TMemo.create(SELF);
  FDataMemo.parent      := SELF;
  FDataMemo.align       := alLeft;
  FDataMemo.top := FTimeLabel.top - FDataMemo.height;
  FDataMemo.left := 0;
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
  FDataMemo.lines.add('Hello');
  FDataMemo.clear;
end;

destructor TSubtitlesForm.Destroy;
begin
  case FSubtitle      <> NIL of TRUE: FSubtitle.free; end;
  case FSubtitlePanel <> NIL of TRUE: FSubtitlePanel.free; end;
  case FInfoPanel     <> NIL of TRUE: FInfoPanel.free; end;
  case FOpInfoTimer   <> NIL of TRUE: FOpInfoTimer.free; end;
  inherited;
end;

function TSubtitlesForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TSubtitlesForm.initSubtitles(aVideoPanel: TControl): boolean;
begin
  case FInitialized of TRUE: EXIT; end;
//  FVideoPanel := aVideoPanel;

  (SELF as TWinControl).parent := TWinControl(aVideoPanel);
  SELF.align  := alBottom;
  initTransparentForm(SELF);

//  FSubtitle.parent := SELF;
//  initTransparentLabel(FSubtitle);

  FInitialized := TRUE;
  SELF.show;
end;

procedure TSubtitlesForm.setDisplayTime(const Value: string);
begin
  case FShowTime of FALSE: EXIT; end;
  FTimeLabel.caption := Value;
end;

procedure TSubtitlesForm.setOpInfo(const Value: string);
begin
  FOpInfo.caption       := Value;
  FOpInfo.top := FTimeLabel.top - FOpInfo.height;
  startOpInfoTimer;
end;

procedure TSubtitlesForm.setShowData(const Value: boolean);
begin
  FShowData := Value;
  case FShowData of FALSE: FDataMemo.clear; end;
end;

procedure TSubtitlesForm.setShowTime(const Value: boolean);
begin
  FShowTime := Value;
  case FShowTime of FALSE: FTimeLabel.caption := ''; end;
end;

procedure TSubtitlesForm.setSubtitle(const Value: string);
begin
  EXIT;
  FSubtitle.caption := Value;
end;

function TSubtitlesForm.startOpInfoTimer: boolean;
begin
  case FOpInfoTimer = NIL of TRUE: FOpInfoTimer := TTimer.create(NIL); end;
  FOpInfoTimer.enabled  := FALSE;
  FOpInfoTimer.interval := 1000;
  FOpInfoTimer.onTimer  := timerEvent;
  FOpInfoTimer.enabled  := TRUE;
end;

procedure TSubtitlesForm.timerEvent(sender: TObject);
begin
  FOpInfoTimer.enabled  := FALSE;
  FOpInfo.caption       := '';
  FOpInfoTimer.free;
  FOpInfoTimer := NIL;
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
