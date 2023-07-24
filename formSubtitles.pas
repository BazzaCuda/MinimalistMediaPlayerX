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
    function  moveOpInfo: boolean;
    function  startOpInfoTimer: boolean;
    procedure setSubtitle(const Value: string);
    procedure setDisplayTime(const Value: string);
    procedure setOpInfo(const Value: string);
    procedure timerEvent(sender: TObject);
    procedure setShowData(const Value: boolean);
    procedure setShowTime(const Value: boolean);
  public
    destructor Destroy; override;
    procedure formResize;
    function initSubtitles(aVideoPanel: TPanel): boolean;
    property dataMemo:      TMemo   read FDataMemo;
    property displayTime:   string                  write setDisplayTime;
    property HWND:          HWND    read getHWND;
    property initialized:   boolean read FInitialized;
    property opInfo:        string                  write setOpInfo;
    property showData:      boolean read FShowData  write setShowData;
    property showTime:      boolean read FShowTime  write setShowTime;
    property subTitle:      string                  write setSubtitle;
  end;

function ST: TSubtitlesForm;

implementation

uses
  mediaPlayer, commonUtils, _debugWindow;

const
  DEFAULT_WINDOW_HEIGHT = 150;

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
    aLabel.font.name      := 'Tahoma';
    aLabel.font.color     := clGray;
    aLabel.font.size      := 10;
    aLabel.margins.top    := 0;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 3;
    aLabel.caption        := '';
    aLabel.wordWrap       := FALSE;
  end;
begin
  inherited create(NIL);

//  FSubtitle := TLabel.create(NIL);
//  FSubtitle.margins.bottom := 6;

//  FSubtitlePanel := TPanel.create(NIL);
//  FSubtitlePanel.parent := SELF;
//  FSubtitle.parent := FSubtitlePanel; // the other labels don't show without this!
//  FSubtitlePanel.color := clGreen;

  SELF.height := DEFAULT_WINDOW_HEIGHT;

  FShowTime := TRUE;

  FInfoPanel := TPanel.create(NIL);
  FInfoPanel.parent := SELF;
  FInfoPanel.align  := alClient;   // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
  FInfoPanel.bevelOuter := bvNone;

  FTimeLabel := TLabel.create(FInfoPanel);
  FTimeLabel.parent := FInfoPanel;
  initTransparentLabel(FTimeLabel);
  FTimeLabel.align := alBottom; // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
  defaultFontEtc(FTimeLabel);
  FTimeLabel.alignment := taRightJustify;
  FTimeLabel.margins.bottom := 0;
  FTimeLabel.caption := '00:00:00 / 99:99:99'; // used to set initial size and position of opInfo
  FTimeLabel.autoSize := FALSE;
  FTimeLabel.top     := FInfoPanel.height - FTimeLabel.height;

  FOpInfo := TLabel.create(FInfoPanel);
  FOpInfo.parent := FInfoPanel;
  initTransparentLabel(FOpInfo);
  FOpInfo.align := alBottom; // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
  defaultFontEtc(FOpInfo);
  FOpInfo.alignment := taRightJustify;
  FOpInfo.autoSize  := FALSE;
  FOpInfo.width     := FTimeLabel.width;
  FOpInfo.top       := FTimeLabel.top - FOpInfo.height;
  formResize;

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
  FDataMemo.font.name   := 'Tahoma';
  FDataMemo.font.color  := clGray;
  FDataMemo.font.size   := 10;
  FDataMemo.font.style  := [fsBold];
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

procedure TSubTitlesForm.formResize;
begin
  SELF.height     := DEFAULT_WINDOW_HEIGHT;
  FTimeLabel.left := SELF.width - FTimeLabel.width;    // don't rely on the dimensions of FInfoPanel!
  FTimeLabel.top  := SELF.height - FTimeLabel.height;
  FOpInfo.left    := FTimeLabel.left;                  // needs to be set even though align = alBottom!
  FOpInfo.top     := FTimeLabel.top - FOpInfo.height;
//  moveOpInfo;
end;

function TSubtitlesForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TSubtitlesForm.initSubtitles(aVideoPanel: TPanel): boolean;
begin
  case FInitialized of TRUE: EXIT; end;
  FVideoPanel := aVideoPanel;

  SELF.parent := aVideoPanel;
  SELF.align  := alBottom;
  initTransparentForm(SELF);

//  FSubtitle.parent := SELF;
//  initTransparentLabel(FSubtitle);


//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));


  FInitialized := TRUE;
  SELF.show;
end;

function TSubtitlesForm.moveOpInfo: boolean;
begin
  debugClear;
  debugInteger('Window     height:', SELF.height);
  debugInteger('FInfoPanel height:', FInfoPanel.height);
  debugInteger('FTimeLabel height:', FTimeLabel.height);
  debugInteger('FOpInfo    height:', FOpInfo.height);
  debugInteger('Window      width:', SELF.width);
  debugInteger('FInfoPanel  width:', FInfoPanel.width);
  debugInteger('FTimeLabel  width:', FTimeLabel.width);
  debugInteger('FOpInfo     width:', FOpInfo.width);
  debugInteger('FInfoPanel    top:', FInfoPanel.top);
  debugInteger('FTimeLabel    top:', FTimeLabel.top);
  debugInteger('FOpInfo       top:', FOpInfo.top);
end;

procedure TSubtitlesForm.setDisplayTime(const Value: string);
begin
  case FShowTime of FALSE: EXIT; end;
  FTimeLabel.caption := Value;
end;

procedure TSubtitlesForm.setOpInfo(const Value: string);
begin
  FOpInfo.caption := Value;
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
