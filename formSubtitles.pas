{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
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
    FVideoPanel: TPanel;
    FInfoPanel: TPanel;
    FInitialized: boolean;

    FTimeLabel: TLabel;

    FOpInfo: TLabel;
    FOpInfoTimer: TTimer;
    FShowTime: boolean;
    FShowData: boolean;
    FUIWidth: integer;

    constructor create;
    function  getHWND: HWND;
    function  startOpInfoTimer: boolean;
    procedure setDisplayTime(const value: string);
    procedure setOpInfo(const value: string);
    procedure timerEvent(sender: TObject);
    procedure setShowData(const value: boolean);
    procedure setShowTime(const value: boolean);
  public
    destructor Destroy; override;
    procedure  formResize(aUIWidth: integer = 0);
    function   initSubtitles(const aVideoPanel: TPanel): boolean;
    function brighter: integer;
    function color: integer;
    function darker: integer;
    function resetColor: integer;
    property dataMemo:      TMemo   read FDataMemo;
    property displayTime:   string                  write setDisplayTime;
    property HWND:          HWND    read getHWND;
    property initialized:   boolean read FInitialized;
    property opInfo:        string                  write setOpInfo;
    property showData:      boolean read FShowData  write setShowData;
    property showTime:      boolean read FShowTime  write setShowTime;
  end;

function ST: TSubtitlesForm;

implementation

uses
  mediaPlayer, commonUtils, configFile, consts, _debugWindow;

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

function TSubtitlesForm.brighter: integer;
begin
  case FTimeLabel.font.color = $FFFFFF of TRUE: EXIT; end;
  FTimeLabel.font.color   := FTimeLabel.font.color  + $010101;
  FOpInfo.font.color      := FOpInfo.font.color     + $010101;
  FDataMemo.font.color    := FDataMemo.font.color   + $010101;
  result := FTimeLabel.font.color;
end;

function TSubtitlesForm.color: integer;
begin
  result := FTimeLabel.font.color;
end;

constructor TSubtitlesForm.create;
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    aLabel.font.name      := 'Tahoma';
    aLabel.font.color     := ST_DEFAULT_COLOR;
    aLabel.font.size      := 10;
    aLabel.align          := alBottom; // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
    aLabel.alignment      := taRightJustify;
    aLabel.margins.top    := 0;
    aLabel.margins.bottom := 0;
    aLabel.margins.left   := 0;
    aLabel.margins.right  := 3;
    aLabel.caption        := '';
    aLabel.wordWrap       := FALSE;
    aLabel.font.style     := [fsBold];
    aLabel.styleElements  := [];
  end;
begin
  inherited create(NIL);

  SELF.height := DEFAULT_WINDOW_HEIGHT;

  FShowTime := TRUE;

  FInfoPanel := TPanel.create(NIL);
  FInfoPanel.parent := SELF;
  FInfoPanel.align  := alClient;   // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
  FInfoPanel.bevelOuter := bvNone;
  FInfoPanel.font.color := ST_DEFAULT_COLOR;

  FTimeLabel := TLabel.create(FInfoPanel);
  FTimeLabel.parent := FInfoPanel;
  CU.initTransparentLabel(FTimeLabel);
  defaultFontEtc(FTimeLabel);
  FTimeLabel.caption  := '00:00:00 / 99:99:99'; // used to set initial size and position of opInfo
  FTimeLabel.autoSize := FALSE;
  FTimeLabel.top      := FInfoPanel.height - FTimeLabel.height;

  FOpInfo := TLabel.create(FInfoPanel);
  FOpInfo.parent := FInfoPanel;
  CU.initTransparentLabel(FOpInfo);
  defaultFontEtc(FOpInfo);
  FOpInfo.autoSize  := FALSE;
  FOpInfo.width     := FTimeLabel.width;
  FOpInfo.top       := FTimeLabel.top - FOpInfo.height;
  formResize;

  FDataMemo := TMemo.create(SELF);
  FDataMemo.parent         := SELF;
  FDataMemo.align          := alLeft;
  FDataMemo.top            := FTimeLabel.top - FDataMemo.height;
  FDataMemo.left           := 0;
  FDataMemo.width          := 200;
  FDataMemo.wordWrap       := FALSE;
  FDataMemo.bevelInner     := bvNone;
  FDataMemo.bevelOuter     := bvNone;
  FDataMemo.borderStyle    := bsNone;
  FDataMemo.color          := clBlack;
  FDataMemo.height         := 110;
  FDataMemo.margins.bottom := 20; // otherwise the bottom line displays behind the progressBar
  FDataMemo.readOnly       := TRUE;
  FDataMemo.tabStop        := FALSE;
  FDataMemo.font.name      := 'Tahoma';
  FDataMemo.font.color     := ST_DEFAULT_COLOR;
  FDataMemo.font.size      := 10;
  FDataMemo.font.style     := [fsBold];
  FDataMemo.styleElements  := [];
  FDataMemo.clear;
  FDataMemo.visible        := FALSE;
end;

function TSubtitlesForm.darker: integer;
begin
  case FTimeLabel.font.color = $010101 of TRUE: EXIT; end;
  FTimeLabel.font.color   := FTimeLabel.font.color  - $010101;
  FOpInfo.font.color      := FOpInfo.font.color     - $010101;
  FDataMemo.font.color    := FDataMemo.font.color   - $010101;
  result := FTimeLabel.font.color;
end;

destructor TSubtitlesForm.Destroy;
begin
  case FInfoPanel     <> NIL of TRUE: FInfoPanel.free; end;
  case FOpInfoTimer   <> NIL of TRUE: FOpInfoTimer.free; end;
  inherited;
end;

procedure TSubTitlesForm.formResize(aUIWidth: integer = 0);
// align := alBottom draws the labels but not in the correct position!
begin
  SELF.height     := DEFAULT_WINDOW_HEIGHT;
  FTimeLabel.left := SELF.width - FTimeLabel.width;    // don't rely on the dimensions of FInfoPanel!
  FTimeLabel.top  := SELF.height - FTimeLabel.height;
  FOpInfo.left    := FTimeLabel.left;                  // needs to be set even though align = alBottom!
  FOpInfo.top     := FTimeLabel.top - FOpInfo.height;
  case aUIWidth <> 0 of TRUE: FUIWidth := aUIWidth; end;
end;

function TSubtitlesForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TSubtitlesForm.initSubtitles(const aVideoPanel: TPanel): boolean;
begin
  case FInitialized of TRUE: EXIT; end;
  FVideoPanel := aVideoPanel;

  SELF.parent := aVideoPanel;
  SELF.align  := alBottom;
  CU.initTransparentForm(SELF);

  case CF.asInteger['timeCaption'] <> 0 of TRUE: FTimeLabel.font.color := CF.asInteger['timeCaption']; end;
  FOpInfo.font.color      := FTimeLabel.font.color;
  FDataMemo.font.color    := FTimeLabel.font.color;

//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));

  FInitialized := TRUE;
  SELF.show;
end;

function TSubtitlesForm.resetColor: integer;
begin
  FTimeLabel.font.color   := ST_DEFAULT_COLOR;
  FOpInfo.font.color      := ST_DEFAULT_COLOR;
  FDataMemo.font.color    := ST_DEFAULT_COLOR;
  result                  := ST_DEFAULT_COLOR;
end;

procedure TSubtitlesForm.setDisplayTime(const value: string);
begin
  case FShowTime of FALSE: EXIT; end;
  FTimeLabel.caption := value;
end;

procedure TSubtitlesForm.setOpInfo(const value: string);
begin
  FOpInfo.caption := value;
  startOpInfoTimer;
end;

procedure TSubtitlesForm.setShowData(const value: boolean);
begin
  FShowData := value;
  case FShowData of FALSE: FDataMemo.clear; end;
  FDataMemo.visible := FShowData and (FUIWidth > 360);
end;

procedure TSubtitlesForm.setShowTime(const value: boolean);
begin
  FShowTime := Value;
  case FShowTime of FALSE: FTimeLabel.caption := ''; end;
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

initialization
  gST := NIL;

finalization
  case gST <> NIL of TRUE: gST.free; end;

end.
