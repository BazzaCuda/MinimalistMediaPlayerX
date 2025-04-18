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
unit view.mmpFormCaptions;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  model.mmpConfigFile;

type
  ICaptions = interface
    ['{1F91FC41-F900-499E-82D7-5BE4733A9F19}']
    function    blankInTimeCaption: boolean;
    function    blankOutTimeCaption: boolean;
    function    getCaptionsForm: TForm;
    function    initCaptions(const aVideoPanel: TPanel; const aColor: TColor): boolean;
    function    notify(const aNotice: INotice): INotice;
    function    resetTimer: boolean;
    property    captionsForm: TForm   read getCaptionsForm;
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  TCaptionsForm = class(TForm) // multiple captions at the bottom of the window
  private
    FDataMemo:      TMemo;
    FVideoPanel:    TPanel;
    FInfoPanel:     TPanel;
    FInitialized:   boolean;

    FTimeLabel:     TLabel;

    FOpInfo:        TLabel;
    FShowTime:      boolean;
    FShowData:      boolean;
    FUIWidth:       integer;

    FPLCurrentItem: string;

    function    getHWND:          HWND;
    procedure   setDisplayTime(const aValue: string);
    procedure   setOpInfo(const aValue: string);
    procedure   setShowData(const aValue: boolean);
    procedure   setShowTime(const aValue: boolean);
    function    getColor:         TColor;
    procedure   setColor(const Value: TColor);
  public
    constructor Create(aOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   formResize(const aUIWidth: integer = 0);
    function    brighter:   integer;
    function    darker:     integer;
    function    getCaptionsForm:  TForm;
    function    initCaptions(const aVideoPanel: TPanel; const aColor: TColor): boolean;
    function    resetColor: integer;
    function    toggleCaptions(const aShiftState: TShiftState): boolean;
  end;
  {$ENDREGION}

function ST(const aOwner: TForm = NIL): ICaptions;

implementation

{$R *.dfm}

uses
  mmpConsts, mmpFuncProg, mmpKeyboardUtils, mmpTickTimer,
  view.mmpThemeUtils,
  _debugWindow;

const
  DEFAULT_WINDOW_HEIGHT = 160;

type
  // There are problems if we put the interface on TCaptionsForm, so we use an intermediary
  TCaptionsProxy = class(TInterfacedObject, ICaptions)
  strict private
    FCaptionsForm:  TCaptionsForm;
    FSavedColor:    TColor;
    FSubscriber:    ISubscriber;
    FSubscriberTT:  ISubscriber;
    FTImerCount:    integer;
  private
    function    onNotify(const aNotice: INotice): INotice;
    function    onTickTimer(const aNotice: INotice): INotice;
  protected
    procedure   formResize(const aUIWidth: integer = 0);
  public
    constructor create(const aOwner: TForm);
    destructor  Destroy; override;
    function    blankInTimeCaption: boolean;
    function    blankOutTimeCaption: boolean;
    function    getCaptionsForm: TForm;
    function    initCaptions(const aVideoPanel: TPanel; const aColor: TColor): boolean;
    function    notify(const aNotice: INotice): INotice;
    function    resetTimer: boolean;
    function    setOpInfo(const aOpInfo: string): boolean;
  end;

var gST: ICaptions = NIL;
function ST(const aOwner: TForm = NIL): ICaptions;
begin
  case gST = NIL of TRUE: gST := TCaptionsProxy.create(aOwner); end;
  result := gST;
end;

{ TSubtitlesForm }

function TCaptionsForm.brighter: integer;
begin
  result                  := FTimeLabel.font.color;
  case FTimeLabel.font.color = $FFFFFF of TRUE: EXIT; end;
  FTimeLabel.font.color   := FTimeLabel.font.color  + $010101;
  FOpInfo.font.color      := FOpInfo.font.color     + $010101;
  FDataMemo.font.color    := FDataMemo.font.color   + $010101;
  result                  := FTimeLabel.font.color;
  CF[CONF_TIME_CAPTION]   := CF.toHex(result);
end;

constructor TCaptionsForm.create(aOwner: TComponent);
  function defaultFontEtc(aLabel: TLabel): boolean;
  begin
    result                := FALSE;
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
    result                := TRUE;
  end;
begin
  inherited Create(aOwner);

  SELF.height             := DEFAULT_WINDOW_HEIGHT;

  FShowTime               := TRUE;

  FInfoPanel              := TPanel.create(SELF);
  FInfoPanel.parent       := SELF;
  FInfoPanel.align        := alClient;   // WE MUST USE ALIGN OTHERWISE SIBLING CONTROLS DON'T GET DRAWN OVER THE VIDEO!!!!!!
  FInfoPanel.bevelOuter   := bvNone;
  FInfoPanel.font.color   := ST_DEFAULT_COLOR;

  FTimeLabel              := TLabel.create(FInfoPanel);
  FTimeLabel.parent       := FInfoPanel;
  mmpInitTransparentLabel(FTimeLabel);
  defaultFontEtc(FTimeLabel);
  FTimeLabel.caption      := '00:00:00 / 99:99:99'; // used to set initial size and position of opInfo
  FTimeLabel.autoSize     := FALSE;
  FTimeLabel.top          := FInfoPanel.height - FTimeLabel.height;

  FOpInfo                 := TLabel.create(FInfoPanel);
  FOpInfo.parent          := FInfoPanel;
  mmpInitTransparentLabel(FOpInfo);
  defaultFontEtc(FOpInfo);
  FOpInfo.autoSize        := FALSE;
  FOpInfo.width           := FTimeLabel.width;
  FOpInfo.top             := FTimeLabel.top - FOpInfo.height;
  formResize;

  FTimeLabel.caption      := ' ';

  FDataMemo := TMemo.create(SELF);
  FDataMemo.parent          := SELF;
  FDataMemo.align           := alLeft;
  FDataMemo.height          := 125;
  FDataMemo.top             := FTimeLabel.top - FDataMemo.height;
  FDataMemo.left            := 0;
  FDataMemo.width           := 200;
  FDataMemo.wordWrap        := FALSE;
  FDataMemo.bevelInner      := bvNone;
  FDataMemo.bevelOuter      := bvNone;
  FDataMemo.borderStyle     := bsNone;
  FDataMemo.color           := clBlack;
  FDataMemo.margins.bottom  := 20; // otherwise the bottom line displays behind the progressBar
  FDataMemo.readOnly        := TRUE;
  FDataMemo.tabStop         := FALSE;
  FDataMemo.font.name       := 'Tahoma';
  FDataMemo.font.color      := ST_DEFAULT_COLOR;
  FDataMemo.font.size       := 10;
  FDataMemo.font.style      := [fsBold];
  FDataMemo.styleElements   := [];
  FDataMemo.clear;
  FDataMemo.visible         := CF.asBoolean[CONF_SHOW_METADATA];
  FShowData                 := CF.asBoolean[CONF_SHOW_METADATA];
end;

function TCaptionsForm.darker: integer;
begin
  result                    := FTimeLabel.font.color;
  case FTimeLabel.font.color = $010101 of TRUE: EXIT; end;
  FTimeLabel.font.color     := FTimeLabel.font.color  - $010101;
  FOpInfo.font.color        := FOpInfo.font.color     - $010101;
  FDataMemo.font.color      := FDataMemo.font.color   - $010101;
  result := FTimeLabel.font.color;
  CF[CONF_TIME_CAPTION]     := CF.toHex(result);
end;

destructor TCaptionsForm.Destroy;
begin
  inherited;
end;

procedure TCaptionsForm.formResize(const aUIWidth: integer = 0);
// align := alBottom draws the labels but not in the correct position!
begin
  SELF.height     := DEFAULT_WINDOW_HEIGHT;
  FTimeLabel.left := SELF.width   - FTimeLabel.width;    // don't rely on the dimensions of FInfoPanel!
  FTimeLabel.top  := SELF.height  - FTimeLabel.height;
  FOpInfo.left    := FTimeLabel.left;                  // needs to be set even though align = alBottom!
  FOpInfo.top     := FTimeLabel.top - FOpInfo.height;
  case aUIWidth <> 0 of TRUE: FUIWidth := aUIWidth; end;
end;

function TCaptionsForm.getCaptionsForm: TForm;
begin
  result := SELF;
end;

function TCaptionsForm.getColor: TColor;
begin
  result := FTimeLabel.font.color;
end;

function TCaptionsForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TCaptionsForm.initCaptions(const aVideoPanel: TPanel; const aColor: TColor): boolean;
begin
  result := FALSE;
  case FInitialized of TRUE: EXIT; end;
  FVideoPanel := aVideoPanel;

  SELF.parent := aVideoPanel;
  SELF.align  := alBottom;
  mmpInitTransparentForm(SELF);

  case aColor <> 0 of TRUE: FTimeLabel.font.color := aColor; end;
  FOpInfo.font.color      := FTimeLabel.font.color;
  FDataMemo.font.color    := FTimeLabel.font.color;

//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));

  FInitialized := TRUE;
  SELF.show;
  result := TRUE;
end;

function TCaptionsForm.resetColor: integer;
begin
  FTimeLabel.font.color   := ST_DEFAULT_COLOR;
  FOpInfo.font.color      := ST_DEFAULT_COLOR;
  FDataMemo.font.color    := ST_DEFAULT_COLOR;
  result                  := ST_DEFAULT_COLOR;
  CF[CONF_TIME_CAPTION]   := CF.toHex(result);
end;

procedure TCaptionsForm.setColor(const Value: TColor);
begin
  FTimeLabel.font.color := value;
end;

procedure TCaptionsForm.setDisplayTime(const aValue: string);
begin
  case FShowTime of FALSE: EXIT; end;
  FTimeLabel.caption := aValue;
  FTimeLabel.repaint; // !!!
end;

procedure TCaptionsForm.setOpInfo(const aValue: string);
begin
  FOpInfo.caption := aValue;
  FOpInfo.repaint; // !!!
  gST.resetTimer;
end;

procedure TCaptionsForm.setShowData(const aValue: boolean);
begin
  FShowData := aValue;

  case FShowData of FALSE: FDataMemo.clear;
                     TRUE: mmp.cmd(evMIFillMetaData, FDataMemo); end;

  FDataMemo.visible := FShowData and (SELF.width > 336);

  case FShowData of  TRUE: CF[CONF_SHOW_METADATA] := 'yes';
                    FALSE: CF[CONF_SHOW_METADATA] := 'no'; end;
end;

procedure TCaptionsForm.setShowTime(const aValue: boolean);
begin
  FShowTime := aValue;
  case FShowTime of FALSE: FTimeLabel.caption := ''; end;
end;

function TCaptionsForm.toggleCaptions(const aShiftState: TShiftState): boolean;
begin
  result := TRUE;
  case (ssCtrl in aShiftState) and FShowTime and (NOT FShowData) of TRUE: begin setShowData(TRUE); EXIT; end;end;

  setShowTime(NOT FShowTime);

  case (ssCtrl in aShiftState) and FShowTime of  TRUE: setShowData(TRUE);
                                                FALSE: setShowData(FALSE); end;
  result := TRUE;
end;

{ TCaptionsProxy }

function TCaptionsProxy.blankInTimeCaption: boolean;
begin
  case FSavedColor = 0 of FALSE:  begin
                                    FCaptionsForm.setColor(FSavedColor);
                                    FSavedColor := 0; end;end;
end;

function TCaptionsProxy.blankOutTimeCaption: boolean;
begin
  case FSavedColor = 0 of TRUE: begin
                                 FSavedColor := FCaptionsForm.FTimeLabel.font.color;
                                 FCaptionsForm.setColor($00000000); end;end;

end;

constructor TCaptionsProxy.create(const aOwner: TForm);
begin
  inherited create;
  FCaptionsForm := TCaptionsForm.create(aOwner);
  FSubscriber   := appEvents.subscribe(newSubscriber(onNotify));
  FSubscriberTT := TT.subscribe(newSubscriber(onTickTimer));
end;

destructor TCaptionsProxy.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
  TT.unsubscribe(FSubscriberTT);
  FSubscriberTT := NIL;
  inherited;
end;

procedure TCaptionsProxy.formResize(const aUIWidth: integer);
begin
  FCaptionsForm.formResize(aUIWidth);
end;

function TCaptionsProxy.getCaptionsForm: TForm;
begin
  result := FCaptionsForm;
end;

function TCaptionsProxy.initCaptions(const aVideoPanel: TPanel; const aColor: TColor): boolean;
begin
  FCaptionsForm.initCaptions(aVideoPanel, aColor);
end;

function TCaptionsProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TCaptionsProxy.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case FCaptionsForm = NIL of TRUE: EXIT; end;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evSTOpInfo:               FCaptionsForm.setOpInfo(aNotice.text);
    evSTDisplayTime:          FCaptionsForm.setDisplayTime(aNotice.text);
    evSTDisplayXY:            FCaptionsForm.setOpInfo(format('%d x %d', [FCaptionsForm.FVideoPanel.width, FCaptionsForm.FVideoPanel.height]));
    evSTForceCaptions:        FCaptionsForm.setShowData(CF.asBoolean[CONF_SHOW_METADATA]);
    evSTToggleCaptions:       FCaptionsForm.toggleCaptions(mmpShiftState);
    evWndResize:              FCaptionsForm.formResize(600);
    evMCReshowCaption:        FCaptionsForm.setOpInfo(mmp.cmd(evPLReqFormattedItem).text);
    evSTUpdateMetaData:       FCaptionsForm.setShowData(FCaptionsForm.FShowData);
    evSTBlankInTimeCaption:   blankInTimeCaption;
    evSTBlankOutTimeCaption:  blankOutTimeCaption;
    evSTBrighter:             FCaptionsForm.brighter;
    evSTDarker:               FCaptionsForm.darker;
    evSTReset:                FCaptionsForm.resetColor;
  end;
end;

function TCaptionsProxy.onTickTimer(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case FCaptionsForm.FOpInfo.caption = '' of TRUE: EXIT; end;
  inc(FTImerCount);
  case FTimerCount < 2 of TRUE: EXIT; end;
  FCaptionsForm.setOpInfo('');
end;

function TCaptionsProxy.resetTimer: boolean;
begin
  FTImerCount := 0;
end;

function TCaptionsProxy.setOpInfo(const aOpInfo: string): boolean;
begin
  FCaptionsForm.setOpInfo(aOpInfo);
end;

initialization

finalization
  gST := NIL;
end.
