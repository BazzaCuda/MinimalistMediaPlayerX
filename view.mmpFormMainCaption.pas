{   MMP: Minimalist Media Player
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
unit view.mmpFormMainCaption;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  IMainCaption = interface
    ['{DA3C189F-C595-4EAD-B417-586DC9E30834}']
    function initCaption(const aVideoPanel: TPanel; const aColor: TColor): boolean;
    function resetTimer: boolean;
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  TMainCaptionForm = class(TForm) // single caption at the top of the window
  strict private
    FVideoPanel:  TPanel;
    FCaptionCopy: string;
    FInitialized: boolean;
    FSubscriber:  ISubscriber;
  private
    FCaption:     TLabel;
    function    reshowCaption: boolean;
    procedure   setCaption(const aValue: string);
  protected
  public
    constructor create(const aOwner: TForm);
    procedure   formResize(Sender: TObject);
    function    brighter:       integer;
    function    darker:         integer;
    function    toggleCaption:  boolean;
    function    initCaption(const aVideoPanel: TPanel; const aColor: TColor): boolean;
    function    resetColor:     integer;
  end;
  {$ENDREGION}

function MC(const aOwner: TForm = NIL): IMainCaption;

implementation

{$R *.dfm}

uses
  mmpConsts, mmpTickTimer, mmpUtils,
  view.mmpThemeUtils,
  model.mmpConfigFile,
  _debugWindow;

type

  // There are problems if we put the interface on a TForm, so we use an intermediary
  TMainCaptionProxy = class(TInterfacedObject, IMainCaption)
  strict private
    FMainCaptionForm: TMainCaptionForm;
    FTimerCount:      integer;
    function initCaption(const aVideoPanel: TPanel; const aColor: TColor): boolean;
  private
    function onNotify(const aNotice: INotice): INotice;
    function onTickTimer(const aNotice: INotice): INotice;
  public
    constructor create(const aOwner: TForm);
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
    function    resetTimer: boolean;
  end;

var gMC: IMainCaption;
function MC(const aOwner: TForm): IMainCaption;
begin
  case gMC = NIL of TRUE: gMC := TMainCaptionProxy.create(aOwner); end;
  result := gMC;
end;

{ TCaptionForm }

function TMainCaptionForm.brighter: integer;
begin
  result                := FCaption.font.color;
  case FCaption.font.color = $FFFFFF of TRUE: EXIT; end;
  FCaption.font.color   := FCaption.font.color + $010101;
  result                := FCaption.font.color;
  CF[CONF_MAIN_CAPTION] := CF.toHex(result);
end;

constructor TMainCaptionForm.create(const aOwner: TForm);
begin
  inherited create(aOwner);
  height    := 80;
  FCaption  := TLabel.create(SELF);
end;

function TMainCaptionForm.darker: integer;
begin
  result                := FCaption.font.color;
  case FCaption.font.color = $010101 of TRUE: EXIT; end;
  FCaption.font.color   := FCaption.font.color - $010101;
  result                := FCaption.font.color;
  CF[CONF_MAIN_CAPTION] := CF.toHex(result);
end;

procedure TMainCaptionForm.formResize(Sender: TObject);
begin
  SELF.HEIGHT   := 80;
  FCaption.left := SELF.width - FCaption.width;
  FCaption.top  := SELF.height - FCaption.height;
end;

function TMainCaptionForm.initCaption(const aVideoPanel: TPanel; const aColor: TColor): boolean;
  function copiedFromDFM: boolean;
  begin
    SELF.Left         := 0;
    SELF.Top          := 0;
    SELF.BorderIcons  := [];
    SELF.Caption      := 'captionForm';
    SELF.ClientHeight := 75;
    SELF.ClientWidth  := 1136;
    SELF.Color        := clGray;
    SELF.Font.Charset := DEFAULT_CHARSET;
    SELF.Font.Color   := clWindowText;
    SELF.Font.Height  := -11;
    SELF.Font.Name    := 'Tahoma';
    SELF.Font.Style   := [];
  end;

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
  copiedFromDFM;
  mmpInitTransparentForm(SELF);
  SELF.align := alTop;

  FCaption.parent        := SELF;
  mmpInitTransparentLabel(FCaption);
  defaultFontEtc(FCaption);
  FCaption.align         := alTop;
  FCaption.alignment     := taLeftJustify;
  FCaption.StyleElements := [];

  case aColor <> 0 of TRUE: FCaption.font.color := aColor; end;

//  handy for debugging
//  SetWindowLong(SELF.handle, GWL_STYLE, GetWindowLong(SELF.handle, GWL_STYLE) OR WS_CHILD OR WS_CLIPSIBLINGS {OR WS_CLIPCHILDREN} OR WS_CAPTION AND (NOT (WS_BORDER)));

  FInitialized := TRUE;
  SELF.show;
end;

function TMainCaptionForm.resetColor: integer;
begin
  FCaption.font.color   := ST_DEFAULT_COLOR;
  result                := ST_DEFAULT_COLOR;
  CF[CONF_MAIN_CAPTION] := CF.toHex(result);
end;

function TMainCaptionForm.reshowCaption: boolean;
begin
  FCaption.caption      := FCaptionCopy;
  gMC.resetTimer;
end;

procedure TMainCaptionForm.setCaption(const aValue: string);
begin
  FCaption.caption      := aValue; // show the new caption immediately
  FCaption.repaint;
  case aValue = '' of FALSE: FCaptionCopy := aValue; end;
  mmpProcessMessages;
  gMC.resetTimer;
end;

function TMainCaptionForm.toggleCaption: boolean;
begin
  FCaption.visible := NOT FCaption.visible;
end;

{ TMainCaptionProxy }

constructor TMainCaptionProxy.create(const aOwner: TForm);
begin
  inherited create;
  FMainCaptionForm := TMainCaptionForm.create(aOwner);
  appNotifier.subscribe(newSubscriber(onNotify));
  TT.notifier.subscribe(newSubscriber(onTickTimer));
end;

destructor TMainCaptionProxy.Destroy;
begin
  inherited;
end;

function TMainCaptionProxy.initCaption(const aVideoPanel: TPanel; const aColor: TColor): boolean;
begin
  FMainCaptionForm.initCaption(aVideoPanel, aColor);
end;

function TMainCaptionProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TMainCaptionProxy.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case FMainCaptionForm = NIL of TRUE: EXIT; end; // this is ok as it's a singleton and should always exist
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evWndResize:        FMainCaptionForm.formResize(NIL);
    evMCCaption:        FMainCaptionForm.setCaption(aNotice.text);
    evMCReshowCaption:  FMainCaptionForm.reshowCaption;
    evMCBrighter:       FMainCaptionForm.brighter;
    evMCDarker:         FMainCaptionForm.darker;
    evMCReset:          FMainCaptionForm.resetColor;
  end;
end;

function TMainCaptionProxy.onTickTimer(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case FMainCaptionForm.FCaption.caption = '' of TRUE: EXIT; end;
  inc(FTImerCount);
  case FTimerCount < 5 of TRUE: EXIT; end;
  FMainCaptionForm.setCaption('');
end;

function TMainCaptionProxy.resetTimer: boolean;
begin
  FTImerCount := 0;
end;

initialization
  gMC := NIL;

finalization
  gMC := NIL;

end.
