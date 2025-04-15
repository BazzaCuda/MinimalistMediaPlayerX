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
unit view.mmpFormMain;

interface

uses
  winApi.messages, winapi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.appEvnts, vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics,
  mmpConsts,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  viewModel.mmpVM;

type
  // we're not currently using this because of TComponent problems via TForm :(
  IUIView = interface
    ['{C3ED06CD-FB6F-403E-A8DA-228771E51166}']
    function    getViewModel: IViewModel;
    procedure   setViewModel(const aValue: IViewModel);
    property    viewModel:    IViewModel  read getViewModel write setViewModel;
  end;

  {$REGION}
  TMMPUI = class(TForm)
    applicationEvents: TApplicationEvents;
    procedure   applicationEventsMessage(var msg: tagMSG; var handled: Boolean);
    procedure   FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure   FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure   FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure   FormResize(Sender: TObject);
    procedure   FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FViewModel: IViewModel;
  protected
    procedure   FormCreate(Sender: TObject);
    procedure   WMNCHitTest       (var msg: TWMNCHitTest);  message WM_NCHITTEST;
    procedure   WMSizing          (var msg: TMessage);      message WM_SIZING;
    procedure   WMDropFiles       (var msg: TWMDropFiles);  message WM_DROPFILES;
    procedure   WMEnterSizeMove   (var msg: TMessage);      message WM_ENTERSIZEMOVE;

    procedure   WINAutoCenterOff  (var msg: TMessage);      message WIN_AUTOCENTER_OFF;
    procedure   WINCaption        (var msg: TMessage);      message WIN_CAPTION;
    procedure   WINCloseApp       (var msg: TMessage);      message WIN_CLOSEAPP;
    procedure   WINGreater        (var msg: TMessage);      message WIN_GREATER;
    procedure   WINMaxSizeOff     (var msg: TMessage);      message WIN_MAX_SIZE_OFF;
    procedure   WINControls       (var msg: TMessage);      message WIN_TOGGLE_CONTROLS;
    procedure   WINPausePlay      (var msg: TMessage);      message WIN_PAUSE_PLAY;
    procedure   WINStartOver      (var msg: TMessage);      message WIN_START_OVER;
    procedure   WINSyncMedia      (var msg: TMessage);      message WIN_SYNC_MEDIA;
    procedure   WINTab            (var msg: TMessage);      message WIN_TAB;
    procedure   WINTabTab         (var msg: TMessage);      message WIN_TABTAB;
    procedure   WINResize         (var msg: TMessage);      message WIN_RESIZE;
  public
    function    getViewModel: IViewModel;
    procedure   setViewModel(const aValue: IViewModel);
    property    viewModel:    IViewModel      read getViewModel write setViewModel;
  end;
  {$ENDREGION}

var
  MMPUI: TMMPUI;

implementation

uses
  winApi.shellApi,
  mmpDesktopUtils, mmpGlobalState, mmpKeyboardUtils,
  view.mmpFormTimeline,
  _debugWindow;

{$R *.dfm}

{ TMMPUI }

procedure TMMPUI.applicationEventsMessage(var msg: tagMSG; var handled: Boolean);
// mouse events on MPV and key events for all windows
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  case GS.showingAbout  of TRUE: EXIT; end;
  case msg.message = WM_LBUTTONDOWN of TRUE: FViewModel.onMouseDown(mbLeft, mmpShiftState, msg.pt.x, msg.pt.Y); end;
  case msg.message = WM_LBUTTONUP   of TRUE: FViewModel.onMouseUp(mbLeft, mmpShiftState, msg.pt.x, msg.pt.Y); end;
  case msg.message = WM_RBUTTONDOWN of TRUE: FViewModel.onMouseDown(mbRight, mmpShiftState, msg.pt.x, msg.pt.Y); end;
  case msg.message = WM_RBUTTONUP   of TRUE: FViewModel.onMouseUp(mbRight, mmpShiftState, msg.pt.x, msg.pt.Y); end;
  case msg.message = WM_MOUSEMOVE   of TRUE: FViewModel.onMouseMove(msg.hwnd, mmpShiftState, msg.pt.x, msg.pt.Y); end;
  case msg.message = WM_KEYDOWN     of TRUE: FViewModel.onKeyDown(WORD(msg.WParam), mmpShiftState); end;
  case msg.message = WM_KEYUP       of TRUE: FViewModel.onKeyUp(WORD(msg.WParam), mmpShiftState); end;
end;

procedure TMMPUI.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.processMessages;
//  debug('formClose');
end;

procedure TMMPUI.FormCreate(Sender: TObject);
begin
  position := poScreenCenter;
end;

procedure TMMPUI.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  case key = VK_F10 of TRUE:  FViewModel.onKeyDown(key, shift); end;
end;

procedure TMMPUI.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  case key = VK_F10 of TRUE:  FViewModel.onKeyUp(key, shift); end;
end;

procedure TMMPUI.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onMouseWheelDown(shift, mousePos, handled);
end;

procedure TMMPUI.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onMouseWheelUp(shift, mousePos, handled);
end;

procedure TMMPUI.FormResize(Sender: TObject);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onFormResize;
end;

function TMMPUI.getViewModel: IViewModel;
begin
  result := FViewModel;
end;

procedure TMMPUI.setViewModel(const aValue: IViewModel);
begin
  FViewModel := aValue;
end;

procedure TMMPUI.WINAutoCenterOff(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINAutoCenterOff(msg);
end;

procedure TMMPUI.WINCaption(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINCaption(msg);
end;

procedure TMMPUI.WINCloseApp(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINCloseApp(msg);
end;

procedure TMMPUI.WINControls(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINControls(msg);
end;

procedure TMMPUI.WINGreater(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINGreater(msg);
end;

procedure TMMPUI.WINMaxSizeOff(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINMaxSizeOff(msg);
end;

procedure TMMPUI.WINPausePlay(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINPausePlay(msg);
end;

procedure TMMPUI.WINResize(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINResize(msg);
end;

procedure TMMPUI.WINStartOver(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINStartOver(msg);
end;

procedure TMMPUI.WINSyncMedia(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINSyncMedia(msg);
end;

procedure TMMPUI.WINTab(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINTab(msg);
end;

procedure TMMPUI.WINTabTab(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWINTabTab(msg);
end;

procedure TMMPUI.WMDropFiles(var msg: TWMDropFiles);
begin
  inherited;
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWMDropFiles(msg);
end;

procedure TMMPUI.WMEnterSizeMove(var msg: TMessage);
begin
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWMEnterSizeMove(msg);
end;

procedure TMMPUI.WMNCHitTest(var msg: TWMNCHitTest);
begin
  inherited;
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onNCHitTest(msg);
end;

procedure TMMPUI.WMSizing(var msg: TMessage);
begin
  inherited;
  case FViewModel = NIL of TRUE: EXIT; end;
  FViewModel.onWMSizing(msg);
end;

end.
