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
unit viewModel.mmpVM;

interface

uses
  winApi.messages, winApi.windows,
  system.classes,
  vcl.Controls, vcl.extCtrls, vcl.forms, vcl.menus,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  view.mmpFormThumbs, view.mmpProgressBar,
  model.mmpMediaPlayer, model.mmpPlaylist;

type
  TKeyUpEvent           = procedure(Sender: TObject; var Key: Word; Shift: TShiftState) of object;
  TMouseWheelEvent      = procedure(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean) of object;
  TWMNCHitTestMessage   = procedure(var msg: TWMNCHitTest) of object;
  TWMDropFilesMessage   = procedure(var msg: TWMDropFiles) of object;
  TWMMessage            = procedure(var msg: TMessage) of object;

  IViewModel = interface
    ['{D9293E74-93D4-4576-A5B9-66A806E6BA42}']
    function    getMediaPlayer:       IMediaPlayer;
    function    getNotifier:          INotifier;
    function    getPlaylist:          IPlaylist;
    function    getProgressBar:       IProgressBar;
    function    getVideoPanel:        TPanel;

    function    initUI(const aForm: TForm; const aVideoPanel: TPanel): boolean;
    function    showUI:               boolean;

    procedure   onFormResize;
    procedure   onKeyDown(Key: Word; Shift: TShiftState);
    procedure   onKeyUp(Key: Word; Shift: TShiftState);
    procedure   onMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseMove(aHWND: HWND; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure   onMouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure   onNCHitTest(var msg: TWMNCHitTest);
    procedure   onWMDropFiles(var msg: TWMDropFiles);
    procedure   onWMEnterSizeMove(var msg: TMessage);
    procedure   onWMSizing(var msg: TMessage);

    procedure   onWINAutoCenterOff(var msg: TMessage);
    procedure   onWINCaption(var msg: TMessage);
    procedure   onWINCloseApp(var msg: TMessage);
    procedure   onWINControls(var msg: TMessage);
    procedure   onWINGreater(var msg: TMessage);
    procedure   onWINMaxSizeOff(var msg: TMessage);
    procedure   onWINPausePlay(var msg: TMessage);
    procedure   onWinResize(var msg: TMessage);
    procedure   onWINStartOver(var msg: TMessage);
    procedure   onWINSyncMedia(var msg: TMessage);
    procedure   onWINTab(var msg: TMessage);        // tabbing with the [T] key
    procedure   onWINTabTab(var msg: TMessage);     // tabbing with the actual [Tab] key

    procedure   setMediaPlayer(const aValue: IMediaPlayer);
    procedure   setPlaylist(const aValue: IPlaylist);
    procedure   setProgressBar(const aValue: IProgressBar);

    property    mediaPlayer:           IMediaPlayer        read getMediaPlayer  write setMediaPlayer;
    property    notifier:              INotifier           read getNotifier;
    property    playlist:              IPlaylist           read getPlaylist     write setPlaylist;
    property    progressBar:           IProgressBar        read getProgressBar  write setProgressBar;
    property    videoPanel:            TPanel              read getVideoPanel;
  end;

function newViewModel: IViewModel;

implementation

uses
  winApi.shellApi,
  system.sysUtils, system.types,
  vcl.dialogs,
  mmpConsts, mmpDesktopUtils, mmpDialogs, mmpFileUtils, mmpFolderNavigation, mmpFormatting, mmpGlobalState, mmpKeyboardUtils, mmpTickTimer, mmpUtils, mmpWindowUtils,
  view.mmpFormCaptions, view.mmpFormTimeline, view.mmpKeyboard, view.mmpThemeUtils,
  viewModel.mmpKeyboardOps,
  model.mmpConfigFile, model.mmpMediaTypes, model.mmpPlaylistUtils,
  _debugWindow;

type
  TVM = class(TInterfacedObject, IViewModel)
  strict private
    FVideoPanel:            TPanel;

    FNotifier:              INotifier;

    FMPDuration:            integer;
    FMPPosition:            integer;

    FMP:                    IMediaPlayer;
    FPlaylist:              IPlaylist;
    FPB:                    IProgressBar;

    FDoubleClick:           boolean;
    FDragged:               boolean;
    FLocked:                boolean;
    FResizingWindow:        boolean;
    FSlideshowTimer:        TTimer;
    FSubscriber:            ISubscriber;
    FSubscriberTT:          ISubscriber;

    FMenu:                  TPopupMenu;
  private
    function    adjustAspectRatio:    boolean;
    function    deleteCurrentItem(const aShiftState: TShiftState): boolean;
    function    doAppClose:           boolean;
    function    doPlayNext:           boolean;
    function    doPlayPrev:           boolean;
    function    doEscapeKey:          boolean;
    function    forcedResize(const aWND: HWND; const aPt: TPoint; const X, Y: int64): boolean;
    function    keepDelete:           boolean;
    function    minimizeWindow:       boolean;
    function    moveHelp(const bCreateNew: boolean = FALSE): boolean;
    function    movePlaylist(const bCreateNew: boolean = FALSE): boolean;
    function    moveTimeline(const bCreateNew: boolean = FALSE): boolean;
    function    playNextFolder:       boolean;
    function    playPrevFolder:       boolean;
    function    playSomething(const aIx: integer): boolean;
    function    reloadPlaylist:       boolean;
    function    renameCurrentItem(const aRenameType: TRenameType): string;
    function    resizeWindow: boolean;
    function    sendOpInfo(const aOpInfo: string): boolean;
    function    setupSlideshowTimer:  boolean;
    function    showThumbnails(const aHostType: THostType = htThumbsHost): boolean;
    function    tab(const aCapsLock:  boolean; const aFactor: integer = 0): string;
    function    toggleEditMode:       boolean;
    function    toggleFiltering:      boolean;
    function    toggleFullscreen:     boolean;
    function    toggleHelp:           boolean;
    function    togglePlaylist:       boolean;
  protected
    procedure   onFormResize;
    procedure   onKeyDown(key: Word; Shift: TShiftState);
    procedure   onKeyUp(key: Word; Shift: TShiftState);
    procedure   onMouseDown(button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseMove(aHWND: HWND; shift: TShiftState; X, Y: Integer);
    procedure   onMouseUp(button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure   onMouseWheelDown(shift: TShiftState; mousePos: TPoint; var Handled: Boolean);
    procedure   onMouseWheelUp(shift: TShiftState; mousePos: TPoint; var Handled: Boolean);
    procedure   onNCHitTest(var msg: TWMNCHitTest);
    procedure   onSlideshowTimer(sender: TObject);
    procedure   onWMDropFiles(var msg: TWMDropFiles);
    procedure   onWMEnterSizeMove(var msg: TMessage);
    procedure   onWMSizing(var msg: TMessage);
    procedure   onVideoPanelDblClick(sender: TObject);

    procedure   onWINAutoCenterOff(var msg: TMessage);
    procedure   onWINCaption(var msg: TMessage);
    procedure   onWINCloseApp(var msg: TMessage);
    procedure   onWINControls(var msg: TMessage);
    procedure   onWINGreater(var msg: TMessage);
    procedure   onWINMaxSizeOff(var msg: TMessage);
    procedure   onWINPausePlay(var msg: TMessage);
    procedure   onWinResize(var msg: TMessage);
    procedure   onWINStartOver(var msg: TMessage);
    procedure   onWINSyncMedia(var msg: TMessage);
    procedure   onWINTab(var msg: TMessage);        // tabbing with the [T] key
    procedure   onWINTabTab(var msg: TMessage);     // tabbing with the actual [Tab] key

    function    onMPNotify(const aNotice: INotice):   INotice;
    function    onMPOpen(const aNotice: INotice):     boolean;
    function    onNotify(const aNotice: INotice):     INotice;
    function    onPLNotify(const aNotice: INotice):   INotice;
    function    onTickTimer(const aNotice: INotice):  INotice;

    function    getMediaPlayer:         IMediaPlayer;
    function    getNotifier:            INotifier;
    function    getPlaylist:            IPlaylist;
    function    getProgressBar:         IProgressBar;
    function    getVideoPanel:          TPanel;

    procedure   setMediaPlayer(const aValue: IMediaPlayer);
    procedure   setPlaylist(const aValue: IPlaylist);
    procedure   setProgressBar(const aValue: IProgressBar);
  public
    constructor create;
    destructor  Destroy; override;
    function    initUI(const aForm: TForm; const aVideoPanel: TPanel): boolean;
    function    showUI:                 boolean;
  end;

var gVM: IViewModel = NIL;
function newViewModel: IViewModel;
begin
  case gVM = NIL of TRUE: gVM := TVM.create; end;
  result := gVM;
end;

// Mouse Drag Control
var
  mouseDown:  Boolean;
  mouseStart: TPoint;
  rectStart:  TRect;

procedure setStartPoint(const aHWND: HWND);
begin
  GetCursorPos(mouseStart);
  getWindowRect(aHWND, rectStart);
  mouseStart.X := mouseStart.X - rectStart.left;
  mouseStart.Y := mouseStart.Y - rectStart.top;
end;

function dragUI(aHWND: HWND): boolean;
var
  newMouse: TPoint;
  wndRect: TRect;
  dx, dy: integer;
begin
  getCursorPos(newMouse);
  dx := newMouse.X - mouseStart.X;
  dy := newMouse.Y - mouseStart.Y;

  getWindowRect(aHWND, wndRect);

  result := (abs(wndRect.left - rectStart.left) > 10) or (abs(wndRect.top - rectStart.top) > 10);
  case result of TRUE: begin notifyApp(newNotice(evGSAutoCenter, FALSE)); notifyApp(newNotice(evGSMaxSize, FALSE));end;end;

  moveWindow(aHWND, dx, dy, wndRect.width, wndRect.height, FALSE);
  case result of FALSE: EXIT; end;
  notifyApp(newNotice(evVMMoveHelp));
  notifyApp(newNotice(evVMMovePlaylist));
  notifyApp(newNotice(evVMMoveTimeline));
end;

{ TVM }

function TVM.adjustAspectRatio: boolean;
begin
  FResizingWindow := TRUE;
  var vPt := mmpAdjustAspectRatio(GS.mainForm.handle, FVideoPanel.height);
  case GS.autoCenter of TRUE: mmpCenterWindow(GS.mainForm.handle, vPt); end;
  mmpSetWindowSize(GS.mainForm.handle, vPt);
  notifyApp(newNotice(evSTDisplayXY));
  FResizingWindow := FALSE;
end;

constructor TVM.create;
begin
  inherited;
  FSubscriber   := appNotifier.subscribe(newSubscriber(onNotify));
  FSubscriberTT := TT.notifier.subscribe(newSubscriber(onTickTimer));
  FMenu := TPopupMenu.create(NIL);
  var vMenuItem := TMenuItem.create(FMenu);
  vMenuItem.caption := 'Exit';
  FMenu.items.add(vMenuItem);
end;

function TVM.deleteCurrentItem(const aShiftState: TShiftState): boolean;
begin
  var vWasPlaying := (GS.mediaType in [mtAudio, mtVideo]) and notifyApp(newNotice(evMPReqPlaying)).tf;
  case vWasPlaying of TRUE: notifyApp(newNotice(evMPPausePlay)); end;

  var vCurrentItem := notifyApp(newNotice(evPLReqCurrentItem)).text;
  case vCurrentItem = '' of TRUE: EXIT; end;

  case mmpDeleteThisFile(vCurrentItem, aShiftState) of FALSE: EXIT; end;

  var vIx := notifyApp(newNotice(evPLReqCurrentIx)).integer;
  case notifyApp(newNotice(evPLDeleteIx, vIx)).tf of FALSE: EXIT; end; // unlikely

  case (ssCtrl in aShiftState) or NOT notifyApp(newNotice(evPLReqHasItems)).tf of
     TRUE: case CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] of   TRUE: case notifyApp(newNotice(evVMPlayNextFolder)).tf of  FALSE: notifyApp(newNotice(evAppClose)); end;
                                                            FALSE: notifyApp(newNotice(evAppClose)); end;
    FALSE: notifyApp(newNotice(evVMPlaySomething, vIx)); end;

  notifyApp(newNotice(evPLFormLoadBox));
end;

destructor TVM.Destroy;
begin
  TT.notifier.unsubscribe(FSubscriberTT);
  appNotifier.unsubscribe(FSubscriber);
  case FSlideshowTimer = NIL of FALSE: FSlideshowTimer.free; end;
  case FMenu = NIL of FALSE: FMenu.free; end;
  inherited;
end;

function TVM.doAppClose: boolean;
begin
  notifyApp(newNotice(evPLFormShutForm));
  notifyApp(newNotice(evHelpShutHelp));
  notifyApp(newNotice(evVMShutTimeline));
  GS.mainForm.close;
end;

function TVM.doEscapeKey: boolean;
begin
  case GS.mainForm.windowState = wsMaximized of  TRUE: notifyApp(newNotice(evVMToggleFullscreen));
                                                FALSE: notifyApp(newNotice(evAppClose)); end;
end;

function TVM.doPlayNext: boolean;
begin
  case FLocked of TRUE: EXIT; end;
  FLocked := TRUE;

  case mmpPlayNext of FALSE:
  case CF.asBoolean[CONF_NEXT_FOLDER_ON_END] of   TRUE: case playNextFolder of FALSE: notifyApp(newNotice(evAppClose)); end;
                                                 FALSE: FLocked := FALSE; end;end;
end;

function TVM.doPlayPrev: boolean;
begin
  case FLocked of TRUE: EXIT; end;
  FLocked := TRUE;

  case mmpPlayPrev of FALSE:
  case CF.asBoolean[CONF_NEXT_FOLDER_ON_END] of   TRUE: case playPrevFolder of FALSE: notifyApp(newNotice(evAppClose)); end;
                                                 FALSE: FLocked := FALSE; end;end;
end;

function TVM.forcedResize(const aWND: HWND; const aPt: TPoint; const X: int64; const Y: int64): boolean;
// X and Y are the video dimensions
// pt.x is the designated width, or...
// py.y is the designated height
var
  vRatio: double;
  vWidth, vHeight: integer;

  function adjustWidthForAspectRatio: boolean;
  begin
    vWidth := trunc(vHeight / Y * X);
  end;

  function adjustHeightForAspectRatio: boolean;
  begin
    vHeight := trunc(Y * (vWidth / X));
  end;

  function withinScreenLimits: boolean;
  begin
    result := (vWidth <= mmpScreenWidth) and (vHeight <= mmpScreenHeight);
  end;

begin
  case (X <= 0) OR (Y <= 0) of TRUE: EXIT; end;

  vWidth  := aPt.x;
  vHeight := aPt.y;

  case aPt.x <> 0 of TRUE: repeat vWidth  := vWidth  - 50; adjustHeightForAspectRatio; until withinScreenLimits; end;

  case aPt.y <> 0 of TRUE: repeat vHeight := vHeight - 50; adjustWidthForAspectRatio;  until withinScreenLimits; end;

  sendMessage(aWnd, WM_SYSCOMMAND, SC_RESTORE, 0); // in case it was minimized
  setWindowPos(aWnd, HWND_TOPMOST, 0, 0, vWidth, vHeight, SWP_NOMOVE);      // Both SWPs achieve HWND_TOP as HWND_TOP itself doesn't work.
  setWindowPos(aWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE); // resize the window. Triggers adjustAspectRatio
end;

function TVM.getMediaPlayer: IMediaPlayer;
begin
  result := FMP;
end;

function TVM.getNotifier: INotifier;
begin
  case FNotifier = NIL of TRUE: FNotifier := newNotifier; end;
  result := FNotifier;
end;

function TVM.getPlaylist: IPlaylist;
begin
  result := FPlaylist;
end;

function TVM.getProgressBar: IProgressBar;
begin
  result := FPB;
end;

function TVM.getVideoPanel: TPanel;
begin
  result := FVideoPanel;
end;

function TVM.initUI(const aForm: TForm; const aVideoPanel: TPanel): boolean;
begin
  FVideoPanel             := aVideoPanel;
  FVideoPanel.OnDblClick  := onVideoPanelDblClick;

  mmpThemeInitForm(aForm);

  case FMP = NIL of FALSE:  begin
                              FMP.notifier.subscribe(newSubscriber(onMPNotify));
                              FMP.initMediaPlayer(aForm.handle); end;end;
  GS.notify(newNotice(evGSAutoCenter, TRUE));
  GS.notify(newNotice(evGSMaxSize, TRUE));
end;

function TVM.keepDelete: boolean;
begin
  var vCurrentFolder := notifyApp(newNotice(evPLReqCurrentFolder)).text;
  case mmpKeepDelete(vCurrentFolder) of TRUE: case CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] of  TRUE: case playNextFolder of FALSE: notifyApp(newNotice(evAppClose)); end;
                                                                                              FALSE: notifyApp(newNotice(evAppClose)); end;end;
end;

function TVM.minimizeWindow: boolean;
begin
   postMessage(GS.mainForm.handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

function TVM.moveHelp(const bCreateNew: boolean = FALSE): boolean;
begin
  var vPt := FVideoPanel.ClientToScreen(point(FVideoPanel.left + FVideoPanel.width + 1, FVideoPanel.top - 2)); // screen position of the top right corner of the application window, roughly.
  var wr: TWndRec;

  wr.HWND       := GS.mainForm.handle;
  wr.pt         := vPt;
  wr.height     := FVideoPanel.height;
  wr.helpType   := htMain;
  wr.createNew  := bCreateNew;

  notifyApp(newNotice(evHelpMoveHelp, wr));
end;

function TVM.movePlaylist(const bCreateNew: boolean): boolean;
begin
  var vPt := FVideoPanel.ClientToScreen(point(FVideoPanel.left + FVideoPanel.width + 1, FVideoPanel.top - 2)); // screen position of the top right corner of the application window, roughly.
  var wr: TWndRec;

  wr.HWND       := GS.mainForm.handle;
  wr.pt         := vPt;
  wr.height     := FVideoPanel.height;
  wr.createNew  := bCreateNew;

  notifyApp(newNotice(evPLFormMove, wr));
end;

function TVM.moveTimeline(const bCreateNew: boolean = FALSE): boolean;
begin
  var vPt := FVideoPanel.ClientToScreen(point(FVideoPanel.left, FVideoPanel.height)); // screen position of the bottom left corner of the application window, roughly.
  showTimeline(vPt, FVideoPanel.width, bCreateNew);
end;

procedure TVM.onFormResize;
begin
  case FResizingWindow of TRUE: EXIT; end;
  resizeWindow;
end;

var SS: TSnapshot;
procedure TVM.onKeyDown(key: Word; shift: TShiftState);
begin
  case GS.userInput                             of TRUE: EXIT; end;
  case GS.showingTimeline and TL.validKey(key)  of TRUE: begin focusTimeline; EXIT; end;end;
  case GS.showingThumbs                         of TRUE: begin focusThumbs;   EXIT; end;end;

  SS                  := default(TSnapshot);
  SS.key              := key;
  SS.shiftState       := shift;
  SS.keyDirection     := kdDn;
  SS.keyOp            := KBProcessKeyStroke(SS);
  mmpProcessKeyOp(FMP, SS);
end;

procedure TVM.onKeyUp(key: Word; shift: TShiftState);
begin
  case SS.handled                               of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  case GS.userInput                             of TRUE: EXIT; end;
  case GS.showingTimeline and TL.validKey(key)  of TRUE: begin focusTimeline; EXIT; end;end;
  case GS.showingThumbs                         of TRUE: begin focusThumbs;   EXIT; end;end;

  SS.key              := key;
  SS.shiftState       := shift;
  SS.keyDirection     := kdUp;
  SS.keyOp            := KBProcessKeyStroke(SS);
  mmpProcessKeyOp(FMP, SS);
end;

procedure TVM.onMouseDown(button: TMouseButton; shift: TShiftState; X, Y: Integer);
begin
  case ptInRect(FVideoPanel.clientRect, FVideoPanel.ScreenToClient(point(X, Y))) of FALSE: EXIT; end;
  case button = mbLeft of TRUE: begin FDragged := FALSE; mouseDown := TRUE; setStartPoint(GS.mainForm.handle); end;end;
end;

procedure TVM.onMouseMove(aHWND: HWND; shift: TShiftState; X, Y: Integer);
begin
  screen.cursor := crDefault;
  case mouseDown and (aHWND = FVideoPanel.handle) of TRUE: FDragged := dragUI(GS.mainForm.handle); end;
end;

procedure TVM.onMouseUp(button: TMouseButton; shift: TShiftState; X, Y: Integer);
begin
  case ptInRect(FVideoPanel.clientRect, FVideoPanel.ScreenToClient(point(X, Y))) of FALSE: EXIT; end;
  case button = mbLeft of TRUE: mouseDown := FALSE; end;
//  case button = mbRight of TRUE: FMenu.Popup(X, Y); end;
end;

procedure TVM.onMouseWheelDown(shift: TShiftState; mousePos: TPoint; var handled: Boolean);
begin
  FMP.notify(newNotice(evWheelDn));
end;

procedure TVM.onMouseWheelUp(shift: TShiftState; mousePos: TPoint; var handled: Boolean);
begin
  FMP.notify(newNotice(evWheelUp));
end;

function TVM.onMPNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;

  case aNotice.event of evMPStatePlay: case GS.mediaType of mtImage: begin mmpDelay(GS.repeatDelayMs); FLocked := FALSE; end;
                                                    mtAudio,mtVideo: FLocked := FALSE; end;end;

  case aNotice.event of
    evVMMPOnOpen:   onMPOpen(aNotice);
    evMPStatePlay:  case GS.mediaType = mtImage of   TRUE: notifyApp(newNotice(evSTBlankOutTimeCaption));
                                                    FALSE: notifyApp(newNotice(evSTBlankInTimeCaption)); end;

    evMPStateEnd:   case GS.mediaType of mtAudio, mtVideo:  case GS.showingTimeline of FALSE: notifyApp(newNotice(evVMMPPlayNext)); end;
                                                  mtImage:  end; // ignore everything. Let onSlideshowTimer handle it.

    evMPDuration:   notifyApp(newNotice(evPBMax, aNotice.integer));
    evMPPosition:   notifyApp(newNotice(evPBPosition, aNotice.integer));
  end;

  case aNotice.event of
    evMPDuration:   begin
                      FMPDuration := aNotice.integer;
                      case GS.showingTimeline of TRUE: TL.max := aNotice.integer; end;end;
    evMPPosition:   begin
                      FMPPosition := aNotice.integer;
                      notifyApp(newNotice(evSTDisplayTime, mmpFormatTime(aNotice.integer) + ' / ' + mmpFormatTime(FMPDuration)));
                      case GS.showingTimeline of TRUE: TL.position := aNotice.integer; end;end;
  end;
end;

function TVM.onMPOpen(const aNotice: INotice): boolean;
begin
//  FLocked := FALSE;
end;

procedure TVM.onNCHitTest(var msg: TWMNCHitTest);
begin
  // Prevent the cursor from changing when hovering over the side edges
  case (msg.result = HTRIGHT) or (msg.result = HTLEFT) of TRUE: msg.result := HTCLIENT; end;
end;

function TVM.onNotify(const aNotice: INotice): INotice;
var msg: TMessage;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;

  case aNotice.event of
    evAppClose:             doAppClose;
    evVMArrangeAll:         mmpArrangeAll(GS.mainForm.handle);
    evVMAdjustAspectRatio:  adjustAspectRatio;
    evVMCenterWindow:       mmpCenterWindow(GS.mainForm.handle, noPoint);
    evVMDeleteCurrentItem:  deleteCurrentItem(aNotice.shiftState);
    evVMDoEscapeKey:        doEscapeKey;
    evVMKeepCurrentItem:    sendOpInfo(renameCurrentItem(rtKeep));
    evVMKeepDelete:         keepDelete;
    evVMImageInBrowser:     showThumbnails(htMPVHost);
    evVMMinimize:           minimizeWindow;
    evVMMoveHelp:           movehelp(aNotice.tf);
    evVMMovePlaylist:       movePlaylist(aNotice.tf);
    evVMMoveTimeline:       moveTimeline(aNotice.tf);
    evVMMPPlayCurrent:      mmpPlayCurrent;
    evVMMPPlayFirst:        mmpPlayFirst;
    evVMMPPlayLast:         mmpPlayLast;
    evVMMPPlayNext:         doPlayNext;
    evVMMPPlayPrev:         doPlayPrev;
    evVMPlayNextFolder:     aNotice.tf := playNextFolder;
    evVMPlayPrevFolder:     playPrevFolder;
    evVMPlaySomething:      playSomething(aNotice.integer);
    evVMRenameCurrentItem:  sendOpInfo(renameCurrentItem(rtUser));
    evVMReloadPlaylist:     reloadPlaylist;
    evVMResizeWindow:       resizeWindow;
    evVMShowThumbs:         showThumbnails(htThumbsHost);
    evVMShutTimeline:       shutTimeline;
    evVMToggleEditMode:     begin toggleEditMode; resizeWindow; end;
    evVMToggleFiltering:    toggleFiltering;
    evVMToggleHelp:         begin toggleHelp;     resizeWindow; end;
    evVMToggleFullscreen:   toggleFullscreen;
    evVMTogglePlaylist:     begin togglePlaylist; resizeWindow; end;
    evWndResize:            begin moveHelp; movePlaylist; moveTimeline; end;
  end;
end;

function TVM.onPLNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
end;

procedure TVM.onSlideshowTimer(sender: TObject);
begin
  FSlideshowTimer.enabled := NOT GS.imagesPaused; // usually because a [R]ename has paused the slideshow
  case FSlideshowTimer.enabled of FALSE: EXIT; end;
  case (GS.mediaType = mtImage) of TRUE: notifyApp(newNotice(evVMMPPlayNext)); end;
end;

function TVM.onTickTimer(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;

  case GS.showingAbout or GS.showingHelp or GS.showingPlaylist or GS.showingThumbs or GS.showingTimeline of TRUE: EXIT; end;
  screen.cursor := crNone;

  case GS.activeTasks = 0 of  FALSE: notifyApp(newNotice(evSTOpInfo, format('Shredding: %d', [GS.activeTasks]))); end;
end;

procedure TVM.onVideoPanelDblClick(sender: TObject);
begin
  FDoubleClick := TRUE;
  case FDragged of FALSE: notifyApp(newNotice(evVMToggleFullscreen)); end; // don't process button up after a drag as a click
end;

procedure TVM.onWINAutoCenterOff(var msg: TMessage);
begin
  GS.notify(newNotice(evGSAutoCenter, FALSE));
end;

procedure TVM.onWINCaption(var msg: TMessage);
begin
  notifyApp(newNotice(evMCReshowCaption));
end;

procedure TVM.onWINCloseApp(var msg: TMessage);
begin
  doAppClose;
end;

procedure TVM.onWINControls(var msg: TMessage);
begin
  notifyApp(newNotice(evSTToggleCaptions));
end;

procedure TVM.onWINGreater(var msg: TMessage);
begin
  FResizingWindow := TRUE;
  var vHeight := mmpGreaterWindow(GS.mainForm.handle, mmpShiftState);
  var vPt     := mmpCalcWindowSize(vHeight, GS.maxSize);
  case GS.autoCenter of TRUE: mmpCenterWindow(GS.mainForm.handle, vPt); end;
  mmpSetWindowSize(GS.mainForm.handle, vPt);
  moveHelp;
  movePlaylist;
  moveTimeline;
  FResizingWindow := FALSE;
end;

procedure TVM.onWINMaxSizeOff(var msg: TMessage);
begin
  GS.notify(newNotice(evGSMaxSize, FALSE));
end;

procedure TVM.onWINPausePlay(var msg: TMessage);
begin
  FMP.notify(newNotice(evMPPausePlay));
  setupSlideshowTimer;
end;

procedure TVM.onWinResize(var msg: TMessage);
begin
  notifyApp(newNotice(evPLFormShutForm));
  notifyApp(newNotice(evHelpShutHelp));
  forcedResize(GS.mainForm.handle, point(msg.WParam, msg.LParam), notifyApp(newNotice(evMPReqVideoWidth)).integer, notifyApp(newNotice(evMPReqVideoHeight)).integer);
end;

procedure TVM.onWINStartOver(var msg: TMessage);
begin
  FMP.notify(newNotice(evMPStartOver));
end;

procedure TVM.onWINSyncMedia(var msg: TMessage);
begin
  FMP.notify(newNotice(evPBClick, msg.WParam));
  sendOpInfo('Synced');
end;

procedure TVM.onWINTab(var msg: TMessage);
begin
  sendOpInfo(tab(mmpCapsLockOn));
end;

procedure TVM.onWINTabTab(var msg: TMessage);
begin
  sendOpInfo(tab(mmpCapsLockOn, -1));
end;

procedure TVM.onWMDropFiles(var msg: TWMDropFiles);
// Allow a media file to be dropped onto the window.
// The playlist will be entirely refreshed using the contents of this media file's folder.
var vFilePath: string;
begin
  inherited;
  var hDrop := msg.Drop;
  try
    var droppedFileCount := dragQueryFile(hDrop, $FFFFFFFF, nil, 0);
    for var i := 0 to pred(droppedFileCount) do begin
      var fileNameLength := dragQueryFile(hDrop, i, nil, 0);
      setLength(vFilePath, fileNameLength);
      dragQueryFile(hDrop, i, PChar(vFilePath), fileNameLength + 1);

      notifyApp(newNotice(evPLFillPlaylist, extractFilePath(vFilePath), CF.asMediaType[CONF_PLAYLIST_FORMAT]));
      notifyApp(newNotice(evPLFind, vFilePath));
      case notifyApp(newNotice(evPLReqHasItems)).tf of TRUE: notifyApp(newNotice(evVMMPPlayCurrent)); end;

      BREAK; // we only process the first file if multiple files are dropped
    end;
  finally
    dragFinish(hDrop);
  end;
  msg.result := 0;
end;

procedure TVM.onWMEnterSizeMove(var msg: TMessage);
// the user manually starts to resize the window
begin
  notifyApp(newNotice(evGSAutoCenter, FALSE));
  notifyApp(newNotice(evGSMaxSize,    FALSE));
end;

procedure TVM.onWMSizing(var msg: TMessage);
// restricts the horizontal resizing by modifying the right edge of the resizing rectangle to ensure that the window's width remains constant.
// The user can control the height of a video - the app controls the width.
var
  newRect: PRect;
begin
  inherited;
  // Prevent horizontal resizing by adjusting the rectangle's left and right edges
  newRect := PRect(msg.LParam);
  newRect^.right := newRect^.left + GS.mainForm.width;
end;

function TVM.playNextFolder: boolean;
// reload playlist from vNextFolder and play first item
var
  vNextFolder: string;
begin
  vNextFolder := mmpNextFolder(notifyApp(newNotice(evPLReqCurrentFolder)).text, nfForwards, CF.asBoolean[CONF_ALLOW_INTO_WINDOWS]);
  notifyApp(newNotice(evSTOpInfo, vNextFolder));

  case GS.imagesPaused of  TRUE: notifyApp(newNotice(evPLFillPlaylist, vNextFolder));
                          FALSE: notifyApp(newNotice(evPLFillPlaylist, vNextFolder, CF.asMediaType[CONF_PLAYLIST_FORMAT])); end;

  notifyApp(newNotice(evPLFormLoadBox));
  case notifyApp(newNotice(evPLReqHasItems)).tf of
                       TRUE: notifyApp(newNotice(evVMMPPlayCurrent));
                      FALSE: case (vNextFolder = '') of  TRUE: notifyApp(newNotice(evAppClose));
                                                        FALSE: case CF.asBoolean[CONF_NEXT_FOLDER_ON_END] of   TRUE: notifyApp(newNotice(evVMPlayNextFolder));
                                                                                                              FALSE: notifyApp(newNotice(evMPStop)); end;end;end; // if the folder is empty we want a blank screen
  result := vNextFolder <> '';
end;

function TVM.playPrevFolder: boolean;
// reload playlist from vPrevFolder and play first item
var
  vPrevFolder: string;
begin
  vPrevFolder := mmpNextFolder(notifyApp(newNotice(evPLReqCurrentFolder)).text, nfBackwards, CF.asBoolean[CONF_ALLOW_INTO_WINDOWS]);
  notifyApp(newNotice(evSTOpInfo, vPrevFolder));

  case GS.imagesPaused of  TRUE: notifyApp(newNotice(evPLFillPlaylist, vPrevFolder));
                          FALSE: notifyApp(newNotice(evPLFillPlaylist, vPrevFolder, CF.asMediaType[CONF_PLAYLIST_FORMAT])); end;

  notifyApp(newNotice(evPLFormLoadBox));
  case notifyApp(newNotice(evPLReqHasItems)).tf of
                       TRUE: notifyApp(newNotice(evVMMPPlayCurrent));
                      FALSE: case (vPrevFolder = '') of  TRUE: notifyApp(newNotice(evAppClose));
                                                        FALSE: case CF.asBoolean[CONF_NEXT_FOLDER_ON_END] of   TRUE: notifyApp(newNotice(evVMPlayPrevFolder));
                                                                                                              FALSE: notifyApp(newNotice(evMPStop)); end;end;end; // if the folder is empty we want a blank screen
  result := vPrevFolder <> '';
end;

function TVM.playSomething(const aIx: integer): boolean;
begin
  result := FALSE;
  case (aIx = 0) or notifyApp(newNotice(evPLReqIsLast)).tf of
       TRUE: notifyApp(newNotice(evVMMPPlayCurrent));  // aIx = 0 is not the same as .isFirst
      FALSE: notifyApp(newNotice(evVMMPPlayNext)); end; // ...hence, playNext
  result := TRUE;
end;

function TVM.reloadPlaylist: boolean;
begin
  result := FALSE;
  var vCurrentItem := notifyApp(newNotice(evPLReqCurrentItem)).text;
  notifyApp(newNotice(evPLFillPlaylist, notifyApp(newNotice(evPLReqCurrentFolder)).text, mtUnk));
  case notifyApp(newNotice(evPLFind, vCurrentItem)).tf of FALSE: notifyApp(newNotice(evPLFirst)); end;
  notifyApp(newNotice(evPLFormLoadBox));
  result := TRUE;
end;

function TVM.renameCurrentItem(const aRenameType: TRenameType): string;
var
  vOldName:         string;
  vNewName:         string;
  vWasPlaying:      boolean;
begin
  result := '';
  notifyApp(newNotice(evGSImagesPaused, TRUE)); // stop any running slideshow at the earliest possible
  case notifyApp(newNotice(evPLReqHasItems)).tf of FALSE: EXIT; end;
  vWasPlaying := (GS.mediaType in [mtAudio, mtVideo]) and notifyApp(newNotice(evMPReqPlaying)).tf;
  case vWasPlaying of TRUE: notifyApp(newNotice(evMPPause)); end; // otherwise we'll rename the wrong file if this one ends and the next one plays

  vOldName := notifyApp(newNotice(evPLReqCurrentItem)).text;
  case aRenameType of
    rtUser: vNewName := mmpRenameFile(vOldName);
    rtKeep: vNewName := mmpRenameFile(vOldName, '_' + mmpFileNameWithoutExtension(vOldName));
  end;

  case vWasPlaying of TRUE: notifyApp(newNotice(evMPResume)); end;

  case vNewName = vOldName of TRUE: EXIT; end;

  notifyApp(newNotice(evPLReplaceCurrentItem, vNewName));
  notifyApp(newNotice(evMCCaption, notifyApp(newNotice(evPLReqFormattedItem)).text));
  notifyApp(newNotice(evPLFormLoadBox));

  case aRenameType of
    rtUser: result := 'Renamed';
    rtKeep: result := 'Kept';
  end;
end;

function TVM.resizeWindow: boolean;
begin
  result := FALSE;
  case notifyApp(newNotice(evPLReqHasItems)).tf of FALSE: EXIT; end; // no MP dimensions with which to calculate a resize

  case FResizingWindow of TRUE: EXIT; end;
  FResizingWindow := TRUE;

  var vPt := mmpCalcWindowSize(GS.mainForm.height, GS.maxSize);
  case GS.autoCenter of TRUE: mmpCenterWindow(GS.mainForm.handle, vPt); end;
  mmpSetWindowSize(GS.mainForm.handle, vPt);

  notifyApp(newNotice(evWndResize)); // reposition the help, playlist and timeline windows

  FResizingWindow := FALSE;
  result          := TRUE;
end;

function TVM.sendOpInfo(const aOpInfo: string): boolean;
begin
  result := FALSE;
  notifyApp(newNotice(evSTOpInfo, aOpInfo));
  result := TRUE;
end;

procedure TVM.setMediaPlayer(const aValue: IMediaPlayer);
begin
  FMP := aValue;
end;

procedure TVM.setPlaylist(const aValue: IPlaylist);
begin
  FPlaylist := aValue;
end;

procedure TVM.setProgressBar(const aValue: IProgressBar);
begin
  FPB := aValue;
end;

function TVM.setupSlideshowTimer: boolean;
begin
  case FSlideshowTimer = NIL of FALSE: freeAndNIL(FSlideshowTimer); end;

  case GS.imagesPaused of FALSE:  begin
                                    FSlideshowTimer           := TTimer.create(NIL);
                                    FSlideshowTimer.interval  := GS.IDD * 1000;
                                    FSlideshowTimer.OnTimer   := onSlideshowTimer;
                                    FSlideshowTimer.enabled   := TRUE; end;end;
end;

function TVM.showThumbnails(const aHostType: THostType = htThumbsHost): boolean;
  function mainFormDimensions: TRect;
  begin
    result.top    := GS.mainForm.top;
    result.left   := GS.mainForm.left;
    result.width  := GS.mainForm.width;
    result.height := GS.mainForm.height;
  end;
begin
  notifyApp(newNotice(evHelpShutHelp));
  notifyApp(newNotice(evPLFormShutForm));
  notifyApp(newNotice(evVMShutTimeline));

  case GS.imagesPaused of FALSE: notifyApp(newNotice(evMPPausePlay)); end;
  notifyApp(newNotice(evMPPause));

  case showThumbs(FPlaylist.currentItem, mainFormDimensions, aHostType) of mrAll: EXIT; end; // showModal; user pressed Ctrl-[0]

  case lowercase(CF[CONF_EXIT_BROWSER]) = 'exitapp' of   TRUE:  notifyApp(newNotice(evAppClose));
                                                        FALSE:  begin
                                                                  GS.mainForm.show;
                                                                  setActiveWindow(GS.mainForm.handle);
                                                                  mmpCheckPlaylistItemExists(FPlaylist, FMP, CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY]); end;end;
end;

function TVM.showUI: boolean;
begin
  result := FALSE;
  GS.mainForm.show;
  result := TRUE;
end;

function TVM.tab(const aCapsLock: boolean; const aFactor: integer = 0): string;
var
  vFactor:    integer;
  vTab:       integer;
  newInfo:    string;
  vDuration:  integer;
  vPosition:  integer;
begin
  case aFactor > 0 of  TRUE: vFactor := aFactor;
                      FALSE: vFactor := 100; end;

  case aCapsLock of TRUE: vFactor := 200; end; // alt-key does the same as it can be a pain having the CapsLock key on all the time
  case ssShift in mmpShiftState of TRUE: vFactor := 50; end;

  vDuration := FMP.notify(newNotice(evMPReqDuration)).integer;
  VPosition := FMP.notify(newNotice(evMPReqPosition)).integer;

  vTab := trunc(vDuration / vFactor);
  case (vTab = 0) or (aFactor = -1) of TRUE: vTab := 1; end;

  case ssCtrl  in mmpShiftState of  TRUE: vPosition := vPosition - vTab;
                                   FALSE: vPosition := vPosition + vTab; end;

  FMP.notify(newNotice(evPBClick, vPosition));    // change MP position
  onMPNotify(newNotice(evMPPosition, vPosition)); // immediately update time display

  case aFactor = -1 of  TRUE: newInfo := 'TAB = 1s';
                       FALSE: newInfo := format('%dth = %s', [vFactor, mmpFormatSeconds(round(vDuration / vFactor))]); end;

  case ssCtrl in mmpShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                  FALSE: newInfo := '>> ' + newInfo;
  end;
  result := newInfo;
end;

function TVM.toggleEditMode: boolean;
begin
  result := FALSE;

  notifyApp(newNotice(evHelpShutHelp));
  notifyApp(newNotice(evPLFormShutForm));

  var vCurrentItem := notifyApp(newNotice(evPLReqCurrentItem)).text;
  case mmpIsEditFriendly(vCurrentItem) of FALSE: begin mmpShowOKCancelMsgDlg(vCurrentItem + #13#10#13#10
                                                                             + 'The path/filename contains a single quote and/or an ampersand.'#13#10#13#10
                                                                             + 'This will cause the Export and Join command line operations to fail.'#13#10#13#10
                                                                             + 'Rename the path/filename first.',
                                                                               mtInformation, [MBOK]);
                                                         EXIT; end;end;


  case GS.showingTimeline of  TRUE: shutTimeline;
                             FALSE: notifyApp(newNotice(evVMMoveTimeline, TRUE)); end;

  case GS.showingTimeline of TRUE: TL.initTimeline(vCurrentItem, FMPDuration); end;

  result := TRUE;
end;

function TVM.toggleFiltering: boolean;
begin
  result := FALSE;
  notifyApp(newNotice(evGSImagesPaused, NOT GS.imagesPaused));
  setupSlideshowTimer;
  case GS.imagesPaused of  TRUE: notifyApp(newNotice(evSTOpInfo, 'Playlist filtering OFF'));
                          FALSE: notifyApp(newNotice(evSTOpInfo, 'Playlist filtering ON')); end;
  result := TRUE;
end;

function TVM.toggleFullscreen: boolean;
begin
  result := FALSE;
  case GS.mainForm.windowState = wsMaximized of  TRUE:  GS.mainForm.windowState := wsNormal;
                                                FALSE:  begin
                                                          FResizingWindow := TRUE;
                                                          GS.mainForm.windowState := wsMaximized;
                                                          FResizingWindow := FALSE; end;end;
  result := TRUE;
end;

function TVM.toggleHelp: boolean;
begin
  result := FALSE;
  notifyApp(newNotice(evPLFormShutForm));
  GS.notify(newNotice(evGSShowingHelp, NOT GS.showingHelp));
  case GS.showingHelp of   TRUE: notifyApp(newNotice(evVMMoveHelp, TRUE));
                          FALSE: notifyApp(newNotice(evHelpShutHelp)); end;
  result := TRUE;
end;

function TVM.togglePlaylist: boolean;
begin
  result := FALSE;
  notifyApp(newNotice(evHelpShutHelp));
  GS.notify(newNotice(evGSShowingPlaylist, NOT GS.showingPlaylist));
  case GS.showingPlaylist of   TRUE: notifyApp(newNotice(evVMMovePlaylist, TRUE));
                              FALSE: notifyApp(newNotice(evPLFormShutForm)); end;
  result := TRUE;
end;

initialization

finalization
  gVM := NIL;

end.
