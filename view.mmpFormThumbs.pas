{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit view.mmpFormThumbs;

interface

uses
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.appEvnts, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.Forms, Vcl.ComCtrls,
  {$endif}
  MPVBasePlayer,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpAction, mmpConsts,
  view.mmpFormProgress, view.mmpFormHelpFull, view.mmpKeyboardThumbs,
  TMPVHostClass, TThumbsClass;

type
  TThumbsForm = class(TForm)
    FStatusBar: TStatusBar;
    FThumbsHost: TPanel;
    applicationEvents: TApplicationEvents;
    procedure applicationEventsHint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FStatusBarResize(Sender: TObject);
    procedure timerTimer(Sender: TObject);
    procedure FStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
    procedure FStatusBarClick(Sender: TObject);
    procedure FStatusBarMouseLeave(Sender: TObject);
    procedure FStatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormActivate(Sender: TObject);
  strict private
    mpv: IMPVBasePlayer;
    FDurationResetSpeed:      integer;
    FImageDisplayDurationMs:  integer;
    FInitialFilePath:         string;
    FInitialHost:             THostType;
    FKeyHandled:              boolean;
    FLocked:                  boolean;
    FMPVHost:                 TMPVHost;
    FPlayingSlideshow:        boolean;
    FProgressForm:            IProgressForm;
    FShowing:                 boolean;
    FSlideshowDirection:      TSlideshowDirection;
    FSubscriber:              ISubscriber;
    FThumbs:                  IThumbs;
    FTimerInterval:           integer;
  private
    procedure onInitMPV(sender: TObject);

    procedure onOpenFile(const aURL: string);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);
    function adjustAspectRatio:   TVoid;
    function autoCenter:          TVoid;
    function checkAudioVideo:     TVoid;
    function checkThumbsPerPage:  TVoid;
    function deleteCurrentItem(const aShiftState: TShiftState): TVoid;
    function imageDisplayDurationMs(const aImageDisplayDurationMs: integer): integer;
    function keepFile(const aFilePath: string): TVoid;
    function maximizeWindow:      TVoid;
    function minimizeWindow:      TVoid;
    function moveHelp(const bCreateNew: boolean = FALSE): TVoid;
    function moveHelpWindow(const bCreateNew: boolean = FALSE): TVoid;
//    function movePlaylist(const bCreateNew: boolean): boolean;
    function onNotify(const aNotice: INotice): INotice;
    function pausePlay:           TVoid;
    function playCurrentItem:     TVoid;
    function playFirst:           TVoid;
    function playLast:            TVoid;
    function playNext:            boolean;
    function playNextFolder:      boolean;
    function playPrev:            boolean;
    function playPrevFolder:      boolean;
    function processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState; const aKey: WORD): boolean;
    function reloadPlaylist:      TVoid;
    function renameFile(const aFilePath: string; const bCleanFile: boolean = FALSE): TVoid;
    function reverseSlideshow:    TVoid;
    function saveCopyFile(const aFilePath: string): TVoid;
    function saveMoveFile(const aFilePath: string; const aDstFilePath: string; const aOpText: string): TVoid;
    function saveMoveFileToFolder(const aFilePath: string; const aFolder: string; const aOpText: string; const aRecordUndo: boolean = TRUE): boolean;
    function showHost(const aHostType: THostType): TVoid;
    function showSlideshowDirection: TVoid;
    function speedDn:             TVoid;
    function speedReset:          TVoid;
    function speedUp:             TVoid;
    function takeScreenshot:      string;
    function undoMove:            string;
    function whichHost:           THostType;
    function windowSize(const aKeyOp: TKeyOp): TVoid;
  protected
    procedure beginDrag;
    procedure CreateParams(var params: TCreateParams); override;
    procedure onDoubleClick(sender: TObject);
    procedure onMouseDown(sender: TObject; button: TMouseButton; shift: TShiftState; X, Y: integer);
    procedure onMouseUp(sender: TObject; button: TMouseButton; shift: TShiftState; X, Y: integer);
    procedure onThumbClick(sender: TObject);
    procedure WMMove(var Message: TMessage); message WM_MOVE;
  public
    function initThumbnails(const aFilePath: string; const aRect: TRect; const aHostType: THostType): TVoid;
  end;

function focusThumbs: TVoid;
function showThumbs(const aFilePath: string; const aRect: TRect; const aHostType: THostType): TModalResult;

implementation

uses
  winApi.shellApi,
  system.types,
  mmpMPVProperties,
  bazCmd,
  mmpDesktopUtils, mmpDialogs, mmpFileUtils, mmpFolderNavigation, mmpGlobalState, mmpKeyboardUtils, mmpPanelCtrls, mmpPostToAllUtils, mmpShellUtils, mmpTicker,
  mmpUserFolders, mmpUtils, mmpWindowUtils,
  view.mmpFormAbout, view.mmpFormHelp, view.mmpFormPlaylist,
  model.mmpConfigFile, model.mmpMediaInfo, model.mmpMPVCtrls, model.mmpPlaylistUtils, model.mmpUndoMove, {these should be refactored away somehow}
  TStatusBarHelperClass,
  _debugWindow;

var gTF: TThumbsForm = NIL;
function showThumbs(const aFilePath: string; const aRect: TRect; const aHostType: THostType): TModalResult;
begin
  gTF := TThumbsForm.create(NIL);
  try
    gTF.initThumbnails(aFilePath, aRect, aHostType);
    mmp.cmd(evGSShowingThumbs, TRUE);
    result := gTF.showModal;
  finally
    mmp.cmd(evGSShowingThumbs, FALSE);
    gTF.free;
  end;
  mmpProcessMessages;
end;

function focusThumbs: TVoid;
begin
  case gTF = NIL of TRUE: EXIT; end;
  setForegroundWindow(gTF.handle); // so this window also receives keyboard keystrokes
end;

{$R *.dfm}

function TThumbsForm.adjustAspectRatio: TVoid;
var
  vWidth:  integer;
  vHeight: integer;

  function adjustWidthForAspectRatio: TVoid;
  begin
    vWidth := round(vHeight / mpvVideoHeight(mpv) * mpvVideoWidth(mpv));
  end;

begin
  case (mpvVideoWidth(mpv) <= 0) OR (mpvVideoHeight(mpv) <= 0) of TRUE: EXIT; end;

  mmpWndWidthHeight(SELF.handle, vWidth, vHeight);

  vHeight := FMPVHost.height;
//  debugInteger('adjustAspectRatio: vHeight', vHeight);
//  debugInteger('adjustAspectRatio: FMPVHost.height', FMPVHost.height);

  adjustWidthForAspectRatio;

  vHeight := vHeight + mmpCaptionHeight + FStatusBar.height + (mmpBorderWidth * 2);

  mmpSetWindowSize(SELF.handle, point(vWidth, vHeight));
  mmpProcessMessages;

  mmp.cmd(GS.autoCenter, autoCenter);
end;

procedure TThumbsForm.applicationEventsHint(Sender: TObject);
begin
  case length(application.hint) > 1 of
    TRUE: case application.hint[1] = '$' of TRUE: FThumbs.setPanelText(copy(application.hint, 2, MAXINT), -1, TRUE);
                                           FALSE: mmpSetPanelText(FStatusBar, pnTick, application.hint); end;end;
end;

function TThumbsForm.autoCenter: TVoid;
begin
  case GS.autoCenter of TRUE: mmpCenterWindow(SELF.handle, point(SELF.width, SELF.height)); end;
end;

procedure TThumbsForm.beginDrag;
begin
  releaseCapture; // Release any existing mouse capture
  perform(WM_SYSCOMMAND, $F012, 0); // SC_MOVE | HTCAPTION to simulate title bar drag
end;

function TThumbsForm.checkAudioVideo: TVoid;
begin
  case FThumbs.playlist.hasItems of TRUE: begin
                                            FThumbs.hasAudioVideo := mmpPlaylistFolderContains(FThumbs.playlist.currentItem, [mtAudioVideo]);
                                            mmpSetPanelOwnerDraw(FStatusBar, pnAVAV, FThumbs.hasAudioVideo);
                                            case FThumbs.hasAudioVideo of  TRUE: mmpSetPanelText(FStatusBar, pnAVAV, AUDIO_VIDEO);
                                                                          FALSE: mmpSetPanelText(FStatusBar, pnAVAV, ''); end;end;end;
end;

function TThumbsForm.checkThumbsPerPage: TVoid;
begin
//  debug('checkThumbsPerPage');
  var  vThumbsSize   := THUMB_DEFAULT_SIZE + THUMB_MARGIN;
  var  vThumbsPerRow := (FThumbsHost.width  - THUMB_MARGIN) div vThumbsSize;
  var  vThumbsPerCol := (FThumbsHost.height - THUMB_MARGIN) div vThumbsSize;
  var  vReqdWidth    := THUMB_MARGIN + (vThumbsPerRow * vThumbsSize);
  var  vReqdHeight   := THUMB_MARGIN + (vThumbsPerCol * vThumbsSize);
  var  vBlankWidth   := FThumbsHost.width  - vReqdWidth;
  var  vBlankHeight  := FThumbsHost.height - vReqdHeight;
  case vBlankWidth   >  vThumbsSize div 3 of  TRUE: SELF.width  := SELF.width + (vThumbsSize - vBlankWidth);
                                             FALSE: SELF.width  := SELF.width - vBlankWidth; end;
  case vBlankHeight  >  vThumbsSize div 3 of  TRUE: SELF.height := SELF.height + (vThumbsSize - vBlankHeight);
                                             FALSE: SELF.height := SELF.height - vBlankHeight; end;
  case (SELF.width > mmpScreenWidth) or (SELF.height > mmpScreenHeight) of TRUE: mmpGreaterWindow(SELF.handle, [ssCtrl], vThumbsSize, htThumbsHost); end;
end;

procedure TThumbsForm.CreateParams(var params: TCreateParams);
begin
  inherited;
  params.exStyle := params.exStyle OR WS_EX_APPWINDOW; // put an icon on the taskbar for the user
end;

function TThumbsForm.deleteCurrentItem(const aShiftState: TShiftState): TVoid;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  var vCheckWithUser := (ssCtrl in aShiftState) and mmpPlaylistFolderContains(FThumbs.playlist.currentItem, [mtAudioVideo]);
  case vCheckWithUser and NOT mmpUserOK('This folder also contains audio/video files'#13#10#13#10'Do you want to continue?') of TRUE: EXIT; end;
  case mmpCheckRecycleBin(FThumbs.playlist.currentItem) of FALSE: EXIT; end;

  case mmpDeleteThisFile(FThumbs.playlist.currentItem, aShiftState, FALSE, TRUE, TRUE) // mpvStop isn't an issue for images // EXPERIMENTAL - ACTUALLY IT MIGHT BE
                                          of TRUE:  begin
                                                      var vIx := FThumbs.playlist.currentIx;
                                                      FThumbs.playlist.deleteIx(FThumbs.playlist.currentIx);  // this decrements PL's FPlayIx...
                                                      case (ssCtrl in aShiftState) or (NOT FThumbs.playlist.hasItems) of
                                                         TRUE:  case CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] AND playNextFolder of FALSE: begin close; mmp.cmd(evAppClose); end;end; // shortcut logic!
                                                        FALSE:  begin
                                                                  mmp.cmd(evPLFormLoadBox);
                                                                  case (vIx = 0) or FThumbs.playlist.isLast of  TRUE: playCurrentItem; // vIx = 0 is not the same as .isFirst
                                                                                                               FALSE: playNext; end;end;end;end;end; // ...hence, playNext
end;

procedure TThumbsForm.FormActivate(Sender: TObject);
begin
//  debugInteger('formActivate SELF.height', SELF.height);
end;

procedure TThumbsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mpv           := NIL; // do this first or the user will briefly see the blank form background
  freeAndNIL(FMPVHost);
  FThumbs       := NIL;
  FProgressForm := NIL;
  mmp.cmd(evHelpShutHelp);
  appEvents.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
end;

procedure TThumbsForm.FormCreate(Sender: TObject);
begin
  FMPVHost              := TMPVHost.create(SELF);
  FMPVHost.parent       := SELF;
  FMPVHost.onDblClick   := onDoubleClick;
  FMPVHost.onOpenFile   := onOpenFile;
  FMPVHost.onMouseDown  := onMouseDown;
  FMPVHost.onMouseUp    := onMouseUp;

  mmpInitStatusBar(FStatusBar);

  mpvCreate(mpv); // if you ever create an IMediaPlayer here, you'll need to appEvents.unsubscribe it (in TMediaPlayer.destroy) when the form closes
  mpv.onInitMPV    := onInitMPV;
  mpv.OnStateChged := onStateChange;
  mpvInitPlayer(mpv, FMPVHost.handle, EMPTY, extractFilePath(paramStr(0)));  // THIS RECREATES THE INTERNAL MPV OBJECT in TMPVBasePlayer
  mpvToggleRepeat(mpv); // so that any GIFs will remain visible rather than going black after one cycle

  var vImageDisplayDuration: string;
  mpvGetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, vImageDisplayDuration);
  vImageDisplayDuration   := mmp.use<string>(vImageDisplayDuration = 'inf', IMAGE_DISPLAY_DURATION_STRING, vImageDisplayDuration); // if there's no image-display-duration= entry at all in mpv.conf, MPV defaults to 5
  FImageDisplayDurationMs := trunc(strToFloatDef(vImageDisplayDuration, IMAGE_DISPLAY_DURATION)) * MILLISECONDS;                  // if the image-display-duration= entry isn't a valid integer

  FImageDisplayDurationMs := imageDisplayDurationMs(FImageDisplayDurationMs); // let the minimalistmediaplayer.conf override mpv.conf

  FDurationResetSpeed     := FImageDisplayDurationMs;
  FSlideshowDirection     := sdForwards;
  mpvSetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, 'inf'); // get the user's duration setting, if any, then override it. MMP controls how long an image is displayed for, not MPV
  FSubscriber             := appEvents.subscribe(newSubscriber(onNotify));
end;

procedure TThumbsForm.FormResize(Sender: TObject);
begin
//  case FThumbs = NIL of TRUE:  EXIT; end;
//  case FShowing      of FALSE: EXIT; end; // ignore the initial resizing while the form starts up
//  case whichHost of htThumbsHost: playCurrentItem; end;
//  moveHelpWindow;
//
//  case whichHost of htMPVHost:    FThumbs.showDisplayDimensions(htMPVHost);
//                    htThumbsHost: FThumbs.showDisplayDimensions(htThumbsHost); end;

  TAction<TVoid>.startWith((FThumbs <> NIL) and FShowing)
                  .aside(whichHost = htThumbsHost, playCurrentItem)
                  .aside(TRUE, moveHelpWindow, FALSE)
                  .aside(whichHost = htMPVHost,     function:TVoid begin FThumbs.showDisplayDimensions(htMPVHost); end)
                  .aside(whichHost = htThumbsHost,  function:TVoid begin FThumbs.showDisplayDimensions(htThumbsHost); end)
                  .thenStop;
end;

procedure TThumbsForm.FormShow(Sender: TObject);
begin
  FThumbs := newThumbs;
  FThumbs.initThumbs(FMPVHost, FThumbsHost, FStatusBar);
  FThumbs.onThumbClick := onThumbClick;

  FProgressForm := mmpNewProgressForm;
  try
    FProgressForm.modal               := FALSE;
    FProgressForm.buttons             := FALSE;
    FProgressForm.heading             := 'MMP Image & Thumbnail Browser';
    case FInitialHost = htThumbsHost of  TRUE: FProgressForm.subHeading := 'Creating Thumbnails';
                                        FALSE: FProgressForm.subHeading := 'Opening'; end;
    case FInitialHost = htThumbsHost of  TRUE: FProgressForm.formShow; end;
    mmpProcessMessages;
    mmpSetWindowTopmost(FProgressForm.handle);
    FProgressForm.timer.interval  := FTimerInterval;
    FProgressForm.timer.onTimer   := timerTimer;
    FProgressForm.timer.enabled   := TRUE;
  finally
  end;

  mmpSetWindowTop(SELF.handle);

  case FInitialHost of htThumbsHost: checkThumbsPerPage; end; // EXPERIMENTAL it was calling checkThumbsPerPage regardless
  autoCenter;                                                 // EXPERIMENTAL and it wasn't calling this at all
end;

function TThumbsForm.imageDisplayDurationMs(const aImageDisplayDurationMs: integer): integer;
begin
  case CF.asInteger[CONF_SLIDESHOW_INTERVAL_MS] <> 0 of  TRUE: result := CF.asInteger[CONF_SLIDESHOW_INTERVAL_MS];
                                                        FALSE: result := aImageDisplayDurationMs; end;
end;

function TThumbsForm.initThumbnails(const aFilePath: string; const aRect: TRect; const aHostType: THostType): TVoid;
begin
  FInitialFilePath := aFilePath;

  SELF.top    := aRect.top;
  SELF.left   := aRect.left;
  SELF.width  := aRect.width;
  SELF.height := aRect.height;

//  debugInteger('initThumbails aRect.height', aRect.height);

  case GS.autoCenter of TRUE: autoCenter; end;

  SELF.borderIcons    := [biSystemMenu, biMaximize];
  SELF.borderStyle    := bsSizeable;
  SELF.borderWidth    := 0;
  SELF.color          := clBlack;

  FMPVHost.align      := alClient;
  FThumbsHost.align   := alClient;

  FInitialHost        := aHostType;
  FMPVHost.visible    := FInitialHost = htMPVHost;
  FThumbsHost.visible := FInitialHost = htThumbsHost;

  case FInitialHost of     htMPVHost: FTimerInterval := 100;
                        htThumbsHost: FTimerInterval := 250; end;

  FThumbsHost.styleElements  := [];
  FThumbsHost.bevelOuter     := bvNone;
  FThumbsHost.color          := clBlack;
end;


function TThumbsForm.keepFile(const aFilePath: string): TVoid;
var
  vNewName: string;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  vNewName := mmpRenameFile(aFilePath, '! ' + mmpFileNameWithoutExtension(aFilePath));
  case vNewName <> aFilePath of  TRUE: begin
                                        FThumbs.playlist.replaceCurrentItem(vNewName);
                                        mmpSetPanelText(FStatusBar, pnHelp, 'Kept');
                                        mmpSetPanelText(FStatusBar, pnName, extractFileName(FThumbs.playlist.currentItem)); end;
                                FALSE:  mmpSetPanelText(FStatusBar, pnHelp, 'NOT Kept'); end;
end;

function TThumbsForm.maximizeWindow: TVoid;
// maximize to the largest size given the screen height,
// while maintaing the correct aspect ratio, width-wise, for the image dimensions
var
  vWidth:  integer;
  vHeight: integer;
begin
  case (MI.imageWidth <= 0) OR (MI.imageHeight <= 0) of TRUE: EXIT; end;

  vHeight := mmpScreenHeight - 20; // EXPERIMENTAL WAS 50: this now matches the 20 in mmpWindowUtils.mmpCalcWindowSize

  vWidth  := trunc(vHeight / MI.imageHeight * MI.imageWidth);

  mmpSetWindowPos(SELF.Handle, point((mmpScreenWidth - vWidth) div 2, (mmpScreenHeight - vHeight) div 2)); // center window
  mmpProcessMessages;
  mmpSetWindowSize(SELF.Handle, point(vWidth, vHeight)); // resize window
end;

function TThumbsForm.minimizeWindow: TVoid;
begin
  SELF.windowState := TWindowState.wsMinimized;
end;

function TThumbsForm.moveHelp(const bCreateNew: boolean = FALSE): TVoid;
var vHostPanel: TPanel;
begin
  vHostPanel := NIL; // suppress compiler warning
  case whichHost of htMPVHost:    vHostPanel := FMPVHost;
                    htThumbsHost: vHostPanel := FThumbsHost; end;

  var vPt := FThumbsHost.ClientToScreen(point(vHostPanel.left + vHostPanel.width + 1, vHostPanel.top - 2 - mmpCaptionHeight - mmpBorderWidth)); // screen position of the top right corner of the application window, roughly.
  var wr: TWndRec;

  wr.HWND       := GS.mainForm.handle;
  wr.pt         := vPt;
  wr.height     := SELF.height;
  wr.helpType   := htIATB;
  wr.createNew  := bCreateNew;

  mmp.cmd(evHelpMoveHelp, wr);

//  debugInteger('moveHelp SELF.height', SELF.height);
  // FormResize calls moveHelp so this will get called repeatedly until both windows fit the desktop
  case mmpWithinScreenLimits(SELF.width + GS.widthHelp, SELF.height) of FALSE:  begin
                                                                                  mmpGreaterWindow(SELF.handle, [ssCtrl], FThumbs.thumbSize, whichHost); // ssCtrl makes the window smaller
                                                                                  mmpCenterWindow(SELF.handle, point(SELF.width, SELF.height)); end;end; // ignore GS.autoCenter
end;

function TThumbsForm.moveHelpWindow(const bCreateNew: boolean = FALSE): TVoid;
begin
  case FThumbs = NIL of  TRUE: EXIT; end;
  case FShowing      of FALSE: EXIT; end; // ignore the initial resizing while the form starts up

  moveHelp(bCreateNew);
end;

//function TThumbsForm.movePlaylist(const bCreateNew: boolean): boolean;
//var vHostPanel: TPanel;
//begin
//  case whichHost of htMPVHost:    vHostPanel := FMPVHost;
//                    htThumbsHost: vHostPanel := FThumbsHost; end;
//
//  var vPt := FThumbsHost.ClientToScreen(point(vHostPanel.left + vHostPanel.width + 1, vHostPanel.top - 2)); // screen position of the top right corner of the application window, roughly.
//  var wr: TWndRec;
//
//  wr.HWND       := GS.mainForm.handle;
//  wr.pt         := vPt;
//  wr.height     := SELF.height;
//  wr.createNew  := bCreateNew;
//
//  mmp.cmd(evPLFormMove, wr);
//end;

procedure TThumbsForm.onDoubleClick(sender: TObject);
begin
  SELF.windowState := mmp.use<TWindowState>(SELF.windowState = wsMaximized, wsNormal, wsMaximized);
end;

procedure TThumbsForm.onMouseDown(sender: TObject; button: TMouseButton; shift: TShiftState; X, Y: integer);
begin
  case button = mbLeft of FALSE: EXIT; end;
  case whichHost = htMPVHost of FALSE: EXIT; end;
  beginDrag;
end;

procedure TThumbsForm.onMouseUp(sender: TObject; button: TMouseButton; shift: TShiftState; X, Y: integer);
begin
//  case button = mbRight of TRUE:  case ssCtrl in shift of  TRUE:  processKeyOp(koReverseSlideshow, [], 0);
//                                                          FALSE:  processKeyOp(koPausePlay, [], 0); end;end;
end;

function TThumbsForm.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case GS.activeTasks = 0 of TRUE: EXIT; end;
  mmp.cmd(aNotice.event = evGSActiveTasks, procedure begin mmpSetPanelText(FStatusBar, pnHelp, format('Shredding: %d', [GS.activeTasks])); end);
end;

procedure TThumbsForm.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(mpv, extractFilePath(paramStr(0)));
end;

procedure TThumbsForm.onOpenFile(const aURL: string);
begin
  tickerStart;
  try
    mpvOpenFile(mpv, aURL); // don't blink!
    mmpProcessMessages;
  except end;
  tickerStop;

  showHost(htMPVHost);

  FThumbs.setPanelText(aURL, tickerTotalMs, TRUE);
end;

procedure TThumbsForm.onStateChange(cSender: TObject; eState: TMPVPlayerState);
begin
  case  FPlayingSlideshow of FALSE:
        case eState of mpsPlay: begin case FLocked of TRUE: mmpDelay(GS.repeatDelayMs); end; FLocked := FALSE; EXIT; end;end;end;

  case FPlayingSlideshow of TRUE:
  case eState of
    mpsPlay,
    mpsEnd: begin FLocked := FALSE; // we don't always get an mpsEnd event!
              case FPlayingSlideshow of TRUE: begin mmpDelay(FImageDisplayDurationMs);
                                                    case FPlayingSlideshow of // still playing slideshow after the delay?
                                                                              TRUE: case FSlideshowDirection of
                                                                                      sdForwards:  case playNext of FALSE: playFirst; end;
                                                                                      sdBackwards: case playPrev of FALSE: playLast;  end;
                                                    end;end;end;end;end;end;end;
  mmpProcessMessages;
end;

procedure TThumbsForm.onThumbClick(sender: TObject);
begin
  FThumbs.cancel;
  mmpResetPanelHelp(FStatusBar);
  showHost(htMPVHost);
  FThumbs.playlist.setIx(TControl(sender).tag);
  playCurrentItem;
end;

function TThumbsForm.pausePlay: TVoid;
begin
  FPlayingSlideshow := NOT FPlayingSlideshow;

//  FImageDisplayDurationMs := imageDisplayDurationMs(FImageDisplayDurationMs); // has the user changed .conf since the last pause/play - EXPERIMENTAL: WHAT!??

  case NOT FPlayingSlideshow of TRUE: mmpCancelDelay; end;

  case FPlayingSlideshow of  TRUE: mpvSetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, intToStr(FImageDisplayDurationMs div MILLISECONDS)); // doesn't really matter as long as it's valid and not 'inf'
                            FALSE: mpvSetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, 'inf'); end;

  case FPlayingSlideshow of  TRUE: showSlideshowDirection;
                            FALSE: mmpSetPanelText(FStatusBar, pnHelp, 'PAUSED'); end;

  case FPlayingSlideshow of  TRUE: FThumbs.playCurrentItem; end;
end;

function TThumbsForm.playCurrentItem: TVoid;
begin
  case whichHost of
    htMPVHost:    FThumbs.playCurrentItem;
    htThumbsHost: FThumbs.playThumbs;
  end;
end;

function TThumbsForm.playFirst: TVoid;
begin
  FThumbs.playlist.first;
  case whichHost of
    htMPVHost:    playCurrentItem;
    htThumbsHost: playCurrentItem;
  end;
end;

function TThumbsForm.playLast: TVoid;
begin
  FThumbs.playlist.last;
  case whichHost of
    htMPVHost:    playCurrentItem;
    htThumbsHost: playCurrentItem;
  end;
end;

function TThumbsForm.playNext: boolean;
begin
  result := TRUE;
  case whichHost of
    htMPVHost:    begin case FLocked of TRUE: EXIT; end;
                        FLocked := TRUE;
                        result := FThumbs.playlist.next; // NB: this ignores the two optional parameters
                        playCurrentItem; end;
    htThumbsHost: playCurrentItem;
  end;
end;

function TThumbsForm.playNextFolder: boolean;
var
  vNextFolder: string;
begin
  result := FALSE;
  mmpSetPanelOwnerDraw(FStatusBar, pnAVAV, FALSE);

  vNextFolder := mmpNextFolder(FThumbs.currentFolder, nfForwards, CF.asBoolean[CONF_ALLOW_INTO_WINDOWS]);
  mpvStop(mpv); // if the folder is empty we want a blank screen
  case vNextFolder = EMPTY of TRUE: EXIT; end;
  mmp.cmd(vNextFolder <> EMPTY, procedure begin FThumbs.playThumbs(vNextFolder + '$$$.$$$', ptPlaylistOnly); end); // because extractFilePath needs a file name ;)

  checkAudioVideo;

  case FThumbs.playlist.hasItems of  TRUE: playCurrentItem;
                                    FALSE: playNextFolder; end; // find a folder with images
  result := FThumbs.playlist.hasItems;
end;

function TThumbsForm.playPrev: boolean;
begin
  result := FALSE;
  case whichHost of
    htMPVHost:    begin case FLocked of TRUE: EXIT; end;
                        FLocked := TRUE;
                        result  := FThumbs.playlist.prev;
                        playCurrentItem; end;
    htThumbsHost: case FThumbs.playlist.currentIx = FThumbs.thumbsPerPage of FALSE: FThumbs.playPrevThumbsPage; end;
  end;
end;

function TThumbsForm.playPrevFolder: boolean;
var
  vPrevFolder: string;
begin
  result := FALSE;
  mmpSetPanelOwnerDraw(FStatusBar, pnAVAV, FALSE);

  vPrevFolder := mmpNextFolder(FThumbs.currentFolder, nfBackwards, CF.asBoolean[CONF_ALLOW_INTO_WINDOWS]);
  mpvStop(mpv); // if the folder is empty we want a blank screen
  case vPrevFolder = EMPTY of TRUE: EXIT; end;
  mmp.cmd(vPrevFolder <> EMPTY, procedure begin FThumbs.playThumbs(vPrevFolder + '$$$.$$$', ptPlaylistOnly); end); // because extractFilePath needs a file name ;)

  checkAudioVideo;

  case FThumbs.playlist.hasItems of  TRUE: playCurrentItem;
                                    FALSE: playPrevFolder; end; // find a folder with images
  result := FThumbs.playlist.hasItems;
end;

function TThumbsForm.reloadPlaylist: TVoid;
begin
  var vCurrentItem := FThumbs.playlist.currentItem;
  FThumbs.playlist.clear;
  FThumbs.fillPlaylist(FThumbs.playlist, vCurrentItem, extractFilePath(vCurrentItem));
  case FThumbs.playlist.find(vCurrentItem) of  TRUE: playCurrentItem;
                                              FALSE: playFirst; end;
  mmpSetPanelText(FStatusBar, pnFold, format('Reloaded: %s', [extractFilePath(vCurrentItem)]));
end;

function TThumbsForm.showHost(const aHostType: THostType): TVoid;
begin
  FMPVHost.visible    := aHostType = htMPVHost;
  FThumbsHost.visible := aHostType = htThumbsHost;
end;

function TThumbsForm.renameFile(const aFilePath: string; const bCleanFile: boolean = FALSE): TVoid;
var
  vNewName: string;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  case bCleanFile of   TRUE: vNewName := mmpRenameFile(aFilePath, mmpCleanFile(mmpFileNameWithoutExtension(aFilePath)));
                      FALSE: vNewName := mmpRenameFile(aFilePath); end;

  mmp.cmd(vNewName <> aFilePath,  procedure begin
                                            FThumbs.playlist.replaceCurrentItem(vNewName);
                                            mmpSetPanelText(FStatusBar, pnFold, 'Renamed: ' + aFilePath); end);
  mmpSetPanelText(FStatusBar, pnName, extractFileName(vNewName));
end;

function TThumbsForm.reverseSlideshow: TVoid;
begin
  FSlideshowDirection := TSlideshowDirection(1 - ord(FSlideshowDirection)); // x := 1 - x
  showSlideshowDirection;
  mmp.cmd(NOT FPlayingSlideshow, pausePlay);
end;

function TThumbsForm.saveCopyFile(const aFilePath: string): TVoid;
begin
  case mmpCopyFile(aFilePath, mmpUserDstFolder('Copied'), FALSE, FALSE) of
                                                   TRUE:  begin
                                                            mmpSetPanelText(FStatusBar, pnHelp, 'Copied');
                                                            mmpSetPanelText(FStatusBar, pnFold, 'Copied to: ' + mmpUserDstFolder('Copied'));
                                                          end;
                                                  FALSE:  begin
                                                            mmpSetPanelOwnerDraw(FStatusBar, pnHelp, TRUE);
                                                            mmpSetPanelText(FStatusBar, pnHelp, 'NOT Copied');
                                                            end;end;
end;

function TThumbsForm.saveMoveFile(const aFilePath: string; const aDstFilePath: string; const aOpText: string): TVoid;
begin
  case saveMoveFileToFolder(aFilePath, aDstFilePath, aOpText) of FALSE: EXIT; end;
//  FThumbs.playlist.deleteIx(FThumbs.playlist.currentIx);
  FThumbs.playlist.deleteIx(-1); // EXPERIMENTAL - after rewriting IPlaylist.deleteIx, passing -1 to delete the current index now seems to be the preferred method
  case FThumbs.playlist.hasItems of FALSE:  begin mpvStop(mpv); mmpPartClearStatusBar(FStatusBar);
                                              case CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] of  TRUE: case playNextFolder of FALSE: begin close; mmp.cmd(evAppClose); end;end;
                                                                                              FALSE: begin close; mmp.cmd(evAppClose); end;
                                              end;
                                            end;
                                     TRUE:  begin
                                              FThumbs.playlist.nextIx; // because playlist.deleteIx decrements FPlayIx
                                              playCurrentItem;
                                            end;end;
end;

function TThumbsForm.saveMoveFileToFolder(const aFilePath: string; const aFolder: string; const aOpText: string; const aRecordUndo: boolean = TRUE): boolean;
begin
  result := mmpCopyFile(aFilePath, aFolder, TRUE, aRecordUndo);
  case result of  TRUE: begin
                          mmpSetPanelText(FStatusBar, pnHelp, aOpText);
                          mmpSetPanelText(FStatusBar, pnFold, aOpText + ' to: ' + aFolder);
                          FThumbs.foldPanelReserved := TRUE;
                        end;
                FALSE:  begin
                          mmpSetPanelOwnerDraw(FStatusBar, pnHelp, TRUE);
                          mmpSetPanelText(FStatusBar, pnHelp, 'NOT ' + aOpText);
                        end;end;
end;

function TThumbsForm.showSlideshowDirection: TVoid;
begin
  case FSlideshowDirection of
    sdForwards:   mmpSetPanelText(FStatusBar, pnHelp, 'Slideshow ->');
    sdBackwards:  mmpSetPanelText(FStatusBar, pnHelp, '<- Slideshow'); end;
end;

function TThumbsForm.speedDn: TVoid;
begin
  FImageDisplayDurationMs := FImageDisplayDurationMs + SLIDESHOW_DELTA_MS;
  mmpSetPanelText(FStatusBar, pnHelp, format('%dms', [FImageDisplayDurationMs]));
end;

function TThumbsForm.speedReset: TVoid;
begin
  FImageDisplayDurationMs := FDurationResetSpeed;
  mmpSetPanelText(FStatusBar, pnHelp, format('%dms', [FImageDisplayDurationMs]));
end;

function TThumbsForm.speedUp: TVoid;
begin
  case FImageDisplayDurationMs = SLIDESHOW_DELTA_MS of TRUE: EXIT; end;// delta doubles as minimum interval/fastest speed
  FImageDisplayDurationMs := FImageDisplayDurationMs - SLIDESHOW_DELTA_MS;
  mmpSetPanelText(FStatusBar, pnHelp, format('%dms', [FImageDisplayDurationMs]));
end;

procedure TThumbsForm.FStatusBarClick(Sender: TObject);
begin
  case mmpIsPanelAt(FStatusBar, PANEL_FOLD, mmpMousePoint(FStatusBar)) of TRUE: mmpShellExec(GS.mainForm.Handle, FThumbs.currentFolder); end;
  case mmpIsPanelAt(FStatusBar, PANEL_AVAV, mmpMousePoint(FStatusBar)) of TRUE: processKeyOp(koCloseToMain, [], 0);  end;
end;

procedure TThumbsForm.FStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel; const Rect: TRect);
// called for pnHelp when it has to highlight its contents, e.g. "NOT Moved".
// called for pnAVAV when there are audio and/or video files in the folder
begin
  statusBar.canvas.font.style   := [fsBold];
  statusBar.canvas.brush.color  := clMaroon;
  var vRect := rect;
  vRect.left   := vRect.left + 2;   // leave room for the panel separators
  vRect.right  := vRect.right - 4;  // leave room for the panel separators
  statusBar.canvas.fillRect(vRect); // fill with color leaving room for the panel separators either side
  var vCenterX: integer := (rect.right  - rect.left - statusBar.canvas.textWidth(panel.text)) div 2;
  var vCenterY: integer := (rect.bottom - rect.top  - statusBar.canvas.textHeight(panel.text)) div 2;
  textOut(statusBar.canvas.handle, rect.left + vCenterX, rect.top + vCenterY, PChar(panel.text), length(panel.text));
end;

procedure TThumbsForm.FStatusBarMouseLeave(Sender: TObject);
begin
  screen.cursor := crDefault;
end;

procedure TThumbsForm.FStatusBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case mmpIsPanelAt(FStatusBar, PANEL_FOLD, mmpMousePoint(FStatusBar))
    or mmpIsPanelAt(FStatusBar, PANEL_AVAV, mmpMousePoint(FStatusBar)) of  TRUE: screen.cursor := crHandPoint;
                                                                          FALSE: screen.cursor := crDefault; end;

end;

procedure TThumbsForm.FStatusBarResize(Sender: TObject);
begin
  mmpResizeStatusBar(FStatusBar);
end;

function TThumbsForm.takeScreenshot: string;
begin
  case whichHost of htThumbsHost: EXIT; end;
  var vScreenshotDirectory: string;
  mpvGetPropertyString(mpv, 'screenshot-directory', vScreenshotDirectory);

  case vScreenshotDirectory = EMPTY of   TRUE: mpvTakeScreenshot(mpv, extractFilePath(mpvFileName(mpv)));   // otherwise screenshots of an image go to Windows/System32 !!
                                        FALSE: mpvTakeScreenshot(mpv, vScreenshotDirectory); end;
end;

procedure TThumbsForm.timerTimer(Sender: TObject);
begin
  FProgressForm.timer.enabled := FALSE;
  case GS.mainForm <> NIL of TRUE: GS.mainForm.hide; end;
  case GS.mainForm <> NIL of TRUE: showWindow(GS.mainForm.handle, SW_HIDE); end;

  FShowing := TRUE;
  FProgressForm := NIL;

  case FInitialHost of
    htThumbsHost: FThumbs.playThumbs(FInitialFilePath);
    htMPVHost:    begin
                    FThumbs.playThumbs(FInitialFilePath, ptPlaylistOnly);
                    playCurrentItem; end;end;

  checkAudioVideo;

  case GS.showingConfig of FALSE: focusThumbs; end;
end;

function TThumbsForm.undoMove: string;
var
  vSrcFilePath: string;
  vDstFilePath: string;
begin
  case UM.undoPop(vSrcFilePath, vDstFilePath) of  TRUE: case saveMoveFileToFolder(vSrcFilePath, extractFilePath(vDstFilePath), 'Undone', FALSE)
                                                            of TRUE:  begin
                                                                        FThumbs.playlist.insert(vDstFilePath);
                                                                        FThumbs.playlist.find(vDstFilePath);
                                                                        FThumbs.playCurrentItem; end;end;
                                                 FALSE: mmpSetPanelText(FStatusBar, pnHelp, 'Nothing to Undo'); end;
end;

function TThumbsForm.whichHost: THostType;
begin
  result := htNoHost; // suppress compiler warning
  case FThumbsHost.visible of  TRUE: result := htThumbsHost;  end;
  case FMPVHost.visible    of  TRUE: result := htMPVHost;     end;
end;

function TThumbsForm.windowSize(const aKeyOp: TKeyOp): TVoid;
var dxy: integer;
begin
  case ssShift in mmpShiftState of  TRUE: dxy := 10;
                                   FALSE: dxy := 1; end;

  case aKeyOp of
    koWindowShorter:  SELF.height := SELF.height - dxy;
    koWindowTaller:   SELF.height := SELF.height + dxy;
    koWindowNarrower: SELF.width  := SELF.width  - dxy;
    koWindowWider:    SELF.width  := SELF.width  + dxy;
  end;
end;

procedure TThumbsForm.WMMove(var Message: TMessage);
begin
  case FThumbs = NIL of TRUE: EXIT; end;
  case FShowing      of FALSE: EXIT; end; // ignore the initial resizing while the form starts up
  moveHelpWindow;
  mmp.cmd(evGSAutoCenter, FALSE);
end;

//==========

procedure TThumbsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case (key = VK_ESCAPE) and GS.showingHelpFull of TRUE: EXIT; end;
  case whichHost of htMPVHost: mmpResetPanelHelp(FStatusBar); end; // don't obliterate thumbnail page numbers

  var vKeyOp: TKeyOp := processKeyStroke(mpv, key, shift, kdDn);

// COMMENTED OUT - adjustments are applied to every subsequent image, so not sure why this is necessary anymore
//  case (key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and (NOT (ssCtrl in shift)) and mpvAdjusted(mpv)
//    of  TRUE: begin
//                FKeyHandled := TRUE; // don't let the user accidentally change folders or image with the arrow keys after adjusting the image somehow
//                EXIT; end;end;

  FKeyHandled := processKeyOp(vKeyOp, shift, key);
end;

procedure TThumbsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case (key = VK_ESCAPE) and GS.showingHelpFull of TRUE: EXIT; end;
  case FKeyHandled of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  processKeyOp(processKeyStroke(mpv, key, shift, kdUp), shift, key);
  case key in [VK_F10..VK_F12] of TRUE: key := 0; end; // Disable the [notorious] Windows "sticky" nature of F10
end;

procedure TThumbsForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case whichHost of htThumbsHost: EXIT; end;
  case mmpCtrlKeyDown of   TRUE: mpvZoomOut(mpv);
                          FALSE: case mmpAltKeyDown of   TRUE: mpvPanLeft(mpv);
                                                        FALSE: mpvPanDn(mpv); end;end;
end;

procedure TThumbsForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  case whichHost of htThumbsHost: EXIT; end;
  case mmpCtrlKeyDown of   TRUE: mpvZoomIn(mpv);
                          FALSE: case mmpAltKeyDown of   TRUE: mpvPanRight(mpv);
                                                        FALSE: mpvPanUp(mpv); end;end;
end;

function TThumbsForm.processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState; const aKey: WORD): boolean;
begin
  result := FALSE;

  var vIx := FThumbs.currentIx;

  case aKeyOp of
    koNone:         EXIT;   // key not processed. bypass setting result to TRUE

    koAboutBox:           mmp.cmd(evAboutFormShow);
    koAdjustAspectRatio:  mmp.cmd(whichHost = htMPVHost, adjustAspectRatio);
    koAllReset:           begin mpvBrightnessReset(mpv); mpvContrastReset(mpv); mpvGammaReset(mpv); mpvPanReset(mpv); mpvRotateReset(mpv); mpvSaturationReset(mpv); mpvZoomReset(mpv); end;
    koBrightnessUp:       mpvBrightnessUp(mpv);
    koBrightnessDn:       mpvBrightnessDn(mpv);
    koBrightnessReset:    mpvBrightnessReset(mpv);
    koCentreWindow:       mmpCenterWindow(SELF.handle, point(SELF.width, SELF.height));
    koClipboard:          case whichHost of htMPVHost: FThumbs.playlist.copyToClipboard; end;
    koCloseAll:           begin mmpCancelDelay; FThumbs.cancel; modalResult := mrAll; mmp.cmd(evPAPostToEvery, WIN_CLOSEAPP); end;
    koCloseImageBrowser:  begin mmpCancelDelay; FThumbs.cancel; modalResult := mrClose; end;
    koCloseToMain:        begin mmpCancelDelay; FThumbs.cancel; modalResult := mrIgnore; end;
    koConfig:             mmp.cmd(evVMConfig);
    koContrastUp:         mpvContrastUp(mpv);
    koContrastDn:         mpvContrastDn(mpv);
    koContrastReset:      mpvContrastReset(mpv);
    koDeleteCurrentItem:  case whichHost of htMPVHost: deleteCurrentItem(aShiftState); end;
    koExploreFolder:      mmpShellExec(GS.mainForm.Handle, FThumbs.currentFolder);
    koFullscreen:         onDoubleClick(NIL);
    koGammaUp:            mpvGammaUp(mpv);
    koGammaDn:            mpvGammaDn(mpv);
    koGammaReset:         mpvGammaReset(mpv);
    koGreaterWindow:      begin mmpGreaterWindow(SELF.handle, aShiftState, FThumbs.thumbSize, whichHost); autoCenter; end;
    koHelpFull:           mmpHelpFull(htIATB, SELF.HANDLE);
    koKeep:               keepFile(FThumbs.playlist.currentItem);
    koKeepDelete:         begin mmpCancelDelay; case mmpKeepDelete(FThumbs.playlist.currentFolder) of TRUE: playNextFolder end;end;
    koMaximize:           maximizeWindow;
    koMinimizeWindow:     minimizeWindow;
    koMoveToKeyFolder:    case whichHost of htMPVHost: saveMoveFile(FThumbs.playlist.currentItem, mmpUserDstFolder(mmpFolderFromFKey(aKey)), 'Moved'); end;
    koNextFolder:         playNextFolder;
    koPanLeft:            mpvPanLeft(mpv);
    koPanRight:           mpvPanRight(mpv);
    koPanUp:              mpvPanUp(mpv);
    koPanDn:              mpvPanDn(mpv);
    koPanReset:           mpvPanReset(mpv);
    koPausePlay:          case whichHost of htMPVHost: pausePlay; end;
    koPlayFirst:          playFirst;
    koPlayLast:           playLast;
    koPlayNext:           playNext;
    koPlayPrev:           playPrev;
    koPlayThumbs:         case whichHost of htThumbsHost: showHost(htMPVHost); htMPVHost: begin showHost(htThumbsHost); FThumbs.playThumbs; end;end;
    koPrevFolder:         playPrevFolder;
    koReloadPlaylist:     reloadPlaylist;
    koRenameFile:         case whichHost of htMPVHost: renameFile(FThumbs.playlist.currentItem); end;
    koRenameCleanFile:    case whichHost of htMPVHost: renameFile(FThumbs.playlist.currentItem, TRUE); end;
    koReverseSlideshow:   case whichHost of htMPVHost: reverseSlideshow; end;
    koRotateR:            mpvRotateRight(mpv);
    koRotateL:            mpvRotateLeft(mpv);
    koRotateReset:        mpvRotateReset(mpv);
    koRunF10:             mmpOpenExternalApp(GS.mainForm.Handle, F10_APP, FThumbs.playlist.currentItem);
    koRunF11:             mmpOpenExternalApp(GS.mainForm.Handle, F11_APP, FThumbs.playlist.currentItem);
    koRunF12:             mmpOpenExternalApp(GS.mainForm.Handle, F12_APP, FThumbs.playlist.currentItem);
    koSaturationUp:       mpvSaturationUp(mpv);
    koSaturationDn:       mpvSaturationDn(mpv);
    koSaturationReset:    mpvSaturationReset(mpv);
    koSaveCopy:           case whichHost of htMPVHost: saveCopyFile(FThumbs.playlist.currentItem); end;
    koSaveMove:           case whichHost of htMPVHost: saveMoveFile(FThumbs.playlist.currentItem, mmpUserDstFolder('Saved'), 'Saved'); end;
    koScreenshot:         takeScreenshot;
    koSpeedDn:            speedDn;
    koSpeedReset:         speedReset;
    koSpeedUp:            speedUp;
    koThumbsDn:           case whichHost of htThumbsHost: begin FThumbs.thumbSize := FThumbs.thumbSize - 10; FThumbs.playThumbs; end;end;
    koThumbsUp:           case whichHost of htThumbsHost: begin FThumbs.thumbSize := FThumbs.thumbSize + 10; FThumbs.playThumbs; end;end;
    koToggleHelp:         begin case GS.showingHelp of TRUE: mmp.cmd(evHelpShutHelp); FALSE: moveHelpWindow(TRUE); end; autoCenter; end;
    koToggleNumlock:      mmpToggleNumlock;
    koUndoMove:           undoMove;
    koWiki:               mmpShellExec(0, MMP_WIKI_URL);
    koZoomIn:             mpvZoomIn(mpv);
    koZoomOut:            mpvZoomOut(mpv);
    koZoomReset:          mpvZoomReset(mpv);

    koWindowShorter,
    koWindowTaller,
    koWindowNarrower,
    koWindowWider:        windowSize(aKeyOp);

    koShowCaption:;
    koToggleControls:;
    koToggleBlackout:;
    koBrighterPB:;
    koDarkerPB:;
  end;

  case whichHost of htThumbsHost: case vIx = FThumbs.currentIx of FALSE:  begin // has the thumbnail page been recreated starting at a different item Ix ?
                                                                            FThumbsHost.refresh;
                                                                            mmpProcessMessages; end;end;end;

  result := TRUE; // key was processed
end;

end.
