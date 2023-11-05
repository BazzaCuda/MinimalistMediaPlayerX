{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit uiCtrls;

interface

uses
  forms, winApi.windows, winAPI.shellAPI, vcl.graphics, vcl.ComCtrls, vcl.extCtrls, system.classes, vcl.controls,
  consts;

type
  TUI = class(TObject)
  strict private
    FMainForm: TForm;
    FAutoCentre: boolean;
    FInitialized: boolean;
    FShowingHelp: boolean;
    FShowingPlaylist: boolean;
    FVideoPanel: TPanel;
  private
    function addMenuItems(const aForm: TForm): boolean;
    function setCustomTitleBar(const aForm: TForm): boolean;
    function setGlassFrame(const aForm: TForm): boolean;
    function setWindowStyle(const aForm: TForm): boolean;
    function createVideoPanel(const aForm: TForm): boolean;
    function getHeight: integer;
    function getWidth: integer;
    procedure onMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure onMPPlayNext(sender: TObject);
  public
    procedure formResize(sender: TObject);
    function adjustAspectRatio(const aWnd: HWND; const X: int64; const Y: int64): boolean;
    function arrangeAll: boolean;
    function autoCentreWindow(const aWnd: HWND): boolean;
    function centreWindow(const aWnd: HWND): boolean;
    function checkScreenLimits(const aWnd: HWND; const aWidth: integer; const aHeight: integer): boolean;
    function deleteCurrentItem(const shift: TShiftState): boolean;
    function doEscapeKey: boolean;
    function greaterWindow(const aWnd: HWND; const shift: TShiftState): boolean;
    function handle: HWND;
    function initUI(const aForm: TForm): boolean;
    function keepFile(const aFilePath: string): boolean;
    function minimizeWindow: boolean;
    function moveHelpWindow(const create: boolean = TRUE): boolean;
    function movePlaylistWindow(const create: boolean = TRUE): boolean;
    function openExternalApp(const anApp: string; const aParams: string): boolean;
    function posWinXY(const aHWND: HWND; const x: integer; const y: integer): boolean;
    function reloadPlaylistWindow: boolean;
    function renameFile(const aFilePath: string): boolean;
    function resize(const aWnd: HWND; const pt: TPoint; const X: int64; const Y: int64): boolean;
    function showAboutBox: boolean;
    function setWindowSize(const aMediaType: TMediaType): boolean;
    function showWindow: boolean;
    function smallerWindow(const aWnd: HWND): boolean;
    function toggleBlackout: boolean;
    function toggleCaptions(shift: TShiftState): boolean;
    function toggleHelpWindow: boolean;
    function toggleMaximized: boolean;
    function togglePlaylist: boolean;
    property autoCentre: boolean read FAutoCentre write FAutoCentre;
    property height: integer read getHeight;
    property initialized: boolean read FInitialized write FInitialized;
    property videoPanel: TPanel read FVideoPanel;
    property width: integer read getWidth;
  end;

function UI: TUI;

implementation

uses
  formSubtitles, mediaInfo, mediaPlayer, commonUtils, progressBar, winApi.messages, playlist, system.sysUtils, formCaption, keyboard, sysCommands,
  formHelp, formPlaylist, formAbout, globalVars, sendAll, _debugWindow;

var
  gUI: TUI;

function UI: TUI;
begin
  case gUI = NIL of TRUE: gUI := TUI.create; end;
  result := gUI;
end;

{ TUI }

function TUI.addMenuItems(const aForm: TForm): boolean;
begin
  var vSysMenu := getSystemMenu(aForm.handle, FALSE);
  AppendMenu(vSysMenu, MF_SEPARATOR, 0, '');
  AppendMenu(vSysMenu, MF_STRING, MENU_ABOUT_ID, '&About Minimalist Media Player…');
  AppendMenu(vSysMenu, MF_STRING, MENU_HELP_ID, 'Show &Keyboard functions');
end;

function TUI.adjustAspectRatio(const aWnd: HWND; const X: int64; const Y: int64): boolean;
var
  vRatio: double;
  vWidth, vHeight: integer;
begin
  case GV.closeApp of TRUE: EXIT; end;
  case FMainForm.WindowState = wsMaximized of TRUE: EXIT; end;

  case (X <= 0) OR (Y <= 0) of TRUE: EXIT; end;

  vRatio := Y / X;

  CU.getWndWidthHeight(aWnd, vWidth, vHeight);
  vHeight := trunc(vWidth * vRatio) + 2;

  case (UI.width <> vWidth) or (UI.height <> vHeight) of TRUE: setWindowPos(aWnd, HWND_TOP, 0, 0, vWidth, vHeight, SWP_NOMOVE); end; // don't add SWP_SHOWWINDOW

  postMessage(GV.appWnd, WM_AUTO_CENTRE_WINDOW, 0, 0);

  case MP.playing and CU.withinScreenLimits(vWidth, vHeight) of TRUE: postMessage(GV.appWnd, WM_SHOW_WINDOW, 0, 0); end;
end;

function TUI.arrangeAll: boolean;
var
  vCount: integer;
  vWidth, vHeight: integer;
  vScreenWidth, vScreenHeight: integer;
  vZero: integer;
  vHMiddle, vVMiddle: integer;
begin
  vCount     := SA.count;

  autoCentre := vCount = 1;
  case autoCentre of FALSE: SA.postToAll(WIN_AUTOCENTER_OFF); end;

  case vCount of
    1:       SA.postToAllEx(WIN_RESIZE, point(CU.getScreenWidth, 0));
    2:       SA.postToAllEx(WIN_RESIZE, point(CU.getScreenWidth div 2, 0));
    3, 4:    SA.postToAllEx(WIN_RESIZE, point(0, CU.getScreenHeight div 2));
    else     SA.postToAllEx(WIN_RESIZE, point(0, CU.getScreenWidth div vCount));
  end;

  application.processMessages; // make sure this window has resized before continuing
  SA.postToAll(WM_PROCESS_MESSAGES);

  CU.getWndWidthHeight(UI.handle, vWidth, vHeight);
  vScreenWidth  := CU.getScreenWidth;
  vScreenHeight := CU.getScreenHeight;
  vHMiddle      := vScreenWidth div 2;
  vVMiddle      := vScreenHeight div 2;
  vZero         := vHMiddle - vWidth;

  vCount := SA.count;
  case vCount = 2 of TRUE: begin
                             posWinXY(SA.HWNDs[1], vZero,  vVMiddle - (vHeight div 2));
                             posWinXY(SA.HWNDs[2], vHMiddle, vVMiddle - (vHeight) div 2);
                             case CU.offScreen(SA.HWNDs[1]) of TRUE: posWinXY(SA.HWNDs[1], vZero, 0); end;
                             case CU.offScreen(SA.HWNDs[2]) of TRUE: posWinXY(SA.HWNDs[2], vHMiddle, 0); end;
                           end;end;

  case vCount in [3, 4] of TRUE: begin
                             posWinXY(SA.HWNDs[1], vZero,  0);
                             posWinXY(SA.HWNDs[2], vHMiddle, 0); end;end;

  case vCount = 3 of TRUE: posWinXY(SA.HWNDs[3], vHMiddle - (vWidth div 2), vHeight); end;

  case vCount = 4 of TRUE: begin
                              posWinXY(SA.HWNDs[3], vZero,  vHeight);
                              posWinXY(SA.HWNDs[4], vHMiddle, vHeight); end;end;

  case vCount > 4 of TRUE: for var i := 1 to vCount do posWinXY(SA.HWNDs[i], 100 + (50 * (i - 1)), 100 + (50 * (i - 1))); end;

  SA.postToAll(WM_SMALLER_WINDOW);

  SA.clear;
end;

function TUI.autoCentreWindow(const aWnd: HWND): boolean;
begin
  case autoCentre of FALSE: EXIT; end;
  centreWindow(aWnd);
end;

function TUI.centreWindow(const aWnd: HWND): boolean;
var
  vR: TRect;
  vHPos: integer;
  vVPos: integer;

  function alreadyCentred: boolean;
  begin
    vHPos := (CU.getScreenWidth  - vR.width) div 2;
    vVPos := (CU.getScreenHeight - vR.height) div 2;
    result := (vR.left = vHPos) and (vR.top = vVPos);
  end;

begin
  getWindowRect(aWnd, vR);

  case alreadyCentred of TRUE: EXIT; end;

  case CU.withinScreenLimits(vR.width, vR.height) of FALSE: postMessage(GV.appWnd, WM_CHECK_SCREEN_LIMITS, 0, 0); end;

  case (vHPos > 0) and (vVPos > 0) of TRUE: SetWindowPos(aWnd, HWND_TOP, vHPos, vVPos, 0, 0, SWP_NOSIZE); end;

  application.processMessages;
  moveHelpWindow(FALSE);
  movePlaylistWindow(FALSE);
end;

function TUI.checkScreenLimits(const aWnd: HWND; const aWidth: integer; const aHeight: integer): boolean;
var
  vR:       TRect;
  vWidth:   integer;
  vHeight:  integer;
begin
  case GV.closeApp of TRUE: EXIT; end;

  getWindowRect(aWnd, vR);
  vWidth  := vR.width;
  vHeight := vR.height;

  case (vWidth > aWidth) or (vHeight > aHeight) of  TRUE: postMessage(GV.appWnd, WM_SMALLER_WINDOW, 0, 0);
                                                   FALSE: postMessage(GV.appWnd, WM_USER_CENTRE_WINDOW, 0, 0); end;
  application.processMessages;
end;

procedure TUI.onMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
// doesn't work in appEvents. Spurious WM_MOUSEMOVE events are generated even with no mouse connected!
begin
  case screen <> NIL of TRUE: screen.cursor := crDefault; end;
end;

procedure TUI.onMPPlayNext(sender: TObject);
begin
  movePlaylistWindow(FALSE);
end;

function TUI.createVideoPanel(const aForm: TForm): boolean;
begin
  FVideoPanel             := TPanel.create(aForm);
  FVideoPanel.parent      := aForm;
  FVideoPanel.align       := alClient;
  FVideoPanel.color       := clBlack;
  FVideoPanel.BevelOuter  := bvNone;
  FVideoPanel.OnMouseMove := onMouseMove;
end;

function TUI.deleteCurrentItem(const shift: TShiftState): boolean;
begin
  case PL.hasItems of FALSE: EXIT; end;
  MP.pause;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(PL.currentItem);
  case ssCtrl in shift of  TRUE: vMsg := vMsg + '*.*';
                          FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(PL.currentItem); end;

  case CU.showOkCancelMsgDlg(vMsg) = IDOK of TRUE:  begin
                                                      var vIx := PL.currentIx;
                                                      MP.dontPlayNext := TRUE;  // because...
                                                      MP.stop;                  // this would have automatically done MP.playNext
                                                      CU.deleteThisFile(PL.currentItem, shift);
                                                      PL.delete(PL.currentIx);  // this decrements PL's FPlayIx...
                                                      case (ssCtrl in shift) or (NOT PL.hasItems) of  TRUE: sendSysCommandClose(FMainForm.handle);
                                                                                                     FALSE: begin
                                                                                                              reloadPlaylistWindow;
                                                                                                              case vIx = 0 of  TRUE: MP.playCurrent;
                                                                                                                              FALSE: MP.playNext; end;end;end;end;end; // ...hence, playNext
end;

function TUI.doEscapeKey: boolean;
begin
  case FMainForm.WindowState = wsMaximized of  TRUE: toggleMaximized;
                                              FALSE: sendSysCommandClose(FMainForm.handle); end;
end;

procedure TUI.formResize(sender: TObject);
begin
  case GV.closeApp  of TRUE:  EXIT; end;
  case FInitialized of FALSE: EXIT; end;
  case PL.hasItems  of FALSE: EXIT; end;
  case ST.initialized and PB.initialized   of FALSE: EXIT; end;
  case FMainForm.WindowState = wsMaximized of TRUE:  EXIT; end;

  CU.delay(100); adjustAspectRatio(FMainForm.handle, MP.videoWidth, MP.videoHeight);
  ST.formResize;
  PB.formResize;

  moveHelpWindow(FALSE);
  movePlaylistWindow(FALSE);
  ST.opInfo := CU.formattedWidthHeight(FMainForm.width, FMainForm.height);
end;

function TUI.smallerWindow(const aWnd: HWND): boolean;
begin
  greaterWindow(aWnd, [ssCtrl]);
end;

function TUI.getHeight: integer;
begin
  result := FMainForm.height;
end;

function TUI.getWidth: integer;
begin
  result := FMainForm.width;
end;

function TUI.greaterWindow(const aWnd: HWND; const shift: TShiftState): boolean;
const
  dx = 50;
  dy = 30;
var
  newW: integer;
  newH: integer;
  vR:   TRect;

  function calcDimensions: boolean;
  begin
    case ssCtrl in shift of
      TRUE: begin
              newW := newW - dx;
              newH := newH - dy;
            end;
     FALSE: begin
              newW := newW + dx;
              newH := newH + dy;
            end;
    end;
  end;

begin
  getWindowRect(aWnd, vR);
  newW := vR.Width;
  newH := vR.height;

  calcDimensions; // do what the user requested

  case NOT CU.withinScreenLimits(newW, newH) of  TRUE:  begin
                                                          newH      := CU.getScreenHeight - dy;
                                                          try newW  := trunc(newH / CU.getAspectRatio(MP.videoWidth, MP.videoHeight)); except newW := 800; end;end;end;

  SetWindowPos(aWnd, HWND_TOP, 0, 0, newW, newH, SWP_NOMOVE); // resize the window

  postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
  application.processMessages;
end;

function TUI.handle: HWND;
begin
  result := FMainForm.handle;
end;

function TUI.initUI(const aForm: TForm): boolean;
begin
  FMainForm           := aForm;
  aForm.OnKeyDown     := KB.formKeyDn;
  aForm.OnKeyUp       := KB.formKeyUp;
  aForm.OnResize      := formResize;
  aForm.position      := poScreenCenter;
  aForm.borderIcons   := [biSystemMenu];
  aForm.styleElements := [];
  setGlassFrame(aForm);
  setCustomTitleBar(aForm);
  setWindowStyle(aForm);
  DragAcceptFiles(aForm.handle, TRUE);
  addMenuItems(aForm);
  aForm.color         := clBlack; // background color of the window's client area, so zooming-out doesn't show the design-time color
  createVideoPanel(aForm);
  FAutoCentre         := TRUE;
  aForm.width         := 800;
  aForm.height        := 300;
  MP.onPlayNext       := onMPPlayNext;
end;

function TUI.keepFile(const aFilePath: string): boolean;
var
  vNewName: string;
begin
  case PL.hasItems of FALSE: EXIT; end;
  MP.pause;
  vNewName := CU.renameFile(aFilePath, '_' + CU.getFileNameWithoutExtension(aFilePath));
  case vNewName <> aFilePath of TRUE: begin
                                        PL.replaceCurrentItem(vNewName);
                                        ST.opInfo := 'Kept'; end;end;
  MC.caption := PL.formattedItem;
  MP.resume;
end;

function TUI.minimizeWindow: boolean;
begin
   postMessage(UI.handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

function TUI.openExternalApp(const anApp, aParams: string): boolean;
begin
  MP.pause;
  CU.shellExec(anApp, aParams);
end;

function TUI.posWinXY(const aHWND: HWND; const x: integer; const y: integer): boolean;
begin
  SetWindowPos(aHWND, HWND_TOP, x, y, 0, 0, SWP_NOSIZE);
end;

function TUI.reloadPlaylistWindow: boolean;
begin
  case showingPlaylist of TRUE: reloadPlaylist(TRUE); end;
end;

function TUI.renameFile(const aFilePath: string): boolean;
var
  vNewName: string;
  vWasPlaying: boolean;
  vWasPlaylist: boolean;
begin
  case PL.hasItems of FALSE: EXIT; end;

  vWasPlaying := MP.playing;
  case vWasPlaying of TRUE: MP.pause; end;

  vWasPlaylist := showingPlaylist;
  case vWasPlaylist of TRUE: shutPlaylist; end;

  vNewName := CU.renameFile(aFilePath);
  case vNewName = aFilePath of FALSE: PL.replaceCurrentItem(vNewName); end;
  MC.caption := PL.formattedItem;

  case vWasPlaying  of TRUE: MP.resume; end;
  case vWasPlaylist of TRUE: movePlaylistWindow; end;
end;

function TUI.resize(const aWnd: HWND; const pt: TPoint; const X: int64; const Y: int64): boolean;
var
  vRatio: double;
  vWidth, vHeight: integer;
begin
  case (X <= 0) OR (Y <= 0) of TRUE: EXIT; end;

  vRatio := Y / X;

  case pt.x <> 0 of TRUE: begin
                            vWidth  := pt.x;
                            vHeight := trunc(pt.x * vRatio); end;end;

  case pt.y <> 0 of TRUE: begin
                            vWidth  := trunc(pt.y / vRatio);
                            vHeight := pt.y; end;end;

  sendMessage(aWnd, WM_SYSCOMMAND, SC_RESTORE, 0); // in case it was minimized
  SetWindowPos(aWnd, HWND_TOPMOST, 0, 0, vWidth, vHeight, SWP_NOMOVE);      // Both SWPs achieve HWND_TOP as HWND_TOP itself doesn't work.
  SetWindowPos(aWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE); // resize the window. Triggers adjustAspectRatio
end;

function TUI.setCustomTitleBar(const aForm: TForm): boolean;
begin
  aForm.customTitleBar.enabled        := TRUE;
  aForm.customTitleBar.showCaption    := FALSE;
  aForm.customTitleBar.showIcon       := FALSE;
  aForm.customTitleBar.systemButtons  := FALSE;
  aForm.customTitleBar.systemColors   := FALSE;
  aForm.customTitleBar.systemHeight   := FALSE;
  aForm.customTitleBar.height         := 1; // systemHeight=FALSE must be set before this
end;

function TUI.setGlassFrame(const aForm: TForm): boolean;
begin
  aForm.glassFrame.enabled  := TRUE;
  aForm.glassFrame.top      := 1;
end;

function TUI.setWindowSize(const aMediaType: TMediaType): boolean;
begin
  case aMediaType of  mtAudio: begin  FMainForm.width  := 600;
                                      FMainForm.height := UI_DEFAULT_AUDIO_HEIGHT; end;
                      mtVideo: begin  FMainForm.width  := trunc(CU.getScreenWidth * 0.9);
                                      {FMainForm.height := trunc(FMainForm.width * 0.75);} end;end; // leave it to adjustAspectRatio
end;

function TUI.setWindowStyle(const aForm: TForm): boolean;
begin
  SetWindowLong(aForm.handle, GWL_STYLE, GetWindowLong(aForm.handle, GWL_STYLE) OR WS_CAPTION AND NOT WS_BORDER AND NOT WS_VISIBLE);
end;

function TUI.showAboutBox: boolean;
begin
  formAbout.showAboutBox(CU.getFileVersionFmt('', '%d.%d'), CU.getFileVersionFmt);
end;

function TUI.showWindow: boolean;
begin
  case GV.closeApp of TRUE: EXIT; end;
  winAPI.windows.showWindow(FMainForm.Handle, SW_SHOW); // solves the "Cannot change Visible in onShow or in onHide" error
  FMainForm.visible := TRUE;                            // still needed in addition to the previous in order to get a mouse cursor!
end;

function TUI.toggleBlackout: boolean;
begin
  PB.showProgressBar := NOT PB.showProgressBar;
end;

function TUI.moveHelpWindow(const create: boolean = TRUE): boolean;
begin
  var vPt := FVideoPanel.ClientToScreen(point(FVideoPanel.left + FVideoPanel.width + 1, FVideoPanel.top - 2)); // screen position of the top right corner of the application window, roughly.
  showHelp(vPt, create);
end;

function TUI.movePlaylistWindow(const create: boolean = TRUE): boolean;
begin
  var vPt := FVideoPanel.ClientToScreen(point(FVideoPanel.left + FVideoPanel.width, FVideoPanel.top - 2)); // screen position of the top right corner of the application window, roughly.
  showPlaylist(vPt, videoPanel.height, create);
end;

function TUI.toggleCaptions(shift: TShiftState): boolean;
begin
  case (ssCtrl in shift) and ST.showTime and (NOT ST.showData) of TRUE: begin MI.getData(ST.dataMemo); ST.showData := TRUE; EXIT; end;end;

  ST.showTime := NOT ST.showTime;

  case (ssCtrl in shift) and ST.showTime of  TRUE: begin MI.getData(ST.dataMemo); ST.showData := TRUE; end;
                                            FALSE: ST.showData := FALSE; end;
end;

function TUI.toggleHelpWindow: boolean;
begin
  shutPlaylist;
  FShowingPlaylist := FALSE;

  FShowingHelp := NOT FShowingHelp;
  case FShowingHelp of  TRUE: moveHelpWindow;
                       FALSE: shutHelp; end;
end;

function TUI.toggleMaximized: boolean;
begin
  case FMainForm.WindowState <> wsMaximized of  TRUE: FMainForm.windowState := wsMaximized;
                                               FALSE: FMainForm.windowState := wsNormal; end;
end;

function TUI.togglePlaylist: boolean;
begin
  shutHelp;
  FShowingHelp := FALSE;

  FShowingPlaylist := NOT FShowingPlaylist;
  case FShowingPlaylist of  TRUE: movePlaylistWindow;
                           FALSE: shutPlaylist; end;
end;

initialization
  gUI := NIL;

finalization
  case gUI <> NIL of TRUE: gUI.free; end;

end.
