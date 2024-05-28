{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit formThumbs;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.appEvnts, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.Forms, Vcl.ComCtrls,
  MPVBasePlayer,
  mmpThumbsKeyboard,
  TMPVHostClass, TThumbsClass;

type
  THostType = (htMPVHost, htThumbsHost);

  TThumbsForm = class(TForm)
    FStatusBar: TStatusBar;
    FThumbsHost: TPanel;
    applicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FStatusBarResize(Sender: TObject);
    procedure applicationEventsHint(Sender: TObject);
  strict private
    mpv: TMPVBasePlayer;
    FInitialFilePath: string;
    FKeyHandled: boolean;
    FMPVHost: TMPVHost;
    FShowing: boolean;
    FThumbs: TThumbs;
  private
    procedure onInitMPV(sender: TObject);
    procedure onOpenFile(const aURL: string);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);
    function autoCentre: boolean;
    function deleteCurrentItem: boolean;
    function keepFile(const aFilePath: string): boolean;
    function playCurrentItem: boolean;
    function playFirst: boolean;
    function playLast: boolean;
    function playNext: boolean;
    function playNextFolder: boolean;
    function playPrev: boolean;
    function playPrevFolder: boolean;
    function processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
    function renameFile(const aFilePath: string): boolean;
    function saveCopyFile(const aFilePath: string): boolean;
    function saveMoveFile(const aFilePath: string): boolean;
    function showHost(const aHostType: THostType): boolean;
    function showPlaylist: boolean;
    function takeScreenshot: string;
    function whichHost: THostType;
  protected
    procedure CreateParams(var params: TCreateParams); override;
  public
    function initThumbnails(const aFilePath: string; const aRect: TRect): boolean;
  end;

function showThumbs(const aFilePath: string; const aRect: TRect): boolean;

implementation

uses
  mmpMPVCtrls, mmpMPVProperties,
  mmpConsts, mmpDialogs, mmpFileUtils, mmpFolderNavigation, mmpPanelCtrls, mmpTicker, mmpUtils, mmpWindowCtrls,
  formAboutBox, formPlaylist,
  TGlobalVarsClass, TSendAllClass,

  _debugWindow;

function showThumbs(const aFilePath: string; const aRect: TRect): boolean;
begin
  var vTF := TThumbsForm.create(NIL);
  try
    vTF.initThumbnails(aFilePath, aRect);
    GV.showingThumbs := TRUE;
    vTF.showModal;
  finally
    vTF.free;
    GV.showingThumbs := FALSE;
  end;
end;

{$R *.dfm}

procedure TThumbsForm.applicationEventsHint(Sender: TObject);
begin
  case length(application.hint) > 1 of
    TRUE: case application.hint[1] = '$' of TRUE: FThumbs.setPanelText(copy(application.hint, 2, MAXINT)); //    mmpSetPanelText(FStatusBar, pnName, copy(application.hint, 2, MAXINT));
                                           FALSE: mmpSetPanelText(FStatusBar, pnHint, application.hint); end;end;
end;

function TThumbsForm.autoCentre: boolean;
begin
  case GV.autoCentre of TRUE: mmpCentreWindow(SELF.handle); end;
end;

procedure TThumbsForm.CreateParams(var params: TCreateParams);
begin
  inherited;
  params.exStyle := params.exStyle OR WS_EX_APPWINDOW; // put an icon on the taskbar for the user
end;

function TThumbsForm.deleteCurrentItem: boolean;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  var vShiftState := mmpShiftState;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(FThumbs.playlist.currentItem);
  case ssCtrl in vshiftState of  TRUE: vMsg := vMsg + '*.*';
                                FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(FThumbs.playlist.currentItem); end;

  case mmpShowOkCancelMsgDlg(vMsg) = IDOK of TRUE:  begin
                                                      var vIx := FThumbs.playlist.currentIx;
                                                      mmpDeleteThisFile(FThumbs.playlist.currentItem, vShiftState);
                                                      FThumbs.playlist.delete(FThumbs.playlist.currentIx);  // this decrements PL's FPlayIx...
                                                      case (ssCtrl in vShiftState) or (NOT FThumbs.playlist.hasItems) of TRUE: begin close; SA.postToAll(WIN_CLOSEAPP); end;
                                                                                                     FALSE: begin
                                                                                                              loadPlaylistWindow;
                                                                                                              case vIx = 0 of  TRUE: playCurrentItem;
                                                                                                                              FALSE: playNext; end;end;end;end;end; // ...hence, playNext
end;

procedure TThumbsForm.FormActivate(Sender: TObject);
begin
  FShowing := TRUE;
end;

procedure TThumbsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case mpv      = NIL of FALSE: freeAndNIL(mpv); end;      // do this first or the user will briefly see the blank form background
  case FMPVHost = NIL of FALSE: freeAndNIL(FMPVHost); end;
  case FThumbs  = NIL of FALSE: freeAndNIL(FThumbs); end;
end;

procedure TThumbsForm.FormCreate(Sender: TObject);
begin
  FMPVHost := TMPVHost.create(SELF);
  FMPVHost.parent := SELF;
  FMPVHost.OnOpenFile := onOpenFile;

  mmpInitStatusBar(FStatusBar);

  mpvCreate(mpv);
  mpv.onInitMPV    := onInitMPV;
  mpvInitPlayer(mpv, FMPVHost.handle, '', extractFilePath(paramStr(0)));  // THIS RECREATES THE INTERNAL MPV OBJECT in TMPVBasePlayer
  mpvSetPropertyString(mpv, 'image-display-duration', 'inf'); // override the user's .conf file
end;

procedure TThumbsForm.FormResize(Sender: TObject);
begin
  case FThumbs = NIL of TRUE: EXIT; end;
  case FShowing      of FALSE: EXIT; end; // ignore the initial resizing while the form starts up
  case whichHost of htThumbsHost: FThumbs.playThumbs; end;
end;

procedure TThumbsForm.FormShow(Sender: TObject);
begin
  FThumbs := TThumbs.create;
  FThumbs.initThumbs(FMPVHost, FThumbsHost, FStatusBar);
  FThumbs.playThumbs(FInitialFilePath);
end;

function TThumbsForm.initThumbnails(const aFilePath: string; const aRect: TRect): boolean;
begin
  FInitialFilePath := aFilePath;

  SELF.top    := aRect.top;
  SELF.left   := aRect.left;
  SELF.width  := aRect.width;
  SELF.height := aRect.height;

//  autoCentre;

  SELF.borderIcons   := [biSystemMenu, biMaximize];
  SELF.borderStyle   := bsSizeable;
  SELF.borderWidth   := 0;
  SELF.color         := clBlack;

  FMPVHost.align      := alClient;
  FThumbsHost.align := alClient;

  FMPVHost.visible      := FALSE;
  FThumbsHost.visible := TRUE;

  FThumbsHost.styleElements  := [];
  FThumbsHost.bevelOuter     := bvNone;
  FThumbsHost.color          := clBlack;
end;


function TThumbsForm.keepFile(const aFilePath: string): boolean;
var
  vNewName: string;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  vNewName := mmpRenameFile(aFilePath, '_' + mmpFileNameWithoutExtension(aFilePath));
  case vNewName <> aFilePath of  TRUE: begin
                                        FThumbs.playlist.replaceCurrentItem(vNewName);
                                        mmpSetPanelText(FStatusBar, pnVers, 'Kept'); end;
                                FALSE:  mmpSetPanelText(FStatusBar, pnVers, 'NOT Kept'); end;
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
  except end;
  tickerStop;

  showHost(htMPVHost);

  FThumbs.setPanelText(aURL, tickerTotalMs);
end;

procedure TThumbsForm.onStateChange(cSender: TObject; eState: TMPVPlayerState);
// don't need this yet, so not hooked up to mpv
begin
  case eState of
    mpsPlay:;
    mpsEnd {, mpsStop}:;
  end;
end;

function TThumbsForm.playCurrentItem: boolean;
begin
  case whichHost of
    htMPVHost:    FThumbs.playCurrentItem;
    htThumbsHost: FThumbs.playThumbs;
  end;
end;

function TThumbsForm.playFirst: boolean;
begin
  FThumbs.playlist.first;
  case whichHost of
    htMPVHost:    FThumbs.playCurrentItem;
    htThumbsHost: FThumbs.playThumbs;
  end;
end;

function TThumbsForm.playLast: boolean;
begin
  FThumbs.playlist.last;
  case whichHost of
    htMPVHost:    FThumbs.playCurrentItem;
    htThumbsHost: FThumbs.playThumbs;
  end;
end;

function TThumbsForm.playNext: boolean;
begin
  case whichHost of
    htMPVHost:    FThumbs.playNext;
    htThumbsHost: FThumbs.playThumbs;
  end;
end;

function TThumbsForm.playNextFolder: boolean;
var
  nextFolder: string;
begin
  nextFolder := mmpNextFolder(FThumbs.currentFolder, nfForwards);
  case NextFolder = '' of FALSE: FThumbs.playThumbs(NextFolder + '$$$.$$$', ptPlaylistOnly); end; // because extractFilePath needs a file name ;)
  mpvStop(mpv); // if the folder is empty we want a blank screen
  playCurrentItem;
end;

function TThumbsForm.playPrev: boolean;
begin
  case whichHost of
    htMPVHost:    FThumbs.playPrev;
    htThumbsHost: FThumbs.playPrevThumbsPage;
  end;
end;

function TThumbsForm.playPrevFolder: boolean;
var
  prevFolder: string;
begin
  prevFolder := mmpNextFolder(FThumbs.currentFolder, nfBackwards);
  case prevFolder = '' of FALSE: FThumbs.playThumbs(prevFolder + '$$$.$$$', ptPlaylistOnly); end; // because extractFilePath needs a file name ;)
  mpvStop(mpv); // if the folder is empty we want a blank screen
  playCurrentItem;
end;

function TThumbsForm.showHost(const aHostType: THostType): boolean;
begin
  FMPVHost.visible    := aHostType = htMPVHost;
  FThumbsHost.enabled := aHostType = htThumbsHost;
  FThumbsHost.visible := aHostType = htThumbsHost;
end;

function TThumbsForm.renameFile(const aFilePath: string): boolean;
var
  vNewName: string;
begin
  case FThumbs.playlist.hasItems of FALSE: EXIT; end;

  vNewName := mmpRenameFile(aFilePath);
  case vNewName = aFilePath of FALSE: begin
                                        FThumbs.playlist.replaceCurrentItem(vNewName);
                                        mmpSetPanelText(FStatusBar, pnSave, 'Renamed: ' + aFilePath); end;end;
  mmpSetPanelText(FStatusBar, pnName, extractFileName(vNewName));
end;

function TThumbsForm.saveCopyFile(const aFilePath: string): boolean;
begin
  case mmpCopyFile(aFilePath, 'Copied', FALSE) of  TRUE:  mmpSetPanelText(FStatusBar, pnVers, 'Copied');
                                                  FALSE:  mmpSetPanelText(FStatusBar, pnVers, 'NOT Copied'); end;
end;

function TThumbsForm.saveMoveFile(const aFilePath: string): boolean;
begin
  case mmpCopyFile(aFilePath, 'Moved', TRUE) of  TRUE:  begin
                                                          mmpSetPanelText(FStatusBar, pnVers, 'Saved');
                                                          FThumbs.playlist.delete(FThumbs.playlist.currentIx);
                                                          playCurrentItem;
                                                        end;
                                                FALSE:  mmpSetPanelText(FStatusBar, pnVers, 'NOT Saved'); end;
end;

function TThumbsForm.showPlaylist: boolean;
begin
  EXIT; // EXPERIMENTAL
  var vPt := FThumbsHost.ClientToScreen(point(FThumbsHost.left + FThumbsHost.width, FThumbsHost.top - 2)); // screen position of the top right corner of the application window, roughly.
  formPlaylist.showPlaylist(FThumbs.playlist, vPt, FThumbsHost.height, TRUE);
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

  case vScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, extractFilePath(mpvFileName(mpv)));   // otherwise screenshots of an image go to Windows/System32 !!
                                    FALSE: result := mpvTakeScreenshot(mpv, vScreenshotDirectory); end;
end;

function TThumbsForm.whichHost: THostType;
begin
  case FThumbsHost.visible of  TRUE: result := htThumbsHost;  end;
  case FMPVHost.visible    of  TRUE: result := htMPVHost;     end;
end;

//==========

procedure TThumbsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  mmpResetPanelVers(FStatusBar);
  var vKeyOp: TKeyOp := processKeyStroke(mpv, key, shift, kdDn);

  case (key in [VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT]) and (NOT (ssCtrl in shift)) and mpvAdjusted(mpv)
    of  TRUE: begin
                FKeyHandled := TRUE; // don't let the user accidentally change folders or image with the arrow keys after adjusting the image somehow
                EXIT; end;end;

  FKeyHandled := processKeyOp(vKeyOp, shift);
end;

procedure TThumbsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FKeyHandled of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  processKeyOp(processKeyStroke(mpv, key, shift, kdUp), shift);
end;

function TThumbsForm.processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
begin
  result := FALSE;

  var vIx := FThumbs.currentIx;

  case aKeyOp of
    koNone:         EXIT;   // key not processed. bypass setting result to TRUE

    koCloseImageBrowser:     close;

    koBrightnessUp:       mpvBrightnessUp(mpv);
    koBrightnessDn:       mpvBrightnessDn(mpv);
    koBrightnessReset:    mpvBrightnessReset(mpv);
    koContrastUp:         mpvContrastUp(mpv);
    koContrastDn:         mpvContrastDn(mpv);
    koContrastReset:      mpvContrastReset(mpv);
    koGammaUp:            mpvGammaUp(mpv);
    koGammaDn:            mpvGammaDn(mpv);
    koGammaReset:         mpvGammaReset(mpv);
    koPanLeft:            mpvPanLeft(mpv);
    koPanRight:           mpvPanRight(mpv);
    koPanUp:              mpvPanUp(mpv);
    koPanDn:              mpvPanDn(mpv);
    koPanReset:           mpvPanReset(mpv);
    koRotateR:            mpvRotateRight(mpv);
    koRotateL:            mpvRotateLeft(mpv);
    koRotateReset:        mpvRotateReset(mpv);
    koSaturationUp:       mpvSaturationUp(mpv);
    koSaturationDn:       mpvSaturationDn(mpv);
    koSaturationReset:    mpvSaturationReset(mpv);
    koScreenshot:         takeScreenshot;
    koZoomIn:             mpvZoomIn(mpv);
    koZoomOut:            mpvZoomOut(mpv);
    koZoomReset:          mpvZoomReset(mpv);

    koAllReset:           begin mpvBrightnessReset(mpv); mpvContrastReset(mpv); mpvGammaReset(mpv); mpvPanReset(mpv); mpvRotateReset(mpv); mpvSaturationReset(mpv); mpvZoomReset(mpv); end;

    koPlayThumbs:         begin FThumbs.playThumbs; showHost(htThumbsHost); end;
    koPlayNext:           playNext;
    koPlayPrev:           playPrev;
    koNextFolder:         playNextFolder;
    koPrevFolder:         playPrevFolder;
    koPlayFirst:          playFirst;
    koPlayLast:           playLast;
    koAboutBox:           showAboutBox;
    koCloseAll:           begin close; SA.postToAll(WIN_CLOSEAPP); end;
    koGreaterWindow:      begin mmpGreaterWindow(SELF.handle, aShiftState); autoCentre; end;
    koCentreWindow:       mmpCentreWindow(SELF.handle);
    koRenameFile:         case whichHost of htMPVHost: renameFile(FThumbs.playlist.currentItem); end;
    koSaveMove:           case whichHost of htMPVHost: saveMoveFile(FThumbs.playlist.currentItem); end;
    koDeleteCurrentItem:  case whichHost of htMPVHost: deleteCurrentItem; end;
    koKeep:               keepFile(FThumbs.playlist.currentItem);
    koSaveCopy:           case whichHost of htMPVHost: saveCopyFile(FThumbs.playlist.currentItem); end;



    koPausePlay:;
    koShowCaption:;
    koFullscreen:;
    koToggleControls:;
    koToggleBlackout:;
    koMinimizeWindow:;
    koClipboard:;
    koReloadPlaylist:;
    koToggleHelp:;
    koBrighterPB:;
    koDarkerPB:;
    koTogglePlaylist:     showPlaylist;
    koToggleRepeat:;
    koMaximize:;
    koSpeedUp:;
    koSpeedDn:;
    koSpeedReset:;
  end;

  case whichHost of htThumbsHost: case vIx = FThumbs.currentIx of FALSE:  begin // has the thumbnail page been recreated starting at a different item Ix ?
                                                                            FThumbsHost.refresh;
                                                                            application.processMessages; end;end;end;

  result := TRUE; // key was processed
end;

end.
