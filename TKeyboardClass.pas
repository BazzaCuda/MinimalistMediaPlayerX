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
unit TKeyboardClass;

interface

uses
  system.classes;

type
  TKeyOp = (koNone,
            koCloseApp, koVolUp, koVolDn, koTab, koTabTab, koPausePlay, koFrameForwards, koFrameBackwards, koAdjustAspectRatio, koBrightnessUp,
            koBrightnessDn, koZoomIn, koZoomOut, koStartOver, koShowCaption, koMuteUnmute, koPlayFirst, koPlayNext, koPlayPrev, koPlayLast,
            koPanLeft, koPanRight, koPanUp, koPanDn, koRotateR, koRotateL, koFullscreen, koZoomReset, koGreaterWindow, koToggleControls,
            koRunPot, koRunCut, koRunShot, koToggleBlackout, koCentreWindow, koMinimizeWindow, koDeleteCurrentItem, koRenameFile, koSpeedUp,
            koSpeedDn, koSpeedReset, koEscape, koClipboard, koKeep, koReloadPlaylist, koAlwaysPot, koPanReset, koBrightnessReset, koBookmarkSave,
            koBookmarkLoad, koBookmarkDelete, koRotateReset, koContrastUp, koContrastDn, koContrastReset, koGammaUp, koGammaDn, koSaturationUp, koSaturationDn,
            koGammaReset, koSaturationReset, koAllReset, koToggleHelp, koBrighterPB, koDarkerPB, koTogglePlaylist, koCloseAll, koArrangeAll, koSyncMedia,
            koScreenshot, koToggleSubtitles, koToggleRepeat, koToggleEditMode, koAboutBox, koMaximize, koCycleAudio, koCycleSubs, koPrevChapter, koNextChapter);
  TKeyDirection = (kdDown, kdUp);

  TKeyboard = class(TObject)
  strict private
    FKey:         word;
    FShiftState:  TShiftState;
    FUpDn:        TKeyDirection;
  private
    function keyIs(const aChar: char): boolean; overload;
    function keyIs(const aKeyCode: word): boolean; overload;
    function getKeyOp: TKeyOp;
    function getKeyUp: boolean;
    function getKeyDn: boolean;
    function getCtrl: boolean;
    function getShift: boolean;
    function getAlt: boolean;
    function getCapsLock: boolean;
    function getNumLock: boolean;
  public
    function  processKeyStroke(var aKey: word; aShiftState: TShiftState; upDn: TKeyDirection): boolean;
    procedure formKeyDn(sender: TObject; var key: WORD; shift: TShiftState);
    procedure formKeyUp(sender: TObject; var key: WORD; shift: TShiftState);
    property alt:         boolean     read getAlt;
    property capsLock:    boolean     read getCapsLock;
    property ctrl:        boolean     read getCtrl;
    property key:         word        read FKey         write FKey;
    property shift:       boolean     read getShift;
    property shiftState:  TShiftState read FShiftState  write FShiftState;
    property keyDn:       boolean     read getKeyDn;
    property keyUp:       boolean     read getKeyUp;
    property numLock:     boolean     read getNumLock;
  end;

function KB: TKeyboard;

implementation

uses
  TSysCommandsClass, winApi.windows, TMediaPlayerClass, TMediaInfoClass, formCaption, TPlaylistClass, TUICtrlsClass, consts, globalVars, commonUtils, vcl.forms,
  system.sysUtils, TBookmarkClass, TProgressBarClass, formSubtitles, TSendAllClass, formPlaylist, _debugWindow;

const
  A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
  N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';
  _0 = '0'; _1 = '1'; _2 = '2'; _3 = '3'; _4 = '4'; _5 = '5'; _6 = '6'; _7 = '7'; _8 = '8'; _9 = '9';
  _EQUALS = 187; SLASH = 191; BACKSLASH = 220; OPEN_BRACKET = 219; CLOSE_BRACKET = 221; HYPHEN = 189; HASH = 222; BACKSPACE = 8;
  OPEN_BRACE = 219; CLOSE_BRACE = 221; SINGLE_QUOTE = 192; SEMICOLON = 186;

var
  gKB: TKeyboard;

function KB: TKeyboard;
begin
  case gKB = NIL of TRUE: gKB := TKeyboard.create; end;
  result := gKB;
end;

{ TKeyboard }

function TKeyboard.getKeyOp: TKeyOp;
begin
  result := koNone;
  case keyUp and keyIs(X)                                     of TRUE: result := koCloseApp; end;
  case keyUp and keyIs(VK_ESCAPE)                             of TRUE: result := koCloseApp; end;
  case keyDn and keyIs(VK_DOWN) and NOT GV.showingPlaylist    of TRUE: result := koVolDn; end;
  case keyDn and keyIs(VK_VOLUME_DOWN)                        of TRUE: result := koVolDn; end;
  case keyDn and keyIs(VK_UP)   and NOT GV.showingPlaylist    of TRUE: result := koVolUp; end;
  case keyDn and keyIs(VK_VOLUME_UP)                          of TRUE: result := koVolUp; end;
  case keyDn and keyIs(T)                                     of TRUE: result := koTab; end;
  case keyUp and keyIs(VK_SPACE)                              of TRUE: result := koPausePlay; end;
  case keyDn and keyIs(VK_RIGHT)                              of TRUE: result := koFrameForwards; end;
  case keyDn and keyIs(VK_LEFT)                               of TRUE: result := koFrameBackwards; end;
  case keyDn and keyIs(VK_TAB)                                of TRUE: result := koTabTab; end;
  case keyUp and keyIs(J)                                     of TRUE: result := koAdjustAspectRatio; end;
  case keyDn and keyIs(_9) and NOT ctrl                       of TRUE: result := koBrightnessUp; end;
  case keyDn and keyIs(_8)                                    of TRUE: result := koBrightnessDn; end;
  case keyDn and keyIs(I)                                     of TRUE: result := koZoomIn; end;
  case keyDn and keyIs(O)                                     of TRUE: result := koZoomOut; end;
  case keyUp and keyIs(S)                                     of TRUE: result := koStartOver; end;
  case keyUp and keyIs(HASH)                                  of TRUE: result := koShowCaption; end;
  case keyUp and keyIs(E)                                     of TRUE: result := koMuteUnmute; end;
  case keyUp and keyIs(VK_VOLUME_MUTE)                        of TRUE: result := koMuteUnmute; end;
  case keyUp and keyIs(W)                                     of TRUE: result := koPlayNext; end;
  case keyUp and keyIs(VK_RETURN) and NOT GV.showingPlaylist  of TRUE: result := koPlayNext; end;
  case keyUp and keyIs(Q)                                     of TRUE: result := koPlayPrev; end;
  case keyDn and keyIs(VK_LEFT) and ctrl                      of TRUE: result := koPanLeft; end;
  case keyDn and keyIs(VK_RIGHT) and ctrl                     of TRUE: result := koPanRight; end;
  case keyDn and keyIs(VK_UP) and ctrl                        of TRUE: result := koPanUp; end;
  case keyDn and keyIs(VK_DOWN) and ctrl                      of TRUE: result := koPanDn; end;
  case keyUp and keyIs(VK_NEXT) and NOT GV.showingPlaylist    of TRUE: result := koRotateR; end;
  case keyUp and keyIs(VK_PRIOR) and NOT GV.showingPlaylist   of TRUE: result := koRotateL; end;
  case keyUp and keyIs(F)                                     of TRUE: result := koFullscreen; end;
  case keyUp and keyIs(U)                                     of TRUE: result := koZoomReset; end;
  case keyDn and keyIs(G)                                     of TRUE: result := koGreaterWindow; end;
  case keyUp and keyIs(A) and NOT ctrl                        of TRUE: result := koPlayFirst; end;
  case keyUp and keyIs(A) and ctrl                            of TRUE: result := koAboutBox; end;
  case keyUp and keyIs(VK_HOME)                               of TRUE: result := koPlayFirst; end;
  case keyUp and keyIs(Z)                                     of TRUE: result := koPlayLast; end;
  case keyUp and keyIs(VK_END)                                of TRUE: result := koPlayLast; end;
  case keyUp and keyIs(C)                                     of TRUE: result := koToggleControls; end;
  case keyUp and keyIs(VK_F10)                                of TRUE: result := koRunPot; end;
  case keyUp and keyIs(VK_F11)                                of TRUE: result := koRunCut; end;
  case keyUp and keyIs(VK_F12)                                of TRUE: result := koRunShot; end;
  case keyUp and keyIs(B) and NOT ctrl                        of TRUE: result := koToggleBlackout; end;
  case keyUp and keyIs(H)                                     of TRUE: result := koCentreWindow; end;
  case keyUp and keyIs(N)                                     of TRUE: result := koMinimizeWindow; end;
  case keyUp and keyIs(D)                                     of TRUE: result := koDeleteCurrentItem; end;
  case keyUp and keyIs(VK_DELETE)                             of TRUE: result := koDeleteCurrentItem; end;
  case keyUp and keyIs(R)                                     of TRUE: result := koRenameFile; end;
  case keyDn and keyIs(VK_ADD)                                of TRUE: result := koSpeedUp; end;
  case keyDn and keyIs(SLASH)                                 of TRUE: result := koSpeedUp; end;
  case keyDn and keyIs(VK_SUBTRACT)                           of TRUE: result := koSpeedDn; end;
  case keyDn and keyIs(BACKSLASH)                             of TRUE: result := koSpeedDn; end;
  case keyUp and keyIs(_1)                                    of TRUE: result := koSpeedReset; end;
  case keyUp and keyIs(VK_ESCAPE)                             of TRUE: result := koEscape; end;
  case keyUp and keyIs(VK_INSERT)                             of TRUE: result := koClipboard; end;
  case keyUp and keyIs(K)                                     of TRUE: result := koKeep; end;
  case keyUp and keyIs(L)                                     of TRUE: result := koReloadPlaylist; end;
  case keyUp and keyIs(P) and ctrl                            of TRUE: result := koAlwaysPot; end;
  case keyUp and keyIs(_2)                                    of TRUE: result := koBrightnessReset; end;
  case keyUp and keyIs(_3)                                    of TRUE: result := koPanReset; end;
  case keyUp and keyIs(_4)                                    of TRUE: result := koRotateReset; end;
  case keyUp and keyIs(_5)                                    of TRUE: result := koBookmarkSave; end;
  case keyUp and keyIs(_6)                                    of TRUE: result := koBookmarkLoad; end;
  case keyUp and keyIs(_7)                                    of TRUE: result := koBookmarkDelete; end;
  case keyDn and keyIs(_EQUALS)                               of TRUE: result := koContrastUp; end;
  case keyDn and keyIs(HYPHEN)                                of TRUE: result := koContrastDn; end;
  case keyUp and keyIs(_0) and NOT ctrl                       of TRUE: result := koContrastReset; end;
  case keyDn and keyIs(OPEN_BRACKET)                          of TRUE: result := koGammaDn; end;
  case keyDn and keyIs(CLOSE_BRACKET)                         of TRUE: result := koGammaUp; end;
  case keyUp and keyIs(SINGLE_QUOTE)                          of TRUE: result := koGammaReset; end;
  case keyDn and keyIs(OPEN_BRACKET) and shift                of TRUE: result := koSaturationDn; end; // open curly brace
  case keyDn and keyIs(CLOSE_BRACKET) and shift               of TRUE: result := koSaturationUp; end; // close curly brace
  case keyUp and keyIs(SEMICOLON)                             of TRUE: result := koSaturationReset; end;
  case keyUp and keyIs(BACKSPACE)                             of TRUE: result := koAllReset; end;
  case keyUp and keyIs(VK_F1)                                 of TRUE: result := koToggleHelp; end;
  case keyUp and keyIs(H) and ctrl                            of TRUE: result := koToggleHelp; end;
  case keyDn and keyIs(B) and ctrl                            of TRUE: result := koBrighterPB; end;
  case keyDn and keyIs(B) and ctrl and shift                  of TRUE: result := koDarkerPB; end;
  case keyUp and keyIs(P)                                     of TRUE: result := koTogglePlaylist; end;
  case keyUp and keyIs(_0) and ctrl                           of TRUE: result := koCloseAll; end;
  case keyUp and keyIs(_9) and ctrl                           of TRUE: result := koArrangeAll; end;
  case keyUp and keyIs(V)                                     of TRUE: result := koSyncMedia; end;
  case keyUp and keyIs(VK_F5)                                 of TRUE: result := koScreenshot; end;
  case keyUp and keyIs(S) and ctrl                            of TRUE: result := koToggleSubtitles; end;
  case keyUp and keyIs(R) and ctrl                            of TRUE: result := koToggleRepeat; end;
  case keyUp and keyIs(E) and ctrl                            of TRUE: result := koToggleEditMode; end;
  case keyUp and keyIs(M)                                     of TRUE: result := koMaximize; end;
  case keyUp and keyIs(VK_F6)                                 of TRUE: result := koCycleAudio; end;
  case keyUp and keyIs(VK_F7)                                 of TRUE: result := koCycleSubs; end;
  case keyUp and keyIs(VK_F8)                                 of TRUE: result := koPrevChapter; end;
  case keyUp and keyIs(VK_F9)                                 of TRUE: result := koNextChapter; end;
end;

function TKeyboard.getAlt: boolean;
begin
  result := GV.altKeyDown;
  case result of TRUE: include(FShiftState, ssAlt); end;
end;

function TKeyboard.getCapsLock: boolean;
begin
  result := GetKeyState(VK_CAPITAL) <> 0;
end;

function TKeyboard.getCtrl: boolean;
begin
  result := ssCtrl in shiftState;
end;

function TKeyboard.getKeyDn: boolean;
begin
  result := FUpDn = kdDown;
end;

function TKeyboard.getKeyUp: boolean;
begin
  result := FUpDn = kdUp;
end;

function TKeyboard.getNumLock: boolean;
begin
  result := GetKeyState(VK_NUMLOCK) <> 0;
end;

function TKeyboard.getShift: boolean;
begin
  result := ssShift in shiftState;
end;

function TKeyboard.keyIs(const aChar: char): boolean;
begin
  result := key = ord(aChar);
end;

function TKeyboard.keyIs(const aKeyCode: word): boolean;
begin
  result := key = aKeyCode;
end;

procedure TKeyboard.formKeyDn(sender: TObject; var key: WORD; shift: TShiftState);
// keys that don't generate a standard WM_KEYUP message
begin
  GV.altKeyDown := ssAlt in shift;
  case GV.altKeyDown of TRUE: SA.postToAll(WIN_TABALT); end;
end;

procedure TKeyboard.formKeyUp(sender: TObject; var key: WORD; shift: TShiftState);
// keys that don't generate a standard WM_KEYUP message
begin
  GV.altKeyDown := NOT (key = VK_MENU);
  case key in [VK_F10] of TRUE: begin
                               postMessage(GV.appWnd, WM_KEY_UP, key, 0);
                               application.processMessages; end;end;
end;

function TKeyboard.processKeyStroke(var aKey: word; aShiftState: TShiftState; upDn: TKeyDirection): boolean;
begin
  result      := FALSE;

  FKey        := aKey;
  FShiftState := aShiftState;
  FUpDn       := upDn;

  case getKeyOp of
    koNone:       EXIT; // key not processed. bypass setting result to TRUE

    koCloseApp:          begin MP.dontPlayNext := TRUE; MP.stop; sendSysCommandClose(UI.handle); end;
    koVolUp:             ST.opInfo := MP.volUp;
    koVolDn:             ST.opInfo := MP.volDown;
    koTab:               SA.postToAll(WIN_TAB);
    koTabTab:            SA.postToAll(WIN_TABTAB);
    koPausePlay:         SA.postToAll(WIN_PAUSE_PLAY);
    koFrameForwards:     MP.frameForwards;
    koFrameBackwards:    MP.frameBackwards;
    koAdjustAspectRatio: UI.adjustAspectRatio(UI.handle, MP.videoWidth, MP.videoHeight);
    koBrightnessUp:      ST.opInfo := MP.brightnessUp;
    koBrightnessDn:      ST.opInfo := MP.brightnessDn;
    koZoomIn:            ST.opInfo := MP.zoomIn;
    koZoomOut:           ST.opInfo := MP.zoomOut;
    koStartOver:         SA.postToAll(WIN_RESTART);
    koShowCaption:       SA.postToAll(WIN_CAPTION);
    koMuteUnmute:        ST.opInfo := MP.muteUnmute;
    koPlayNext:          begin MP.playNext; UI.movePlaylistWindow(FALSE); end;
    koPlayPrev:          begin MP.playPrev; UI.movePlaylistWindow(FALSE); end;
    koPanLeft:           ST.opInfo := MP.panLeft(aShiftState);
    koPanRight:          ST.opInfo := MP.panRight(aShiftState);
    koPanUp:             ST.opInfo := MP.panUp(aShiftState);
    koPanDn:             ST.opInfo := MP.panDn(aShiftState);
    koRotateR:           ST.opInfo := MP.rotateRight;
    koRotateL:           ST.opInfo := MP.rotateLeft;
    koFullscreen:        case NOT GV.showingPlaylist AND NOT GV.showingTimeline of TRUE: MP.toggleFullscreen; end;
    koZoomReset:         ST.opInfo := MP.zoomReset;
    koGreaterWindow:     SA.postToAll(WIN_GREATER);
    koPlayFirst:         begin MP.playFirst; UI.movePlaylistWindow(FALSE); end;
    koPlayLast:          begin MP.playLast;  UI.movePlaylistWindow(FALSE); end;
    koToggleControls:    SA.postToAll(WIN_CONTROLS);
    koRunPot:            UI.openExternalApp(POT_PLAYER, PL.currentItem);
    koRunCut:            UI.openExternalApp(LOSSLESS_CUT, PL.currentItem);
    koRunShot:           UI.openExternalApp(SHOTCUT, PL.currentItem);
    koToggleBlackout:    UI.toggleBlackout;
    koCentreWindow:      begin UI.autoCentre := TRUE; postMessage(GV.appWnd, WM_USER_CENTRE_WINDOW, 0, 0); end;
    koMinimizeWindow:    UI.minimizeWindow;
    koDeleteCurrentItem: UI.deleteCurrentItem(aShiftState);
    koRenameFile:        UI.renameFile(PL.currentItem);
    koSpeedUp:           ST.opInfo := MP.speedUp;
    koSpeedDn:           ST.opInfo := MP.speedDn;
    koSpeedReset:        ST.opInfo := MP.speedReset;
    koEscape:            UI.doEscapeKey;
    koClipboard:         PL.copyToClipboard;
    koKeep:              UI.keepFile(PL.currentItem);
    koReloadPlaylist:    begin ST.opInfo := CU.reloadPlaylist(extractFilePath(PL.currentItem)); UI.reloadPlaylistWindow; end;
    koAlwaysPot:         MP.alwaysPot := NOT MP.alwaysPot;
    koPanReset:          ST.opInfo := MP.panReset;
    koBrightnessReset:   ST.opInfo := MP.brightnessReset;
    koBookmarkSave:      ST.opInfo := BM.save(PL.currentItem, MP.position);
    koBookmarkLoad:      case BM.asInteger(PL.currentItem) <> 0 of TRUE: begin MP.position := BM.asInteger(PL.currentItem); ST.opInfo := 'From bookmark'; end;end;
    koBookmarkDelete:    ST.opInfo := BM.delete(PL.currentItem);
    koRotateReset:       ST.opInfo := MP.rotateReset;
    koAllReset:          ST.opInfo := MP.allReset;
    koContrastUp:        ST.opInfo := MP.contrastUp;
    koContrastDn:        ST.opInfo := MP.contrastDn;
    koContrastReset:     ST.opInfo := MP.contrastReset;
    koGammaUp:           ST.opInfo := MP.gammaUp;
    koGammaDn:           ST.opInfo := MP.gammaDn;
    koSaturationUp:      ST.opInfo := MP.saturationUp;
    koSaturationDn:      ST.opInfo := MP.saturationDn;
    koGammaReset:        ST.opInfo := MP.gammaReset;
    koSaturationReset:   ST.opInfo := MP.saturationReset;
    koToggleHelp:        UI.toggleHelpWindow;
    koBrighterPB:        CU.brighter;
    koDarkerPB:          CU.darker;
    koCloseAll:          SA.postToAll(WIN_CLOSEAPP);
    koArrangeAll:        UI.arrangeAll;
    koSyncMedia:         SA.postToAllEx(WIN_SYNC_MEDIA, point(MP.position, 0));
    koTogglePlaylist:    UI.togglePlaylist;
    koScreenshot:        begin ST.opInfo := 'Screenshot...'; application.processMessages; ST.opInfo := MP.takeScreenshot; end;
    koToggleSubtitles:   ST.opInfo := MP.toggleSubtitles;
    koToggleRepeat:      ST.opInfo := MP.toggleRepeat;
    koToggleEditMode:    UI.toggleTimeline;
    koAboutBox:          UI.showAboutBox;
    koMaximize:          UI.maximize;
    koCycleAudio:        MP.cycleAudio;
    koCycleSubs:         MP.cycleSubs;
    koPrevChapter:       MP.chapterPrev;
    koNextChapter:       MP.chapterNext;
  end;

  result := TRUE;
end;

initialization
  gKB := NIL;

finalization
  case gKB <> NIL of TRUE: gKB.free; end;

end.
