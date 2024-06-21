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
unit mmpKeyboard;

interface

uses
  system.classes;

type
  TKeyOp = (koNone,
            koCloseApp, koVolUp, koVolDn, koTab, koTabTab, koPausePlay, koFrameForwards, koFrameBackwards, koBrightnessUp, koBrightnessDn,
            koZoomIn, koZoomOut, koStartOver, koShowCaption, koMuteUnmute, koPlayFirst, koPlayNext, koPlayPrev, koPlayLast, koPanLeft,
            koPanRight, koPanUp, koPanDn, koRotateR, koRotateL, koFullscreen, koZoomReset, koGreaterWindow, koToggleControls, koRunPot,
            koRunCut, koRunShot, koToggleBlackout, koCentreWindow, koMinimizeWindow, koDeleteCurrentItem, koRenameFile, koSpeedUp, koSpeedDn, koSpeedReset,
            koEscape, koClipboard, koKeep, koReloadPlaylist, koPanReset, koBrightnessReset, koBookmarkSave, koBookmarkLoad, koBookmarkDelete, koRotateReset,
            koContrastUp, koContrastDn, koContrastReset, koGammaUp, koGammaDn, koSaturationUp, koSaturationDn, koGammaReset, koSaturationReset, koAllReset,
            koToggleHelp, koBrighterPB, koDarkerPB, koTogglePlaylist, koCloseAll, koArrangeAll, koSyncMedia, koScreenshot, koToggleSubtitles, koToggleRepeat,
            koToggleEditMode, koAboutBox, koMaximize, koCycleAudio, koCycleSubs, koPrevChapter, koNextChapter, koThumbnails, koAdjustAspectRatio, koWiki,
            koToggleNumlock, koKeepDelete, koNextFolder, koPrevFolder, koImageInBrowser, koExploreFolder, koPBReset, koSysVolMax);
  TKeyDirection = (kdDn, kdUp);

function KBCapsLock: boolean;
function KBNumLock: boolean;
function KBProcessKeyStroke(const aKey: word; const aShiftState: TShiftState; const upDn: TKeyDirection): boolean;

implementation

uses
  winApi.shellApi, winApi.windows,
  system.sysUtils, system.types,
  vcl.forms,
  mmpConsts, mmpFileUtils, mmpKeyboardUtils, mmpPlaylistUtils, mmpShellUtils, mmpSingletons, mmpSysCommands, mmpUtils, mmpWindowCtrls,
  formAboutBox, formCaptions, formMediaCaption, formPlaylist, formThumbs,
  _debugWindow;

const
  A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
  N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';
  _0 = '0'; _1 = '1'; _2 = '2'; _3 = '3'; _4 = '4'; _5 = '5'; _6 = '6'; _7 = '7'; _8 = '8'; _9 = '9';
  _EQUALS = 187; SLASH = 191; BACKSLASH = 220; OPEN_BRACKET = 219; CLOSE_BRACKET = 221; HYPHEN = 189; HASH = 222; BACKSPACE = 8;
  OPEN_BRACE = 219; CLOSE_BRACE = 221; SINGLE_QUOTE = 192; SEMICOLON = 186;

function KBCapsLock: boolean;
begin
  result := GetKeyState(VK_CAPITAL) <> 0;
end;

function KBNumLock: boolean;
begin
  result := GetKeyState(VK_NUMLOCK) <> 0;
end;

function KBProcessKeyStroke(const aKey: word;  const aShiftState: TShiftState; const upDn: TKeyDirection): boolean;

  function ctrl: boolean;
  begin
    result := ssCtrl in aShiftState;
  end;

  function keyDn: boolean;
  begin
    result := upDn = kdDn;
  end;

  function keyUp: boolean;
  begin
    result := upDn = kdUp;
  end;

  function keyIs(const aKeyCode: word): boolean; overload;
  begin
    result := aKey = aKeyCode;
  end;

  function keyIs(const aChar: char): boolean; overload;
  begin
    result := aKey = ord(aChar);
  end;

  function shift: boolean;
  begin
    result := ssShift in aShiftState;
  end;

  function getKeyOp: TKeyOp;
  begin
    result := koNone;

    case keyUp and keyIs(_1)                                                          of TRUE: result := koSpeedReset; end;
    case keyUp and keyIs(_2)                                                          of TRUE: result := koContrastReset; end;
    case keyUp and keyIs(_3)                                                          of TRUE: result := koPanReset; end;
    case keyUp and keyIs(_4)                                                          of TRUE: result := koRotateReset; end;
    case keyUp and keyIs(_5)                                                          of TRUE: result := koBookmarkSave; end;
    case keyUp and keyIs(_6)                                                          of TRUE: result := koBookmarkLoad; end;
    case keyUp and keyIs(_7)                                                          of TRUE: result := koBookmarkDelete; end;
    case keyDn and keyIs(_8)                                                          of TRUE: result := koContrastDn; end;
    case keyUp and keyIs(_9) and     ctrl                                             of TRUE: result := koArrangeAll; end;
    case keyDn and keyIs(_9) and NOT ctrl                                             of TRUE: result := koContrastUp; end;
    case keyUp and keyIs(_0) and     ctrl                                             of TRUE: result := koCloseAll; end;
    case keyUp and keyIs(_0) and NOT ctrl                                             of TRUE: result := koBrightnessReset; end;
    case keyDn and keyIs(_EQUALS)                                                     of TRUE: result := koBrightnessUp; end;

    case keyUp and keyIs(A) and     ctrl                                              of TRUE: result := koAboutBox; end;
    case keyUp and keyIs(A) and NOT ctrl                                              of TRUE: result := koPlayFirst; end;
    case keyDn and keyIs(B) and     ctrl and NOT shift                                of TRUE: result := koBrighterPB; end;
    case keyDn and keyIs(B) and NOT ctrl and     shift                                of TRUE: result := koDarkerPB; end;
    case keyUp and keyIs(B) and     ctrl and     shift                                of TRUE: result := koPBReset; end;
    case keyUp and keyIs(B) and NOT ctrl and NOT shift                                of TRUE: result := koToggleBlackout; end;
    case keyUp and keyIs(C)                         and NOT GV.showingTimeline        of TRUE: result := koToggleControls; end;
    case keyDn and keyIs(D) and     ctrl                                              of TRUE: result := koPrevFolder; end;
    case keyDn and keyIs(D) and NOT ctrl                                              of TRUE: result := koNextFolder; end;
    case keyUp and keyIs(E) and     ctrl                                              of TRUE: result := koToggleEditMode; end;
    case keyUp and keyIs(E) and NOT ctrl                                              of TRUE: result := koMuteUnmute; end;
    case keyUp and keyIs(F) and     ctrl                                              of TRUE: result := koExploreFolder; end;
    case keyUp and keyIs(F) and NOT ctrl                                              of TRUE: result := koFullscreen; end;
    case keyDn and keyIs(G)                                                           of TRUE: result := koGreaterWindow; end;
    case keyUp and keyIs(H) and     ctrl                                              of TRUE: result := koToggleHelp; end;
    case keyUp and keyIs(H) and NOT ctrl                                              of TRUE: result := koCentreWindow; end;
    case keyDn and keyIs(I) and     ctrl            and (MP.mediaType = mtImage)      of TRUE: result := koImageInBrowser; end;
    case keyDn and keyIs(I) and NOT ctrl            and NOT GV.showingTimeline        of TRUE: result := koZoomIn; end;
    case keyUp and keyIs(J)                                                           of TRUE: result := koAdjustAspectRatio; end;
    case keyUp and keyIs(K) and     ctrl                                              of TRUE: result := koKeepDelete; end;
    case keyUp and keyIs(K) and NOT ctrl                                              of TRUE: result := koKeep; end;
    case keyUp and keyIs(L)                         and NOT GV.showingTimeline        of TRUE: result := koReloadPlaylist; end;
    case keyUp and keyIs(M)                         and NOT GV.showingTimeline        of TRUE: result := koMaximize; end;
    case keyUp and keyIs(N) and     ctrl            and NOT GV.showingTimeline        of TRUE: result := koToggleNumlock; end;
    case keyUp and keyIs(N) and NOT ctrl            and NOT GV.showingTimeline        of TRUE: result := koMinimizeWindow; end;
    case keyDn and keyIs(O)                         and NOT GV.showingTimeline        of TRUE: result := koZoomOut; end;
    case keyUp and keyIs(P)                                                           of TRUE: result := koTogglePlaylist; end;
    case keyUp and keyIs(Q)                                                           of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(R) and     ctrl                                              of TRUE: result := koToggleRepeat; end;
    case keyUp and keyIs(R) and NOT ctrl                                              of TRUE: result := koRenameFile; end;
    case keyUp and keyIs(S) and     ctrl                                              of TRUE: result := koToggleSubtitles; end;
    case keyUp and keyIs(S) and NOT ctrl            and NOT GV.showingTimeline        of TRUE: result := koStartOver; end;
    case keyUp and keyIs(T)                         and (MP.mediaType = mtImage)      of TRUE: result := koThumbnails; end;
    case keyDn and keyIs(T)                         and (MP.mediaType <> mtImage)     of TRUE: result := koTab; end;
    case keyUp and keyIs(U)                                                           of TRUE: result := koZoomReset; end;
    case keyUp and keyIs(V) and     ctrl                                              of TRUE: result := koSysVolMax; end;
    case keyUp and keyIs(V) and NOT ctrl                                              of TRUE: result := koSyncMedia; end;
    case keyUp and keyIs(W) and     ctrl                                              of TRUE: result := koWiki; end;
    case keyUp and keyIs(W) and NOT ctrl                                              of TRUE: result := koPlayNext; end;
    case keyUp and keyIs(X)                                                           of TRUE: result := koCloseApp; end;
    case keyUp and keyIs(Y)                                                           of TRUE: result := koThumbnails; end;
    case keyUp and keyIs(Z)                                                           of TRUE: result := koPlayLast; end;

    case keyDn and keyIs(BACKSLASH)                                                   of TRUE: result := koSpeedDn; end;
    case keyUp and keyIs(BACKSPACE)                                                   of TRUE: result := koAllReset; end;
    case keyDn and keyIs(CLOSE_BRACKET) and     shift                                 of TRUE: result := koSaturationUp; end; // close curly brace
    case keyDn and keyIs(CLOSE_BRACKET) and NOT shift                                 of TRUE: result := koGammaUp; end;      // close square bracket
    case keyUp and keyIs(HASH)                                                        of TRUE: result := koShowCaption; end;
    case keyDn and keyIs(HYPHEN)                                                      of TRUE: result := koBrightnessDn; end;
    case keyDn and keyIs(OPEN_BRACKET)  and     shift                                 of TRUE: result := koSaturationDn; end; // open curly brace
    case keyDn and keyIs(OPEN_BRACKET)  and NOT shift                                 of TRUE: result := koGammaDn; end;      // open square bracket
    case keyUp and keyIs(SEMICOLON)                                                   of TRUE: result := koSaturationReset; end;
    case keyUp and keyIs(SINGLE_QUOTE)                                                of TRUE: result := koGammaReset; end;
    case keyDn and keyIs(SLASH)                                                       of TRUE: result := koSpeedUp; end;

    case keyDn and keyIs(VK_ADD)                                                      of TRUE: result := koSpeedUp; end;
    case keyUp and keyIs(VK_DELETE)                                                   of TRUE: result := koDeleteCurrentItem; end;
    case keyDn and keyIs(VK_DOWN)   and     ctrl                                      of TRUE: result := koPanDn; end;
    case keyDn and keyIs(VK_DOWN)   and NOT ctrl    and NOT GV.showingPlaylist        of TRUE: result := koVolDn; end;
    case keyDn and keyIs(VK_DOWN)   and NOT ctrl    and NOT GV.showingPlaylist and NOT (MP.mediaType in [mtAudio, mtVideo]) of TRUE: result := koNextFolder; end;
    case keyUp and keyIs(VK_END)                                                      of TRUE: result := koPlayLast; end;
    case keyUp and keyIs(VK_ESCAPE)                                                   of TRUE: result := koEscape; end;
    case keyUp and keyIs(VK_F5)                                                       of TRUE: result := koScreenshot; end;
    case keyUp and keyIs(VK_F6)                                                       of TRUE: result := koCycleAudio; end;
    case keyUp and keyIs(VK_F7)                                                       of TRUE: result := koCycleSubs; end;
    case keyUp and keyIs(VK_F8)                                                       of TRUE: result := koPrevChapter; end;
    case keyUp and keyIs(VK_F9)                                                       of TRUE: result := koNextChapter; end;
    case keyUp and keyIs(VK_F10)                                                      of TRUE: result := koRunPot; end;
    case keyUp and keyIs(VK_F11)                                                      of TRUE: result := koRunCut; end;
    case keyUp and keyIs(VK_F12)                                                      of TRUE: result := koRunShot; end;
    case keyUp and keyIs(VK_HOME)                                                     of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(VK_INSERT)                                                   of TRUE: result := koClipboard; end;
    case keyDn and keyIs(VK_LEFT)   and     ctrl                                      of TRUE: result := koPanLeft; end;
    case keyDn and keyIs(VK_LEFT)   and NOT ctrl    and (MP.mediaType = mtVideo)      of TRUE: result := koFrameBackwards; end;
    case keyDn and keyIs(VK_LEFT)   and NOT ctrl    and (MP.mediaType <> mtVideo)     of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(VK_NEXT)                   and NOT GV.showingPlaylist        of TRUE: result := koRotateR; end;
    case keyDn and keyIs(VK_RETURN) and     ctrl    and NOT GV.showingPlaylist        of TRUE: result := koPlayPrev; end;
    case keyDn and keyIs(VK_RETURN) and NOT ctrl    and NOT GV.showingPlaylist        of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_RIGHT)  and     ctrl                                      of TRUE: result := koPanRight; end;
    case keyDn and keyIs(VK_RIGHT)  and NOT ctrl    and (MP.mediaType = mtVideo)      of TRUE: result := koFrameForwards; end;
    case keyDn and keyIs(VK_RIGHT)  and NOT ctrl    and (MP.mediaType <> mtVideo)     of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_UP)     and     ctrl                                      of TRUE: result := koPanUp; end;
    case keyDn and keyIs(VK_UP)     and NOT ctrl    and NOT GV.showingPlaylist        of TRUE: result := koVolUp; end;
    case keyDn and keyIs(VK_UP)     and NOT ctrl    and NOT GV.showingPlaylist and NOT (MP.mediaType in [mtAudio, mtVideo]) of TRUE: result := koPrevFolder; end;
    case keyUp and keyIs(VK_PRIOR)                  and NOT GV.showingPlaylist        of TRUE: result := koRotateL; end;
    case keyUp and keyIs(VK_SPACE)                                                    of TRUE: result := koPausePlay; end;
    case keyDn and keyIs(VK_SUBTRACT)                                                 of TRUE: result := koSpeedDn; end;
    case keyDn and keyIs(VK_TAB)                                                      of TRUE: result := koTabTab; end;
    case keyDn and keyIs(VK_VOLUME_DOWN)                                              of TRUE: result := koVolDn; end;
    case keyUp and keyIs(VK_VOLUME_MUTE)                                              of TRUE: result := koMuteUnmute; end;
    case keyDn and keyIs(VK_VOLUME_UP)                                                of TRUE: result := koVolUp; end;

    // spare keys
    case keyUp and keyIs(P) and ctrl                                                  of TRUE: result := koNone; end;
    case keyUp and keyIs(VK_F1)                                                       of TRUE: result := koNone; end;
  end;

begin
  result      := FALSE;

  case getKeyOp of
    koNone:       EXIT; // key not processed. bypass setting result to TRUE

    koAboutBox:          showAboutBox;
    koAdjustAspectRatio: case MP.mediaType = mtImage of TRUE: UI.adjustAspectRatio; end;
    koAllReset:          ST.opInfo := MP.allReset;
    koArrangeAll:        UI.arrangeAll;
    koBookmarkDelete:    ST.opInfo := BM.delete(PL.currentItem);
    koBookmarkLoad:      case BM.asInteger(PL.currentItem) <> 0 of TRUE: begin MP.position := BM.asInteger(PL.currentItem); ST.opInfo := 'From bookmark'; end;end;
    koBookmarkSave:      ST.opInfo := BM.save(PL.currentItem, MP.position);
    koBrighterPB:        begin CF['caption'] := CF.toHex(MC.brighter); CF['timeCaption'] := CF.toHex(ST.brighter); CF['progressBar'] := CF.toHex(PB.brighter); end;
    koBrightnessDn:      ST.opInfo := MP.brightnessDn;
    koBrightnessUp:      ST.opInfo := MP.brightnessUp;
    koBrightnessReset:   ST.opInfo := MP.brightnessReset;
    koCentreWindow:      begin GV.autoCentre := TRUE; postMessage(GV.appWnd, WM_USER_CENTRE_WINDOW, 0, 0); end;
    koClipboard:         PL.copyToClipboard;
    koCloseAll:          begin MP.ceaseOps; mmpPostToAll(WIN_CLOSEAPP, TRUE); end;
    koCloseApp:          begin MP.ceaseOps; mmpSendSysCommandClose; end;
    koContrastDn:        ST.opInfo := MP.contrastDn;
    koContrastUp:        ST.opInfo := MP.contrastUp;
    koContrastReset:     ST.opInfo := MP.contrastReset;
    koCycleAudio:        MP.cycleAudio;
    koCycleSubs:         MP.cycleSubs;
    koDarkerPB:          UI.darker;
    koDeleteCurrentItem: mmpDeleteCurrentItem(PL, MP, aShiftState, CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY]);
    koEscape:            UI.doEscapeKey;
    koExploreFolder:     mmpShellExec(PL.currentFolder, '');
    koFrameBackwards:    MP.frameBackwards;
    koFrameForwards:     MP.frameForwards;
    koFullscreen:        case NOT GV.showingPlaylist AND NOT GV.showingTimeline of TRUE: MP.toggleFullscreen; end;
    koGammaDn:           ST.opInfo := MP.gammaDn;
    koGammaReset:        ST.opInfo := MP.gammaReset;
    koGammaUp:           ST.opInfo := MP.gammaUp;
    koGreaterWindow:     mmpPostToAll(WIN_GREATER, KBNumLock);
    koKeep:              UI.keepFile(PL.currentItem);
    koKeepDelete:        begin MP.ceaseOps; case mmpKeepDelete(PL.currentFolder) of TRUE: mmpSendSysCommandClose; end;end;
    koImageInBrowser:    UI.showThumbnails(htMPVHost);
    koMaximize:          UI.maximize;
    koMinimizeWindow:    UI.minimizeWindow;
    koMuteUnmute:        ST.opInfo := MP.muteUnmute;
    koNextChapter:       MP.chapterNext;
    koNextFolder:        MP.playNextFolder;
    koPanDn:             ST.opInfo := MP.panDn;
    koPanLeft:           ST.opInfo := MP.panLeft;
    koPanReset:          ST.opInfo := MP.panReset;
    koPanRight:          ST.opInfo := MP.panRight;
    koPanUp:             ST.opInfo := MP.panUp;
    koPausePlay:         mmpPostToAll(WIN_PAUSE_PLAY, KBNumlock);
    koPBReset:           UI.resetColor;
    koPlayNext:          MP.playNext;
    koPlayPrev:          MP.playPrev;
    koPrevFolder:        MP.playPrevFolder;
    koRenameFile:        UI.renameFile(PL.currentItem);
    koRotateL:           ST.opInfo := MP.rotateLeft;
    koRotateR:           ST.opInfo := MP.rotateRight;
    koRotateReset:       ST.opInfo := MP.rotateReset;
    koShowCaption:       mmpPostToAll(WIN_CAPTION, KBNumLock);
    koSpeedDn:           ST.opInfo := MP.speedDn;
    koSpeedReset:        ST.opInfo := MP.speedReset;
    koSpeedUp:           ST.opInfo := MP.speedUp;
    koStartOver:         mmpPostToAll(WIN_RESTART, KBNumLock);
    koSysVolMax:         ST.opInfo := MX.setSysVolMax;
    koTab:               mmpPostToAll(WIN_TAB, KBNumLock);
    koTabTab:            mmpPostToAll(WIN_TABTAB, KBNumLock);
    koPlayFirst:         MP.playFirst;
    koPlayLast:          MP.playLast;
    koPrevChapter:       MP.chapterPrev;
    koReloadPlaylist:    begin ST.opInfo := mmpReloadPlaylist(PL, MP); MC.caption := PL.formattedItem; loadPlaylistWindow(TRUE); end;
    koRunCut:            mmpOpenExternalApp(F11_APP, PL.currentItem);
    koRunPot:            mmpOpenExternalApp(F10_APP, PL.currentItem);
    koRunShot:           mmpOpenExternalApp(F12_APP, PL.currentItem);
    koSaturationDn:      ST.opInfo := MP.saturationDn;
    koSaturationReset:   ST.opInfo := MP.saturationReset;
    koSaturationUp:      ST.opInfo := MP.saturationUp;
    koScreenshot:        begin ST.opInfo := 'Screenshot...'; mmpProcessMessages; ST.opInfo := MP.takeScreenshot; end;
    koSyncMedia:         mmpPostToAllEx(WIN_SYNC_MEDIA, point(MP.position, 0), TRUE);
    koThumbnails:        UI.showThumbnails;
    koToggleBlackout:    UI.toggleBlackout;
    koToggleControls:    mmpPostToAll(WIN_CONTROLS, KBNumLock);
    koToggleEditMode:    UI.toggleTimeline;
    koToggleHelp:        UI.toggleHelpWindow;
    koToggleNumlock:     mmpToggleNumlock;
    koTogglePlaylist:    UI.togglePlaylist;
    koToggleRepeat:      ST.opInfo := MP.toggleRepeat;
    koToggleSubtitles:   ST.opInfo := MP.toggleSubtitles;
    koVolDn:             ST.opInfo := MP.volDown;
    koVolUp:             ST.opInfo := MP.volUp;
    koWiki:              mmpShellExec('https://minimalistmediaplayer.com');
    koZoomIn:            ST.opInfo := MP.zoomIn;
    koZoomOut:           ST.opInfo := MP.zoomOut;
    koZoomReset:         ST.opInfo := MP.zoomReset;
  end;

  result := TRUE;
end;

end.
