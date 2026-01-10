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
unit view.mmpKeyboardMain;

interface

uses
  system.classes,
  mmpConsts, mmpGlobalState;

function KBProcessKeyStroke(const SS: TSnapshot): TKeyOp;

implementation

uses
  winApi.shellApi, winApi.windows,
  system.sysUtils, system.types,
  vcl.Dialogs,
  _debugWindow;

const
  A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
  N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';
  _0 = '0'; _1 = '1'; _2 = '2'; _3 = '3'; _4 = '4'; _5 = '5'; _6 = '6'; _7 = '7'; _8 = '8'; _9 = '9';
  _EQUALS = 187; SLASH = 191; BACKSLASH = 220; OPEN_BRACKET = 219; CLOSE_BRACKET = 221; HYPHEN = 189; HASH = 222; BACKSPACE = 8;
  OPEN_BRACE = 219; CLOSE_BRACE = 221; SINGLE_QUOTE = 192; SEMICOLON = 186; TICK = 223;

function KBCapsLock: boolean;
begin
  result := getKeyState(VK_CAPITAL) <> 0;
end;

function KBNumLock: boolean;
begin
  result := getKeyState(VK_NUMLOCK) <> 0;
end;

function KBProcessKeyStroke(const SS: TSnapshot): TKeyOp;

  function ctrl: boolean;
  begin
    result := ssCtrl in SS.shiftState;
  end;

  function keyDn: boolean;
  begin
    result := SS.keyDirection = kdDn;
  end;

  function keyUp: boolean;
  begin
    result := SS.keyDirection = kdUp;
  end;

  function keyIs(const aKeyCode: word): boolean; overload;
  begin
    result := SS.key = aKeyCode;
  end;

  function keyIs(const aChar: char): boolean; overload;
  begin
    result := SS.key = ord(aChar);
  end;

  function shift: boolean;
  begin
    result := ssShift in SS.shiftState;
  end;

  function getKeyOp: TKeyOp;
  // check keyUp for keys which should be pressed once
  // check keyDn for keys which can be held down for rapid repeat
  begin
    result := koNone;
    case GS.showingThumbs of TRUE: EXIT; end;

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
    case keyUp and keyIs(_0) and     ctrl                                             of TRUE: result := koCloseEvery; end;
    case keyUp and keyIs(_0) and NOT ctrl                                             of TRUE: result := koBrightnessReset; end;
    case keyDn and keyIs(_EQUALS)                                                     of TRUE: result := koBrightnessUp; end;

    case keyUp and keyIs(A) and     ctrl                                              of TRUE: result := koAboutBox; end;
    case keyUp and keyIs(A) and NOT ctrl                                              of TRUE: result := koPlayFirst; end;
    case keyDn and keyIs(B) and     ctrl and NOT shift                                of TRUE: result := koBrighterPB; end;
    case keyDn and keyIs(B) and NOT ctrl and     shift                                of TRUE: result := koDarkerPB; end;
    case keyUp and keyIs(B) and     ctrl and     shift                                of TRUE: result := koPBReset; end;
    case keyUp and keyIs(B) and NOT ctrl and NOT shift                                of TRUE: result := koToggleProgressBar; end;
    case keyUp and keyIs(C)                                                           of TRUE: result := koToggleControls; end; // TCaptionsForm.toggleCaptions checks ctrl key (so does TTimeline.validKey)
    case keyUp and keyIs(C) and     ctrl and     shift                                of TRUE: result := koCleanup; end;        // This overrides the previous line
    case keyDn and keyIs(D) and     ctrl                                              of TRUE: result := koPlayPrevFolder; end;
    case keyDn and keyIs(D) and NOT ctrl                                              of TRUE: result := koPlayNextFolder; end;
    case keyUp and keyIs(E) and     ctrl and NOT shift                                of TRUE: result := koToggleEditMode; end;
    case keyUp and keyIs(E) and NOT ctrl and NOT shift                                of TRUE: result := koMuteUnmute; end;
    case keyUp and keyIs(E) and NOT ctrl and     shift  and     GS.showingTimeline    of TRUE: result := koPlayEdited; end;
    case keyUp and keyIs(F) and     ctrl                                              of TRUE: result := koExploreFolder; end;
    case keyUp and keyIs(F) and NOT ctrl                and NOT GS.showingTimeline    of TRUE: result := koFullscreen; end;
    case keyDn and keyIs(G)                                                           of TRUE: result := koGreaterWindow; end;
    case keyUp and keyIs(H) and     ctrl                                              of TRUE: result := koToggleHelp; end;
    case keyUp and keyIs(H) and NOT ctrl                                              of TRUE: result := koCentreWindow; end;
    case keyDn and keyIs(I) and     ctrl                and (GS.mediaType = mtImage)  of TRUE: result := koImageInBrowser; end;
    case keyDn and keyIs(I) and NOT ctrl                and NOT GS.showingTimeline    of TRUE: result := koZoomIn; end;
    case keyUp and keyIs(J)                                                           of TRUE: result := koAdjustAspectRatio; end;
    case keyUp and keyIs(K) and     ctrl                                              of TRUE: result := koKeepDelete; end;
    case keyUp and keyIs(K) and NOT ctrl                                              of TRUE: result := koKeep; end;
    case keyUp and keyIs(L) and NOT ctrl                and NOT GS.showingTimeline    of TRUE: result := koReloadPlaylist; end;
    case keyUp and keyIs(M) and     ctrl                                              of TRUE: result := koKeepMove; end;
    case keyUp and keyIs(M) and NOT ctrl and     shift                                of TRUE: result := koKeepMove; end;
    case keyUp and keyIs(M) and NOT ctrl and NOT shift  and NOT GS.showingTimeline    of TRUE: result := koMaximize; end;
    case keyUp and keyIs(N) and     ctrl                and NOT GS.showingTimeline    of TRUE: result := koToggleNumlock; end;
    case keyUp and keyIs(N) and NOT ctrl                and NOT GS.showingTimeline    of TRUE: result := koMinimizeWindow; end;
    case keyDn and keyIs(O)                             and NOT GS.showingTimeline    of TRUE: result := koZoomOut; end;
    case keyUp and keyIs(P)                                                           of TRUE: result := koTogglePlaylist; end;
    case keyUp and keyIs(P) and     ctrl                and NOT GS.showingTimeline    of TRUE: result := koToggleFiltering; end;
    case keyUp and keyIs(Q)                                                           of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(R) and     ctrl and NOT shift  and NOT GS.showingTimeline    of TRUE: result := koToggleRepeat; end;
    case keyUp and keyIs(R) and NOT ctrl and NOT shift  and NOT GS.showingTimeline    of TRUE: result := koRenameFile; end;
    case keyUp and keyIs(R) and     ctrl and NOT shift  and     GS.showingTimeline    of TRUE: result := koRenameFile; end;
    case keyUp and keyIs(R) and     ctrl and     shift                                of TRUE: result := koRenameCleanFile; end;
    case keyUp and keyIs(S) and NOT ctrl and     shift                                of TRUE: result := koKeepSave; end;
    case keyUp and keyIs(S) and     ctrl and NOT shift                                of TRUE: result := koToggleSubtitles; end;
    case keyUp and keyIs(S) and NOT ctrl and NOT shift  and NOT GS.showingTimeline    of TRUE: result := koStartOver; end;
    case keyUp and keyIs(S) and     ctrl                and     GS.showingTimeline    of TRUE: result := koToggleSkipExcluded; end;
    case keyUp and keyIs(T)                             and (GS.mediaType = mtImage)  of TRUE: result := koThumbnails; end;
    case keyDn and keyIs(T)                             and (GS.mediaType <> mtImage) of TRUE: result := koTab; end;
    case keyUp and keyIs(U)                                                           of TRUE: result := koZoomReset; end;
    case keyUp and keyIs(V) and     ctrl                                              of TRUE: result := koSysVolMax; end;
    case keyUp and keyIs(V) and NOT ctrl                                              of TRUE: result := koSyncMedia; end;
    case keyUp and keyIs(W) and     ctrl                                              of TRUE: result := koWiki; end;
    case keyUp and keyIs(W) and NOT ctrl                                              of TRUE: result := koPlayNext; end;
    case keyUp and keyIs(X)                                                           of TRUE: result := koCloseApp; end;
    case keyUp and keyIs(Y)                                                           of TRUE: result := koThumbnails; end;
    case keyUp and keyIs(Z)                                                           of TRUE: result := koPlayLast; end;

    case keyUp and keyIs(BACKSLASH)      and     shift                                of TRUE: result := koConfig; end;
    case keyDn and keyIs(BACKSLASH)      and NOT shift                                of TRUE: result := koSpeedDn; end;
    case keyUp and keyIs(BACKSPACE)                                                   of TRUE: result := koResetAll; end;
    case keyDn and keyIs(CLOSE_BRACKET)  and     shift                                of TRUE: result := koSaturationUp; end; // close curly brace
    case keyDn and keyIs(CLOSE_BRACKET)  and NOT shift                                of TRUE: result := koGammaUp; end;      // close square bracket
    case keyUp and keyIs(HASH)                                                        of TRUE: result := koShowCaption; end;
    case keyDn and keyIs(HYPHEN)                                                      of TRUE: result := koBrightnessDn; end;
    case keyDn and keyIs(OPEN_BRACKET)   and     shift                                of TRUE: result := koSaturationDn; end; // open curly brace
    case keyDn and keyIs(OPEN_BRACKET)   and NOT shift                                of TRUE: result := koGammaDn; end;      // open square bracket
    case keyUp and keyIs(SEMICOLON)                                                   of TRUE: result := koSaturationReset; end;
    case keyUp and keyIs(SINGLE_QUOTE)                                                of TRUE: result := koGammaReset; end;
    case keyDn and keyIs(SLASH)                                                       of TRUE: result := koSpeedUp; end;
    case keyUp and keyIs(TICK)                                                        of TRUE: result := koStartOver; end;

    case keyDn and keyIs(VK_ADD)                                                      of TRUE: result := koSpeedUp; end;
    case keyUp and keyIs(VK_DELETE)                                                   of TRUE: result := koDeleteCurrentItem; end;
    case keyDn and keyIs(VK_DOWN)   and     ctrl                                      of TRUE: result := koPanDn; end;
    case keyDn and keyIs(VK_DOWN)   and NOT ctrl        and NOT GS.showingPlaylist    of TRUE: result := koVolDn; end;
    case keyDn and keyIs(VK_DOWN)   and NOT ctrl        and NOT GS.showingPlaylist
                                                        and NOT (GS.mediaType in [mtAudio, mtVideo])
                                                                                      of TRUE: result := koPlayNextFolder; end;
    case keyUp and keyIs(VK_END)                                                      of TRUE: result := koPlayLast; end;
    case keyUp and keyIs(VK_ESCAPE)                     and NOT GS.userInput          of TRUE: result := koEscape; end;
    case keyUp and keyIs(VK_F1)                                                       of TRUE: result := koKeepCatF1; end;
    case keyUp and keyIs(VK_F2)                                                       of TRUE: result := koKeepCatF2; end;
    case keyUp and keyIs(VK_F3)                                                       of TRUE: result := koKeepCatF3; end;
    case keyUp and keyIs(VK_F4)                                                       of TRUE: result := koKeepCatF4; end;
    case keyUp and keyIs(VK_F5)                                                       of TRUE: result := koScreenshot; end;
    case keyUp and keyIs(VK_F6)                                                       of TRUE: result := koCycleAudio; end;
    case keyUp and keyIs(VK_F7)                                                       of TRUE: result := koCycleSubs; end;
    case keyUp and keyIs(VK_F8)                                                       of TRUE: result := koPrevChapter; end;
    case keyUp and keyIs(VK_F9)                                                       of TRUE: result := koNextChapter; end;
    case keyUp and keyIs(VK_F10)                                                      of TRUE: result := koRunPot; end;
    case keyUp and keyIs(VK_F11)                                                      of TRUE: result := koRunCut; end;
    case keyUp and keyIs(VK_F12)                                                      of TRUE: result := koRunShot; end;
    case keyUp and keyIs(VK_HOME)   and NOT ctrl and NOT shift                        of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(VK_HOME)   and NOT ctrl and     shift                        of TRUE: result := koToggleShuffle; end;
    case keyUp and keyIs(VK_HOME)   and     ctrl and NOT shift                        of TRUE: result := koStartOver; end;
    case keyUp and keyIs(VK_INSERT)                                                   of TRUE: result := koClipboard; end;
    case keyDn and keyIs(VK_LEFT)   and     ctrl                                      of TRUE: result := koPanLeft; end;
    case keyDn and keyIs(VK_LEFT)   and NOT ctrl and NOT shift and (GS.mediaType = mtVideo)  of TRUE: result := koFrameBackwards; end;
    case keyDn and keyIs(VK_LEFT)   and NOT ctrl        and (GS.mediaType <> mtVideo) of TRUE: result := koPlayPrev; end;
    case keyDn and keyIs(VK_NEXT)                       and NOT GS.showingPlaylist    of TRUE: result := koRotateR; end;
    case keyUp and keyIs(VK_RETURN) and     ctrl        and NOT GS.showingPlaylist    of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(VK_RETURN) and NOT ctrl        and NOT GS.showingPlaylist    of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_RIGHT)  and     ctrl                                      of TRUE: result := koPanRight; end;
    case keyDn and keyIs(VK_RIGHT)  and NOT ctrl and NOT shift and (GS.mediaType = mtVideo)  of TRUE: result := koFrameForwards; end;
    case keyDn and keyIs(VK_RIGHT)  and NOT ctrl        and (GS.mediaType <> mtVideo) of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_UP)     and     ctrl                                      of TRUE: result := koPanUp; end;
    case keyDn and keyIs(VK_UP)     and NOT ctrl        and NOT GS.showingPlaylist    of TRUE: result := koVolUp; end;
    case keyDn and keyIs(VK_UP)     and NOT ctrl        and NOT GS.showingPlaylist
                                                        and NOT (GS.mediaType in [mtAudio, mtVideo])
                                                                                      of TRUE: result := koPlayPrevFolder; end;
    case keyDn and keyIs(VK_PRIOR)                      and NOT GS.showingPlaylist    of TRUE: result := koRotateL; end;
    case keyUp and keyIs(VK_SPACE)                                                    of TRUE: result := koPausePlay; end;
    case keyDn and keyIs(VK_SUBTRACT)                                                 of TRUE: result := koSpeedDn; end;
    case keyDn and keyIs(VK_TAB)                                                      of TRUE: result := koTabTab; end;
    case keyDn and keyIs(VK_VOLUME_DOWN)                                              of TRUE: result := koVolDn; end;
    case keyUp and keyIs(VK_VOLUME_MUTE)                                              of TRUE: result := koMuteUnmute; end;
    case keyDn and keyIs(VK_VOLUME_UP)                                                of TRUE: result := koVolUp; end;
  end;

begin
  result      := getKeyOp;
end;

end.
