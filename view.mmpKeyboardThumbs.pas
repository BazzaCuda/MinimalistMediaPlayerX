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
unit view.mmpKeyboardThumbs;

interface

uses
  system.classes,
  MPVBasePlayer;

type
  TKeyOp = (koNone,
            koCloseApp, koPausePlay, koBrightnessUp, koBrightnessDn, koZoomIn, koZoomOut, koSaveMove, koShowCaption, koPlayFirst, koPlayNext,
            koPlayPrev, koPlayLast, koPanLeft, koPanRight, koPanUp, koPanDn, koRotateR, koRotateL, koFullscreen, koZoomReset,
            koGreaterWindow, koToggleControls, koToggleBlackout, koCentreWindow, koMinimizeWindow, koDeleteCurrentItem, koRenameFile, koSpeedUp, koSpeedDn, koSpeedReset,
            koCloseImageBrowser, koClipboard, koKeep, koReloadPlaylist, koPanReset, koBrightnessReset, koRotateReset, koContrastUp, koContrastDn, koContrastReset,
            koGammaUp, koGammaDn, koSaturationUp, koSaturationDn, koGammaReset, koSaturationReset, koAllReset, koToggleHelp, koBrighterPB, koDarkerPB,
            koCloseAll, koScreenshot, koAboutBox, koMaximize, koPlayThumbs, koNextFolder, koPrevFolder, koSaveCopy, koMoveToKeyFolder, koThumbsUp,
            koThumbsDn, koAdjustAspectRatio, koWindowShorter, koWindowTaller, koWindowNarrower, koWindowWider, koUndoMove, koReverseSlideshow, koWiki, koToggleNumlock,
            koKeepDelete, koExploreFolder, koCloseToMain, koConfig, koRenameCleanFile, koHelpFull
            );
  TKeyDirection = (kdDn, kdUp);

function processKeyStroke(const mpv: IMPVBasePlayer; const aKey: word; const aShiftState: TShiftState; const upDn: TKeyDirection): TKeyOp;

implementation

uses
  winApi.windows,
  bazCmd,
  mmpNotify.notices,
  mmpGlobalState,
  _debugWindow;

const
  A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
  N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';
  _0 = '0'; _1 = '1'; _2 = '2'; _3 = '3'; _4 = '4'; _5 = '5'; _6 = '6'; _7 = '7'; _8 = '8'; _9 = '9';
  _EQUALS = 187; SLASH = 191; BACKSLASH = 220; OPEN_BRACKET = 219; CLOSE_BRACKET = 221; HYPHEN = 189; HASH = 222; BACKSPACE = 8;
  OPEN_BRACE = 219; CLOSE_BRACE = 221; SINGLE_QUOTE = 192; SEMICOLON = 186;

function processKeyStroke(const mpv: IMPVBasePlayer; const aKey: word; const aShiftState: TShiftState; const upDn: TKeyDirection): TKeyOp;

  function Ctrl: boolean;
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
  // check keyUp for keys which should be pressed once
  // check keyDn for keys which can be held down for rapid repeat
  begin
    result := koNone;

    case keyUp and keyIs(_1)                                                    of TRUE: result := koSpeedReset; end;
    case keyUp and keyIs(_2)                                                    of TRUE: result := koContrastReset; end;
    case keyUp and keyIs(_3)                                                    of TRUE: result := koPanReset; end;
    case keyUp and keyIs(_4)                                                    of TRUE: result := koRotateReset; end;
    case keyDn and keyIs(_8)                                                    of TRUE: result := koContrastDn; end;
    case keyDn and keyIs(_9)          and NOT ctrl                              of TRUE: result := koContrastUp; end;
    case keyUp and keyIs(_0)          and     ctrl                              of TRUE: result := koCloseAll; end;
    case keyUp and keyIs(_0)          and NOT ctrl                              of TRUE: result := koBrightnessReset; end;
    case keyDn and keyIs(_EQUALS)                                               of TRUE: result := koBrightnessUp; end;

    case keyUp and keyIs(A)           and     ctrl                              of TRUE: result := koAboutBox; end;
    case keyUp and keyIs(A)           and NOT ctrl                              of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(C)                                                     of TRUE: result := koSaveCopy; end;
    case keyUp and keyIs(D)           and     ctrl                              of TRUE: result := koPrevFolder; end;
    case keyUp and keyIs(D)           and NOT ctrl                              of TRUE: result := koNextFolder; end;
    case keyUp and keyIs(E)                                                     of TRUE: result := koPrevFolder; end;
    case keyUp and keyIs(F)           and     ctrl                              of TRUE: result := koExploreFolder; end;
    case keyUp and keyIs(F)           and NOT ctrl                              of TRUE: result := koFullscreen; end;
    case keyDn and keyIs(G)                                                     of TRUE: result := koGreaterWindow; end;
    case keyUp and keyIs(H)           and     ctrl  and NOT shift               of TRUE: result := koToggleHelp; end;
    case keyUp and keyIs(H)           and     ctrl  and     shift               of TRUE: result := koHelpFull; end;
    case keyUp and keyIs(H)           and NOT ctrl  and NOT shift               of TRUE: result := koCentreWindow; end;
    case keyDn and keyIs(I)                                                     of TRUE: result := koZoomIn; end;
    case keyUp and keyIs(J)                                                     of TRUE: result := koAdjustAspectRatio; end;
    case keyUp and keyIs(K)           and     ctrl                              of TRUE: result := koKeepDelete; end;
    case keyUp and keyIs(K)           and NOT ctrl                              of TRUE: result := koKeep; end;
    case keyUp and keyIs(M)                                                     of TRUE: result := koMaximize; end;
    case keyUp and keyIs(L)                                                     of TRUE: result := koReloadPlaylist; end;
    case keyUp and keyIs(N)           and     ctrl                              of TRUE: result := koToggleNumlock; end;
    case keyUp and keyIs(N)           and NOT ctrl                              of TRUE: result := koMinimizeWindow; end;
    case keyDn and keyIs(O)                                                     of TRUE: result := koZoomOut; end;
    case keyUp and keyIs(Q)                                                     of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(R)           and NOT ctrl                              of TRUE: result := koRenameFile; end;
    case keyUp and keyIs(R)           and     ctrl  and     shift               of TRUE: result := koRenameCleanFile; end;
    case keyUp and keyIs(S)           and     ctrl                              of TRUE: result := koScreenshot; end; // in Image Browser, all F-keys are reserved
    case keyUp and keyIs(S)           and NOT ctrl                              of TRUE: result := koSaveMove; end;
    case keyDn and keyIs(T)                                                     of TRUE: result := koPlayThumbs; end;
    case keyUp and keyIs(U)           and     ctrl                              of TRUE: result := koUndoMove; end;
    case keyUp and keyIs(U)           and NOT ctrl                              of TRUE: result := koZoomReset; end;
    case keyUp and keyIs(W)           and     ctrl                              of TRUE: result := koWiki; end;
    case keyUp and keyIs(W)           and NOT ctrl                              of TRUE: result := koPlayNext; end;
    case keyUp and keyIs(X)                                                     of TRUE: result := koCloseImageBrowser; end;
    case keyUp and keyIs(X)           and     ctrl                              of TRUE: result := koCloseToMain; end;
    case keyUp and keyIs(Y)                                                     of TRUE: result := koPlayThumbs; end;
    case keyUp and keyIs(Z)                                                     of TRUE: result := koPlayLast; end;

    case keyDn and keyIs(BACKSLASH)                 and     shift               of TRUE: result := koConfig; end;
    case keyDn and keyIs(BACKSLASH)                 and NOT shift               of TRUE: result := koSpeedDn; end;
    case keyUp and keyIs(BACKSPACE)                                             of TRUE: result := koAllReset; end;
    case keyDn and keyIs(CLOSE_BRACKET)             and     shift               of TRUE: result := koSaturationUp; end; // close curly brace
    case keyDn and keyIs(CLOSE_BRACKET)             and NOT shift               of TRUE: result := koGammaUp; end;
    case keyDn and keyIs(HYPHEN)                                                of TRUE: result := koBrightnessDn; end;
    case keyDn and keyIs(OPEN_BRACKET)              and     shift               of TRUE: result := koSaturationDn; end; // open curly brace
    case keyDn and keyIs(OPEN_BRACKET)              and NOT shift               of TRUE: result := koGammaDn; end;
    case keyUp and keyIs(SEMICOLON)                                             of TRUE: result := koSaturationReset; end;
    case keyUp and keyIs(SINGLE_QUOTE)                                          of TRUE: result := koGammaReset; end;
    case keyDn and keyIs(SLASH)                                                 of TRUE: result := koSpeedUp; end;

    case keyDn and keyIs(VK_ADD)      and     ctrl                              of TRUE: result := koWindowWider; end;
    case keyDn and keyIs(VK_ADD)      and NOT ctrl                              of TRUE: result := koWindowTaller; end;
    case keyUp and keyIs(VK_DELETE)                                             of TRUE: result := koDeleteCurrentItem; end;
    case keyDn and keyIs(VK_DOWN)     and     ctrl                              of TRUE: result := koPanDn; end; // can also be ctrl-shift
    case keyDn and keyIs(VK_DOWN)     and NOT ctrl  and     shift               of TRUE: result := koThumbsDn; end;
    case keyDn and keyIs(VK_DOWN)     and NOT ctrl  and NOT shift               of TRUE: result := koNextFolder; end;
    case keyUp and keyIs(VK_END)                                                of TRUE: result := koPlayLast; end;
//    case keyUp and keyIs(VK_ESCAPE)                 and     GS.helpFull         of TRUE: result := koHelpFull; end; // close Help Full  - this is handled in mmpVM.onKeyUp
    case keyUp and keyIs(VK_ESCAPE)   and NOT GS.ignoreEscape and NOT GS.userInput  of TRUE: result := koCloseImageBrowser; end;
    case keyUp and keyIs(VK_HOME)                                               of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(VK_INSERT)                                             of TRUE: result := koClipboard; end;
    case keyDn and keyIs(VK_LEFT)     and     ctrl                              of TRUE: result := koPanLeft; end;
    case keyDn and keyIs(VK_LEFT)     and NOT ctrl                              of TRUE: result := koPlayPrev; end;
    case keyDn and keyIs(VK_NEXT)                  and NOT GS.showingPlaylist   of TRUE: result := koRotateR; end;
    case keyDn and keyIs(VK_PRIOR)                 and NOT GS.showingPlaylist   of TRUE: result := koRotateL; end;
    case keyUp and keyIs(VK_RETURN)   and     ctrl and NOT GS.showingPlaylist   of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(VK_RETURN)   and NOT ctrl and NOT GS.showingPlaylist and NOT GS.userInput   of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_RIGHT)    and     ctrl                              of TRUE: result := koPanRight; end;
    case keyDn and keyIs(VK_RIGHT)    and NOT ctrl                              of TRUE: result := koPlayNext; end;
    case keyUp and keyIs(VK_SPACE)    and     ctrl                              of TRUE: result := koReverseSlideshow; end;
    case keyUp and keyIs(VK_SPACE)    and NOT ctrl                              of TRUE: result := koPausePlay; end;
    case keyDn and keyIs(VK_SUBTRACT) and     ctrl                              of TRUE: result := koWindowNarrower; end;
    case keyDn and keyIs(VK_SUBTRACT) and NOT ctrl                              of TRUE: result := koWindowShorter; end;
    case keyDn and keyIs(VK_UP)       and     ctrl                              of TRUE: result := koPanUp; end; // can also be ctrl-shift
    case keyDn and keyIs(VK_UP)       and NOT ctrl and     shift                of TRUE: result := koThumbsUp; end;
    case keyDn and keyIs(VK_UP)       and NOT ctrl and NOT shift                of TRUE: result := koPrevFolder; end;

    case keyUp and (aKey in [VK_F1..VK_F12])                                    of TRUE: result := koMoveToKeyFolder; end;

// spare keys
//    case keyUp and keyIs(R) and ctrl                            of TRUE: result := koNone; end;
//    case keyDn and keyIs(VK_TAB)                                of TRUE: result := koNone; end;
//    case keyUp and keyIs(P) and ctrl                            of TRUE: result := koNone; end;
//    case keyUp and keyIs(_5)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(_6)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(_7)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(V)                                     of TRUE: result := koNone; end;
  end;

begin
  result := getKeyOp;

  {$if BazDebugWindow}
//  case keyIs(VK_ESCAPE) of TRUE:  begin
//                                    TDebug.debugEnum<TKeyOp>('getKeyOp', result);
//                                    debugBoolean('keyboardThumbs: evGSIgnoreEscape', GS.ignoreEscape); end;end;
  {$endif}

  mmp.cmd(evGSIgnoreEscape, FALSE);
end;

end.
