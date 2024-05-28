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
unit mmpThumbsKeyboard;

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
            koTogglePlaylist, koCloseAll, koScreenshot, koAboutBox, koMaximize, koPlayThumbs, koNextFolder, koPrevFolder, koSaveCopy, koMoveToKeyFolder,
            koThumbsUp, koThumbsDn
            );
  TKeyDirection = (kdDn, kdUp);

function processKeyStroke(const mpv: TMPVBasePlayer; const aKey: word; const aShiftState: TShiftState; const upDn: TKeyDirection): TKeyOp;

implementation

uses
  winApi.windows,
  TGlobalVarsClass,
  _debugWindow;

const
  A = 'A'; B = 'B'; C = 'C'; D = 'D'; E = 'E'; F = 'F'; G = 'G'; H = 'H'; I = 'I'; J = 'J'; K = 'K'; L = 'L'; M = 'M';
  N = 'N'; O = 'O'; P = 'P'; Q = 'Q'; R = 'R'; S = 'S'; T = 'T'; U = 'U'; V = 'V'; W = 'W'; X = 'X'; Y = 'Y'; Z = 'Z';
  _0 = '0'; _1 = '1'; _2 = '2'; _3 = '3'; _4 = '4'; _5 = '5'; _6 = '6'; _7 = '7'; _8 = '8'; _9 = '9';
  _EQUALS = 187; SLASH = 191; BACKSLASH = 220; OPEN_BRACKET = 219; CLOSE_BRACKET = 221; HYPHEN = 189; HASH = 222; BACKSPACE = 8;
  OPEN_BRACE = 219; CLOSE_BRACE = 221; SINGLE_QUOTE = 192; SEMICOLON = 186;

function processKeyStroke(const mpv: TMPVBasePlayer; const aKey: word; const aShiftState: TShiftState; const upDn: TKeyDirection): TKeyOp;

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
  begin
    result := koNone;
    case keyUp and keyIs(X)                                               of TRUE: result := koCloseImageBrowser; end;
    case keyUp and keyIs(VK_ESCAPE)                                       of TRUE: result := koCloseImageBrowser; end;
    case keyUp and keyIs(_0) and ctrl                                     of TRUE: result := koCloseAll; end;

    case keyDn and keyIs(_9) and NOT ctrl                                 of TRUE: result := koBrightnessUp; end;
    case keyDn and keyIs(_8)                                              of TRUE: result := koBrightnessDn; end;
    case keyUp and keyIs(_2)                                              of TRUE: result := koBrightnessReset; end;
    case keyDn and keyIs(_EQUALS)                                         of TRUE: result := koContrastUp; end;
    case keyDn and keyIs(HYPHEN)                                          of TRUE: result := koContrastDn; end;
    case keyUp and keyIs(_0) and NOT ctrl                                 of TRUE: result := koContrastReset; end;
    case keyDn and keyIs(CLOSE_BRACKET)                                   of TRUE: result := koGammaUp; end;
    case keyDn and keyIs(OPEN_BRACKET)                                    of TRUE: result := koGammaDn; end;
    case keyUp and keyIs(SINGLE_QUOTE)                                    of TRUE: result := koGammaReset; end;
    case keyDn and keyIs(VK_UP) and ctrl                                  of TRUE: result := koPanUp; end;
    case keyDn and keyIs(VK_DOWN) and ctrl                                of TRUE: result := koPanDn; end;
    case keyDn and keyIs(VK_LEFT) and ctrl                                of TRUE: result := koPanLeft; end;
    case keyDn and keyIs(VK_RIGHT) and ctrl                               of TRUE: result := koPanRight; end;
    case keyDn and keyIs(VK_UP)   and NOT shift and NOT ctrl              of TRUE: result := koPrevFolder; end;
    case keyDn and keyIs(VK_DOWN) and NOT shift and NOT ctrl              of TRUE: result := koNextFolder; end;
    case keyDn and keyIs(VK_LEFT) and NOT ctrl                            of TRUE: result := koPlayPrev; end;
    case keyDn and keyIs(VK_RIGHT) and NOT ctrl                           of TRUE: result := koPlayNext; end;
    case keyUp and keyIs(_3)                                              of TRUE: result := koPanReset; end;
    case keyUp and keyIs(VK_NEXT) and NOT GV.showingPlaylist              of TRUE: result := koRotateR; end;
    case keyUp and keyIs(VK_PRIOR) and NOT GV.showingPlaylist             of TRUE: result := koRotateL; end;
    case keyUp and keyIs(_4)                                              of TRUE: result := koRotateReset; end;
    case keyDn and keyIs(CLOSE_BRACKET) and shift                         of TRUE: result := koSaturationUp; end; // close curly brace
    case keyDn and keyIs(OPEN_BRACKET) and shift                          of TRUE: result := koSaturationDn; end; // open curly brace
    case keyUp and keyIs(SEMICOLON)                                       of TRUE: result := koSaturationReset; end;
    case keyUp and keyIs(S) and ctrl                                      of TRUE: result := koScreenshot; end; // in Image Browser, all F-keys are reserved
    case keyUp and keyIs(S) and NOT ctrl                                  of TRUE: result := koSaveMove; end;
    case keyDn and keyIs(I)                                               of TRUE: result := koZoomIn; end;
    case keyDn and keyIs(O)                                               of TRUE: result := koZoomOut; end;
    case keyUp and keyIs(U)                                               of TRUE: result := koZoomReset; end;

    case keyUp and keyIs(BACKSPACE)                                       of TRUE: result := koAllReset; end;

//    case keyUp and keyIs(VK_SPACE) and GV.playingSlideshow                of TRUE: result := koPausePlay; end;
    case keyDn and keyIs(VK_SPACE) and NOT GV.playingSlideshow            of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_SPACE) and ctrl and NOT GV.playingSlideshow   of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(W)                                               of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_RETURN) and NOT GV.showingPlaylist            of TRUE: result := koPlayNext; end;
    case keyDn and keyIs(VK_RETURN) and ctrl and NOT GV.showingPlaylist   of TRUE: result := koPlayPrev; end;
    case keyUp and keyIs(Q)                                               of TRUE: result := koPlayPrev; end;
    case keyDn and keyIs(G)                                               of TRUE: result := koGreaterWindow; end;
    case keyUp and keyIs(A) and NOT ctrl                                  of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(A) and ctrl                                      of TRUE: result := koAboutBox; end;
    case keyUp and keyIs(VK_HOME)                                         of TRUE: result := koPlayFirst; end;
    case keyUp and keyIs(Z)                                               of TRUE: result := koPlayLast; end;
    case keyUp and keyIs(VK_END)                                          of TRUE: result := koPlayLast; end;
    case keyUp and keyIs(H)                                               of TRUE: result := koCentreWindow; end;
    case keyUp and keyIs(D)                                               of TRUE: result := koNextFolder; end;
    case keyUp and keyIs(E)                                               of TRUE: result := koPrevFolder; end;
    case keyUp and keyIs(VK_DELETE)                                       of TRUE: result := koDeleteCurrentItem; end;
    case keyUp and keyIs(R)                                               of TRUE: result := koRenameFile; end;
    case keyUp and keyIs(K)                                               of TRUE: result := koKeep; end;
    case keyUp and keyIs(Y)                                               of TRUE: result := koPlayThumbs; end;
    case keyDn and keyIs(T)                                               of TRUE: result := koPlayThumbs; end;
    case keyUp and keyIs(C)                                               of TRUE: result := koSaveCopy; end;
    case keyUp and (aKey in [VK_F1..VK_F12])                              of TRUE: result := koMoveToKeyFolder; end;
    case keyUp and keyIs(H) and ctrl                                      of TRUE: result := koToggleHelp; end;
    case keyUp and keyIs(VK_INSERT)                                       of TRUE: result := koClipboard; end;
    case keyUp and keyIs(L)                                               of TRUE: result := koReloadPlaylist; end;
    case keyUp and keyIs(VK_UP)   and shift and NOT ctrl                  of TRUE: result := koThumbsUp; end;
    case keyUp and keyIs(VK_DOWN) and shift and NOT ctrl                  of TRUE: result := koThumbsDn; end;

// TO DO


//    case keyUp and keyIs(F)                                               of TRUE: result := koFullscreen; end;
//    case keyUp and keyIs(N)                                               of TRUE: result := koMinimizeWindow; end;
//    case keyUp and keyIs(B) and NOT ctrl                                  of TRUE: result := koToggleBlackout; end;
//    case keyUp and keyIs(HASH)                                            of TRUE: result := koShowCaption; end;
//    case keyDn and keyIs(VK_ADD)                                          of TRUE: result := koSpeedUp; end;
//    case keyDn and keyIs(SLASH)                                           of TRUE: result := koSpeedUp; end;
//    case keyDn and keyIs(VK_SUBTRACT)                                     of TRUE: result := koSpeedDn; end;
//    case keyDn and keyIs(BACKSLASH)                                       of TRUE: result := koSpeedDn; end;
//    case keyUp and keyIs(_1)                                              of TRUE: result := koSpeedReset; end;
//    case keyDn and keyIs(B) and ctrl                                      of TRUE: result := koBrighterPB; end;
//    case keyDn and keyIs(B) and ctrl and shift                            of TRUE: result := koDarkerPB; end;
//    case keyUp and keyIs(P)                                               of TRUE: result := koTogglePlaylist; end;
//    case keyUp and keyIs(M)                                               of TRUE: result := koMaximize; end;

// spare keys
//    case keyUp and keyIs(R) and ctrl                            of TRUE: result := koNone; end;
//    case keyDn and keyIs(VK_TAB)                                of TRUE: result := koNone; end;
//    case keyUp and keyIs(J)                                     of TRUE: result := koNone; end;
//    case keyUp and keyIs(P) and ctrl                            of TRUE: result := koNone; end;
//    case keyUp and keyIs(_5)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(_6)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(_7)                                    of TRUE: result := koNone; end;
//    case keyUp and keyIs(_9) and ctrl                           of TRUE: result := koNone; end;
//    case keyUp and keyIs(V)                                     of TRUE: result := koNone; end;
//    case keyUp and keyIs(E) and ctrl                            of TRUE: result := koNone; end;
  end;

begin
//  debugInteger('keyOpKey', aKey);
//  case upDn = kdUp of TRUE: debug('key UP'); end;
//  case upDn = kdDn of TRUE: debug('key DN'); end;
//  debugBoolean('shift in shift', shift);
//  debugBoolean('ctrl in shift', ctrl);

  result := getKeyOp;

//  debugInteger('keyOp', integer(result));
end;

end.
