{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit keyboard;

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
            koGammaReset, koSaturationReset, koAllReset);
  TKeyDirection = (kdDown, kdUp);

  TKeyboard = class(TObject)
  strict private
    FKey:         word;
    FShiftState:  TShiftState;
    FUpDn:        TKeyDirection;
  private
    function keyIs(aChar: char): boolean; overload;
    function keyIs(aKeyCode: word): boolean; overload;
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
  sysCommands, winApi.windows, mediaPlayer, mediaInfo, formCaption, playlist, UICtrls, consts, globalVars, commonUtils, vcl.forms,
  system.sysUtils, bookmark, _debugWindow;

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
  case keyUp and keyIs(X)                 of TRUE: result := koCloseApp; end;
  case keyUp and keyIs(VK_ESCAPE)         of TRUE: result := koCloseApp; end;
  case keyDn and keyIs(VK_DOWN)           of TRUE: result := koVolDn; end;
  case keyDn and keyIs(VK_VOLUME_DOWN)    of TRUE: result := koVolDn; end;
  case keyDn and keyIs(VK_UP)             of TRUE: result := koVolUp; end;
  case keyDn and keyIs(VK_VOLUME_UP)      of TRUE: result := koVolUp; end;
  case keyDn and keyIs(T)                 of TRUE: result := koTab; end;
  case keyUp and keyIs(VK_SPACE)          of TRUE: result := koPausePlay; end;
  case keyDn and keyIs(VK_RIGHT)          of TRUE: result := koFrameForwards; end;
  case keyDn and keyIs(VK_LEFT)           of TRUE: result := koFrameBackwards; end;
  case keyDn and keyIs(VK_TAB)            of TRUE: result := koTabTab; end;
  case keyUp and keyIs(J)                 of TRUE: result := koAdjustAspectRatio; end;
  case keyDn and keyIs(_9)                of TRUE: result := koBrightnessUp; end;
  case keyDn and keyIs(_8)                of TRUE: result := koBrightnessDn; end;
  case keyDn and keyIs(I)                 of TRUE: result := koZoomIn; end;
  case keyDn and keyIs(O)                 of TRUE: result := koZoomOut; end;
  case keyUp and keyIs(S)                 of TRUE: result := koStartOver; end;
  case keyUp and keyIs(HASH)                of TRUE: result := koShowCaption; end;
  case keyUp and keyIs(E)                 of TRUE: result := koMuteUnmute; end;
  case keyUp and keyIs(VK_VOLUME_MUTE)    of TRUE: result := koMuteUnmute; end;
  case keyUp and keyIs(W)                 of TRUE: result := koPlayNext; end;
  case keyUp and keyIs(VK_RETURN)         of TRUE: result := koPlayNext; end;
  case keyUp and keyIs(Q)                 of TRUE: result := koPlayPrev; end;
  case keyDn and ctrl and keyIs(VK_LEFT)  of TRUE: result := koPanLeft; end;
  case keyDn and ctrl and keyIs(VK_RIGHT) of TRUE: result := koPanRight; end;
  case keyDn and ctrl and keyIs(VK_UP)    of TRUE: result := koPanUp; end;
  case keyDn and ctrl and keyIs(VK_DOWN)  of TRUE: result := koPanDn; end;
  case keyUp and keyIs(VK_NEXT)           of TRUE: result := koRotateR; end;
  case keyUp and keyIs(VK_PRIOR)          of TRUE: result := koRotateL; end;
  case keyUp and keyIs(F)                 of TRUE: result := koFullscreen; end;
  case keyUp and keyIs(U)                 of TRUE: result := koZoomReset; end;
  case keyDn and keyIs(G)                 of TRUE: result := koGreaterWindow; end;
  case keyUp and keyIs(A)                 of TRUE: result := koPlayFirst; end;
  case keyUp and keyIs(VK_HOME)           of TRUE: result := koPlayFirst; end;
  case keyUp and keyIs(Z)                 of TRUE: result := koPlayLast; end;
  case keyUp and keyIs(VK_END)            of TRUE: result := koPlayLast; end;
  case keyUp and keyIs(C)                 of TRUE: result := koToggleControls; end;
  case keyUp and keyIs(VK_F10)            of TRUE: result := koRunPot; end;
  case keyUp and keyIs(VK_F11)            of TRUE: result := koRunCut; end;
  case keyUp and keyIs(VK_F12)            of TRUE: result := koRunShot; end;
  case keyUp and keyIs(B)                 of TRUE: result := koToggleBlackout; end;
  case keyUp and keyIs(H)                 of TRUE: result := koCentreWindow; end;
  case keyUp and keyIs(N)                 of TRUE: result := koMinimizeWindow; end;
  case keyUp and keyIs(D)                 of TRUE: result := koDeleteCurrentItem; end;
  case keyUp and keyIs(VK_DELETE)         of TRUE: result := koDeleteCurrentItem; end;
  case keyUp and keyIs(R)                 of TRUE: result := koRenameFile; end;
  case keyDn and keyIs(VK_ADD)            of TRUE: result := koSpeedUp; end;
  case keyDn and keyIs(SLASH)             of TRUE: result := koSpeedUp; end;
  case keyDn and keyIs(VK_SUBTRACT)       of TRUE: result := koSpeedDn; end;
  case keyDn and keyIs(BACKSLASH)         of TRUE: result := koSpeedDn; end;
  case keyUp and keyIs(_1)                of TRUE: result := koSpeedReset; end;
  case keyUp and keyIs(VK_ESCAPE)         of TRUE: result := koEscape; end;
  case keyUp and keyIs(VK_INSERT)         of TRUE: result := koClipboard; end;
  case keyUp and keyIs(K)                 of TRUE: result := koKeep; end;
  case keyUp and keyIs(L)                 of TRUE: result := koReloadPlaylist; end;
  case keyUp and ctrl and keyIs(P)        of TRUE: result := koAlwaysPot; end;
  case keyUp and keyIs(_2)                of TRUE: result := koPanReset; end;
  case keyUp and keyIs(_3)                of TRUE: result := koBrightnessReset; end;
  case keyUp and keyIs(_4)                of TRUE: result := koRotateReset; end;
  case keyUp and keyIs(_5)                of TRUE: result := koBookmarkSave; end;
  case keyUp and keyIs(_6)                of TRUE: result := koBookmarkLoad; end;
  case keyUp and keyIs(_7)                of TRUE: result := koBookmarkDelete; end;
  case keyDn and keyIs(_EQUALS)           of TRUE: result := koContrastUp; end;
  case keyDn and keyIs(HYPHEN)            of TRUE: result := koContrastDn; end;
  case keyUp and keyIs(_0)                of TRUE: result := koContrastReset; end;
  case keyDn and keyIs(OPEN_BRACKET)      of TRUE: result := koGammaDn; end;
  case keyDn and keyIs(CLOSE_BRACKET)     of TRUE: result := koGammaUp; end;
  case keyUp and keyIs(SINGLE_QUOTE)      of TRUE: result := koGammaReset; end;
  case keyDn and shift and keyIs(OPEN_BRACKET)  of TRUE: result := koSaturationDn; end; // open curly brace
  case keyDn and shift and keyIs(CLOSE_BRACKET) of TRUE: result := koSaturationUp; end; // close curly brace
  case keyDn and keyIs(SEMICOLON)         of TRUE: result := koSaturationReset; end;
  case keyDn and keyIs(BACKSPACE)         of TRUE: result := koAllReset; end;

//  debugInteger('keyOp', integer(result));
end;

function TKeyboard.getAlt: boolean;
begin
  result := ssAlt in shiftState;
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

function TKeyboard.keyIs(aChar: char): boolean;
begin
  result := key = ord(aChar);
end;

function TKeyboard.keyIs(aKeyCode: word): boolean;
begin
  result := key = aKeyCode;
end;

procedure TKeyboard.formKeyUp(sender: TObject; var key: WORD; shift: TShiftState);
// keys that don't generate a standard WM_KEYUP message
begin
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

//  debugInteger('aKey', aKey);

  case getKeyOp of
    koNone:       EXIT; // key not processed. bypass setting result to TRUE

    koCloseApp:          sendSysCommandClose(UI.handle);
    koVolUp:             MP.volUp;
    koVolDn:             MP.volDown;
    koTab:               MP.tab(aShiftState, KB.capsLock);
    koTabTab:            MP.tab(aShiftState, KB.capsLock, -1);
    koPausePlay:         MP.pausePlay;
    koFrameForwards:     MP.frameForwards;
    koFrameBackwards:    MP.frameBackwards;
    koAdjustAspectRatio: UI.adjustAspectRatio(UI.handle, MP.videoWidth, MP.videoHeight);
    koBrightnessUp:      MP.brightnessUp;
    koBrightnessDn:      MP.brightnessDn;
    koZoomIn:            MP.zoomIn;
    koZoomOut:           MP.zoomOut;
    koStartOver:         MP.startOver;
    koShowCaption:       MC.caption := PL.formattedItem;
    koMuteUnmute:        MP.muteUnmute;
    koPlayNext:          MP.playNext;
    koPlayPrev:          MP.playPrev;
    koPanLeft:           MP.panLeft;
    koPanRight:          MP.panRight;
    koPanUp:             MP.panUp;
    koPanDn:             MP.panDn;
    koRotateR:           MP.rotateRight;
    koRotateL:           MP.rotateLeft;
    koFullscreen:        MP.toggleFullscreen;
    koZoomReset:         MP.zoomReset;
    koGreaterWindow:     UI.greaterWindow(UI.handle, aShiftState);
    koPlayFirst:         MP.playFirst;
    koPlayLast:          MP.playLast;
    koToggleControls:    UI.toggleControls(aShiftState);
    koRunPot:            UI.openExternalApp(POT_PLAYER, PL.currentItem);
    koRunCut:            UI.openExternalApp(LOSSLESS_CUT, PL.currentItem);
    koRunShot:           UI.openExternalApp(SHOTCUT, PL.currentItem);
    koToggleBlackout:    UI.toggleBlackout;
    koCentreWindow:      postMessage(GV.appWnd, WM_CENTRE_WINDOW, 0, 0);
    koMinimizeWindow:    UI.minimizeWindow;
    koDeleteCurrentItem: UI.deleteCurrentItem(aShiftState);
    koRenameFile:        UI.renameFile(PL.currentItem);
    koSpeedUp:           MP.speedUp;
    koSpeedDn:           MP.speedDn;
    koSpeedReset:        MP.speedReset;
    koEscape:            UI.doEscapeKey;
    koClipboard:         PL.copyToClipboard;
    koKeep:              UI.keepFile(PL.currentItem);
    koReloadPlaylist:    CU.reloadPlaylist(extractFilePath(PL.currentItem));
    koAlwaysPot:         GV.alwaysPot := NOT GV.alwaysPot;
    koPanReset:          MP.panReset;
    koBrightnessReset:   MP.brightnessReset;
    koBookmarkSave:      BM.save;
    koBookmarkLoad:      case BM.asInteger <> 0 of TRUE: MP.position := BM.asInteger; end;
    koBookmarkDelete:    BM.delete;
    koRotateReset:       MP.rotateReset;
    koAllReset:          MP.allReset;
    koContrastUp:        MP.contrastUp;
    koContrastDn:        MP.contrastDn;
    koContrastReset:     MP.contrastReset;
    koGammaUp:           MP.gammaUp;
    koGammaDn:           MP.gammaDn;
    koSaturationUp:      MP.saturationUp;
    koSaturationDn:      MP.saturationDn;
    koGammaReset:        MP.gammaReset;
    koSaturationReset:   MP.saturationReset;
  end;

  result := TRUE;
end;

initialization
  gKB := NIL;

finalization
  case gKB <> NIL of TRUE: gKB.free; end;

end.
