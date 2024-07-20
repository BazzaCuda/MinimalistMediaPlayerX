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
unit viewModel.mmpKeyboardOps;

interface

uses
  winApi.messages, winApi.windows,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts,
  mmpKeyboardUtils, mmpShellUtils,
  model.mmpMediaPlayer;

function mmpProcessKeyOp(const MP: IMediaPlayer; var SS: TSnapshot): boolean;

implementation

uses
  mmpPostToAllUtils, mmpWindowUtils,
  model.mmpBookmark, model.mmpMixer, model.mmpPlaylistUtils,
  _debugWindow;

function sendOpInfo(const aOpInfo: string): boolean;
begin
  notifyApp(newNotice(evSTOpInfo, aOpInfo));
end;

function message(const aMsg: integer; const aWParam: NativeUInt; aLParam: NativeInt): TMessage;
begin
  result.msg := aMsg;
  result.WParam := aWParam;
  result.LParam := aLParam;
end;

function mmpProcessKeyOp(const MP: IMediaPlayer; var SS: TSnapshot): boolean;
begin
  result      := FALSE;
  SS.handled  := FALSE;

  case SS.keyOp of
    koNone:               EXIT; // key not processed. bypass setting result to TRUE

    koAboutBox:           notifyApp(newNotice(evAboutFormShow));
    koAdjustAspectRatio:  notifyApp(newNotice(evVMAdjustAspectRatio));
    koArrangeAll:         notifyApp(newNotice(evVMArrangeAll));
    koBookmarkDelete:     newBookmark.delete(MP.notify(newNotice(evMPReqFileName)).text);
    koBookmarkLoad:       MP.notify(newNotice(evPBClick, newBookmark.position(MP.notify(newNotice(evMPReqFileName)).text)));
    koBookmarkSave:       newBookmark.save(MP.notify(newNotice(evMPReqFileName)).text, MP.notify(newNotice(evMPReqPosition)).integer);
    koBrighterPB:         begin notifyApp(newNotice(evMCBrighter)); notifyApp(newNotice(evPBBrighter)); notifyApp(newNotice(evSTBrighter)); end;
    koBrightnessDn:       MP.notify(newNotice(evMPBrightnessDn));
    koBrightnessReset:    MP.notify(newNotice(evMPBrightnessReset));
    koBrightnessUp:       MP.notify(newNotice(evMPBrightnessUp));
    koCentreWindow:       notifyApp(newNotice(evVMCenterWindow));
    koClipboard:          sendOpInfo(notifyApp(newNotice(evPLCopyToClipboard)).text);
    koCloseEvery:         notifyApp(newNotice(evPAPostToEvery, WIN_CLOSEAPP));
    koCloseApp:           notifyApp(newNotice(evAppClose));
    koContrastDn:         MP.notify(newNotice(evMPContrastDn));
    koContrastReset:      MP.notify(newNotice(evMPContrastReset));
    koContrastUp:         MP.notify(newNotice(evMPContrastUp));
    koCycleAudio:         MP.notify(newNotice(evMPCycleAudio));
    koCycleSubs:          MP.notify(newNotice(evMPCycleSubs));
    koDarkerPB:           begin notifyApp(newNotice(evMCDarker)); notifyApp(newNotice(evPBDarker)); notifyApp(newNotice(evSTDarker)); end;
    koDeleteCurrentItem:  notifyApp(newNotice(evVMDeleteCurrentItem, SS.shiftState));
    koEscape:             notifyApp(newNotice(evVMDoEscapeKey));
    koExploreFolder:      mmpShellExec(notifyApp(newNotice(evPLReqCurrentFolder)).text);
    koFrameBackwards:     MP.notify(newNotice(evMPFrameBackwards));
    koFrameForwards:      MP.notify(newNotice(evMPFrameForwards));
    koFullscreen:         notifyApp(newNotice(evVMToggleFullscreen));
    koGammaDn:            MP.notify(newNotice(evMPGammaDn));
    koGammaReset:         MP.notify(newNotice(evMPGammaReset));
    koGammaUp:            MP.notify(newNotice(evMPGammaUp));
    koGreaterWindow:      notifyApp(newNotice(evPAPostToAll, WIN_GREATER));
    koKeep:               notifyApp(newNotice(evVMKeepCurrentItem));
    koKeepDelete:         notifyApp(newNotice(evVMKeepDelete));
    koImageInBrowser:     notifyApp(newNotice(evVMImageInBrowser));
    koMaximize:           begin notifyApp(newNotice(evGSAutoCenter, TRUE)); notifyApp(newNotice(evGSMaxSize, TRUE)); notifyApp(newNotice(evVMResizeWindow)); end; // maximize the video according to the height of the screen
    koMinimizeWindow:     notifyApp(newNotice(evVMMinimize));
    koMuteUnmute:         notifyApp(newNotice(evMPMuteUnmute));
    koNextChapter:        MP.notify(newNotice(evMPNextChapter));
    koPanDn:              MP.notify(newNotice(evMPPanDn));
    koPanLeft:            MP.notify(newNotice(evMPPanLeft));
    koPanReset:           MP.notify(newNotice(evMPPanReset));
    koPanRight:           MP.notify(newNotice(evMPPanRight));
    koPanUp:              MP.notify(newNotice(evMPPanUP));
    koPausePlay:          notifyApp(newNotice(evPAPostToAll, WIN_PAUSE_PLAY));
    koPBReset:            begin notifyApp(newNotice(evMCReset)); notifyApp(newNotice(evPBReset)); notifyApp(newNotice(evSTReset)); end;
    koPrevChapter:        MP.notify(newNotice(evMPPrevChapter));
    koPlayFirst:          notifyApp(newNotice(evVMMPPlayFirst));
    koPlayLast:           notifyApp(newNotice(evVMMPPlayLast));
    koPlayNext:           notifyApp(newNotice(evVMMPPlayNext));
    koPlayNextFolder:     notifyApp(newNotice(evVMPlayNextFolder));
    koPlayPrev:           notifyApp(newNotice(evVMMPPlayPrev));
    koPlayPrevFolder:     notifyApp(newNotice(evVMPlayPrevFolder));
    koReloadPlaylist:     notifyApp(newNotice(evVMReloadPlaylist));
    koRenameFile:         notifyApp(newNotice(evVMRenameCurrentItem));
    koResetAll:           MP.notify(newNotice(evMPResetAll));
    koRotateL:            MP.notify(newNotice(evMPRotateLeft));
    koRotateR:            MP.notify(newNotice(evMPRotateRight));
    koRotateReset:        MP.notify(newNotice(evMPRotateReset));
    koRunCut:             mmpOpenExternalApp(F11_APP, notifyApp(newNotice(evPLReqCurrentItem)).text);
    koRunPot:             mmpOpenExternalApp(F10_APP, notifyApp(newNotice(evPLReqCurrentItem)).text);
    koRunShot:            mmpOpenExternalApp(F12_APP, notifyApp(newNotice(evPLReqCurrentItem)).text);
    koShowCaption:        notifyApp(newNotice(evPAPostToAll, WIN_CAPTION));
    koSpeedDn:            MP.notify(newNotice(evMPSpeedDn));
    koSpeedReset:         MP.notify(newNotice(evMPSpeedReset));
    koSpeedUp:            MP.notify(newNotice(evMPSpeedUp));
    koStartOver:          notifyApp(newNotice(evPAPostToAll, WIN_START_OVER));
    koSysVolMax:          notifyApp(newNotice(evMXSysVolMax)); // someone or something needs to know this but we don't know (or care) who
    koTab:                notifyApp(newNotice(evPAPostToAll, WIN_TAB));
    koTabTab:             notifyApp(newNotice(evPAPostToAll, WIN_TABTAB));
    koSaturationDn:       MP.notify(newNotice(evMPSaturationDn));
    koSaturationReset:    MP.notify(newNotice(evMPSaturationReset));
    koSaturationUp:       MP.notify(newNotice(evMPSaturationUp));
    koScreenshot:         MP.notify(newNotice(evMPScreenshot, mmpScreenshotFolder));
    koSyncMedia:          notifyApp(newNotice(evPAPostToEveryEx, message(WIN_SYNC_MEDIA, MP.notify(newNotice(evMPReqPosition)).integer, 0)));
    koThumbnails:         notifyApp(newNotice(evVMShowThumbs));
    koToggleProgressBar:  notifyApp(newNotice(evPBToggleProgressBar));
    koToggleControls:     notifyApp(newNotice(evPAPostToAll, WIN_TOGGLE_CONTROLS));
    koToggleEditMode:     notifyApp(newNotice(evVMToggleEditMode));
    koToggleFiltering:    notifyApp(newNotice(evVMToggleFiltering));
    koToggleHelp:         notifyApp(newNotice(evVMToggleHelp));
    koToggleNumlock:      mmpToggleNumlock;
    koTogglePlaylist:     notifyApp(newNotice(evVMTogglePlaylist));
    koToggleRepeat:       MP.notify(newNotice(evMPToggleRepeat));
    koToggleSubtitles:    MP.notify(newNotice(evMPToggleSubtitles));
    koVolDn:              MP.notify(newNotice(evMPVolDn));
    koVolUp:              MP.notify(newNotice(evMPVolUp));
    koWiki:               mmpShellExec('https://minimalistmediaplayer.com');
    koZoomIn:             MP.notify(newNotice(evMPZoomIn));
    koZoomOut:            MP.notify(newNotice(evMPZoomOut));
    koZoomReset:          MP.notify(newNotice(evMPZoomReset));
  end;

  SS.handled := TRUE;
  result     := TRUE;
end;


end.
