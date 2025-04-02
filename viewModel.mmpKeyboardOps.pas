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
  mmpFuncProcs, mmpPostToAllUtils, mmpWindowUtils,
  model.mmpBookmark, model.mmpMixer, model.mmpPlaylistUtils,
  _debugWindow;

function sendOpInfo(const aOpInfo: string): boolean;
begin
  mmpDo(evSTOpInfo, aOpInfo);
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

    koAboutBox:           mmpDo(evAboutFormShow);
    koAdjustAspectRatio:  mmpDo(evVMAdjustAspectRatio);
    koArrangeAll:         mmpDo(evVMArrangeAll);
    koBookmarkDelete:     newBookmark.delete(MP.notify(mmpDo(evMPReqFileName)).text);
    koBookmarkLoad:       MP.notify(mmpDo(evPBClick, newBookmark.position(MP.notify(mmpDo(evMPReqFileName)).text)));
    koBookmarkSave:       newBookmark.save(MP.notify(mmpDo(evMPReqFileName)).text, MP.notify(mmpDo(evMPReqPosition)).integer);
    koBrighterPB:         begin mmpDo(evMCBrighter); mmpDo(evPBBrighter); mmpDo(evSTBrighter); end;
    koBrightnessDn:       MP.notify(mmpDo(evMPBrightnessDn));
    koBrightnessReset:    MP.notify(mmpDo(evMPBrightnessReset));
    koBrightnessUp:       MP.notify(mmpDo(evMPBrightnessUp));
    koCleanup:            mmpDo(evVMCleanup);
    koCentreWindow:       mmpDo(evVMCenterWindow);
    koClipboard:          sendOpInfo(mmpDo(evPLCopyToClipboard).text);
    koCloseEvery:         mmpDo(evPAPostToEvery, WIN_CLOSEAPP);
    koCloseApp:           mmpDo(evAppClose);
    koContrastDn:         MP.notify(mmpDo(evMPContrastDn));
    koContrastReset:      MP.notify(mmpDo(evMPContrastReset));
    koContrastUp:         MP.notify(mmpDo(evMPContrastUp));
    koCycleAudio:         MP.notify(mmpDo(evMPCycleAudio));
    koCycleSubs:          MP.notify(mmpDo(evMPCycleSubs));
    koDarkerPB:           begin mmpDo(evMCDarker); mmpDo(evPBDarker); mmpDo(evSTDarker); end;
    koDeleteCurrentItem:  mmpDo(evVMDeleteCurrentItem, SS.shiftState);
    koEscape:             mmpDo(evVMDoEscapeKey);
    koExploreFolder:      mmpShellExec(mmpDo(evPLReqCurrentFolder).text);
    koFrameBackwards:     MP.notify(mmpDo(evMPFrameBackwards));
    koFrameForwards:      MP.notify(mmpDo(evMPFrameForwards));
    koFullscreen:         notifyApp(mmpDo(evVMToggleFullscreen));
    koGammaDn:            MP.notify(mmpDo(evMPGammaDn));
    koGammaReset:         MP.notify(mmpDo(evMPGammaReset));
    koGammaUp:            MP.notify(mmpDo(evMPGammaUp));
    koGreaterWindow:      mmpDo(evPAPostToAll, WIN_GREATER);
    koKeep:               mmpDo(evVMKeepCurrentItem);
    koKeepCatF1:          mmpDo(evVMKeepCatF1);
    koKeepCatF2:          mmpDo(evVMKeepCatF2);
    koKeepCatF3:          mmpDo(evVMKeepCatF3);
    koKeepCatF4:          mmpDo(evVMKeepCatF4);
    koKeepDelete:         mmpDo(evVMKeepDelete);
    koKeepMove:           mmpDo(evVMKeepMove);
    koImageInBrowser:     mmpDo(evVMImageInBrowser);
    koMaximize:           begin mmpDo(evGSAutoCenter, TRUE); mmpDo(evGSMaxSize, TRUE); mmpDo(evVMResizeWindow); end; // maximize the video according to the height of the screen
    koMinimizeWindow:     mmpDo(evVMMinimize);
    koMuteUnmute:         mmpDo(evMPMuteUnmute);
    koNextChapter:        MP.notify(mmpDo(evMPNextChapter));
    koPanDn:              MP.notify(mmpDo(evMPPanDn));
    koPanLeft:            MP.notify(mmpDo(evMPPanLeft));
    koPanReset:           MP.notify(mmpDo(evMPPanReset));
    koPanRight:           MP.notify(mmpDo(evMPPanRight));
    koPanUp:              MP.notify(mmpDo(evMPPanUP));
    koPausePlay:          mmpDo(evPAPostToAll, WIN_PAUSE_PLAY);
    koPBReset:            begin mmpDo(evMCReset); mmpDo(evPBReset); mmpDo(evSTReset); end;
    koPrevChapter:        MP.notify(mmpDo(evMPPrevChapter));
    koPlayFirst:          mmpDo(evVMMPPlayFirst);
    koPlayLast:           mmpDo(evVMMPPlayLast);
    koPlayNext:           mmpDo(evVMMPPlayNext);
    koPlayNextFolder:     mmpDo(evVMPlayNextFolder);
    koPlayPrev:           mmpDo(evVMMPPlayPrev);
    koPlayPrevFolder:     mmpDo(evVMPlayPrevFolder);
    koReloadPlaylist:     mmpDo(evVMReloadPlaylist);
    koRenameFile:         mmpDo(evVMRenameCurrentItem);
    koResetAll:           MP.notify(mmpDo(evMPResetAll));
    koRotateL:            MP.notify(mmpDo(evMPRotateLeft));
    koRotateR:            MP.notify(mmpDo(evMPRotateRight));
    koRotateReset:        MP.notify(mmpDo(evMPRotateReset));
    koRunCut:             mmpOpenExternalApp(F11_APP, mmpDo(evPLReqCurrentItem).text);
    koRunPot:             mmpOpenExternalApp(F10_APP, mmpDo(evPLReqCurrentItem).text);
    koRunShot:            mmpOpenExternalApp(F12_APP, mmpDo(evPLReqCurrentItem).text);
    koShowCaption:        mmpDo(evPAPostToAll, WIN_CAPTION);
    koSpeedDn:            MP.notify(mmpDo(evMPSpeedDn));
    koSpeedReset:         MP.notify(mmpDo(evMPSpeedReset));
    koSpeedUp:            MP.notify(mmpDo(evMPSpeedUp));
    koStartOver:          mmpDo(evPAPostToAll, WIN_START_OVER);
    koSysVolMax:          mmpDo(evMXSysVolMax); // someone or something needs to know this but we don't know (or care) who or what
    koTab:                mmpDo(evPAPostToAll, WIN_TAB);
    koTabTab:             mmpDo(evPAPostToAll, WIN_TABTAB);
    koSaturationDn:       MP.notify(mmpDo(evMPSaturationDn));
    koSaturationReset:    MP.notify(mmpDo(evMPSaturationReset));
    koSaturationUp:       MP.notify(mmpDo(evMPSaturationUp));
    koScreenshot:         MP.notify(mmpDo(evMPScreenshot, mmpScreenshotFolder));
    koSyncMedia:          mmpDo(evPAPostToEveryEx, message(WIN_SYNC_MEDIA, MP.notify(mmpDo(evMPReqPosition)).integer, 0));
    koThumbnails:         mmpDo(evVMShowThumbs);
    koToggleProgressBar:  mmpDo(evPBToggleProgressBar);
    koToggleControls:     mmpDo(evPAPostToAll, WIN_TOGGLE_CONTROLS);
    koToggleEditMode:     mmpDo(evVMToggleEditMode);
    koToggleFiltering:    mmpDo(evVMToggleFiltering);
    koToggleHelp:         mmpDo(evVMToggleHelp);
    koToggleNumlock:      mmpToggleNumlock;
    koTogglePlaylist:     mmpDo(evVMTogglePlaylist);
    koToggleRepeat:       MP.notify(mmpDo(evMPToggleRepeat));
    koToggleSubtitles:    MP.notify(mmpDo(evMPToggleSubtitles));
    koVolDn:              MP.notify(mmpDo(evMPVolDn));
    koVolUp:              MP.notify(mmpDo(evMPVolUp));
    koWiki:               mmpShellExec('https://minimalistmediaplayer.com');
    koZoomIn:             MP.notify(mmpDo(evMPZoomIn));
    koZoomOut:            MP.notify(mmpDo(evMPZoomOut));
    koZoomReset:          MP.notify(mmpDo(evMPZoomReset));
  end;

  SS.handled := TRUE;
  result     := TRUE;
end;


end.
