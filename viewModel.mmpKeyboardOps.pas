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
  mmpDoProcs, mmpPostToAllUtils, mmpWindowUtils,
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
    koBookmarkDelete:     newBookmark.delete(MP.notify(newNotice(evMPReqFileName)).text);
    koBookmarkLoad:       MP.notify(newNotice(evPBClick, newBookmark.position(MP.notify(newNotice(evMPReqFileName)).text)));
    koBookmarkSave:       newBookmark.save(MP.notify(newNotice(evMPReqFileName)).text, MP.notify(newNotice(evMPReqPosition)).integer);
    koBrighterPB:         begin mmpDo(evMCBrighter); mmpDo(evPBBrighter); mmpDo(evSTBrighter); end;
    koBrightnessDn:       MP.notify(newNotice(evMPBrightnessDn));
    koBrightnessReset:    MP.notify(newNotice(evMPBrightnessReset));
    koBrightnessUp:       MP.notify(newNotice(evMPBrightnessUp));
    koCleanup:            mmpDo(evVMCleanup);
    koCentreWindow:       mmpDo(evVMCenterWindow);
    koClipboard:          sendOpInfo(mmpDo(evPLCopyToClipboard).text);
    koCloseEvery:         mmpDo(evPAPostToEvery, WIN_CLOSEAPP);
    koCloseApp:           mmpDo(evAppClose);
    koContrastDn:         MP.notify(newNotice(evMPContrastDn));
    koContrastReset:      MP.notify(newNotice(evMPContrastReset));
    koContrastUp:         MP.notify(newNotice(evMPContrastUp));
    koCycleAudio:         MP.notify(newNotice(evMPCycleAudio));
    koCycleSubs:          MP.notify(newNotice(evMPCycleSubs));
    koDarkerPB:           begin mmpDo(evMCDarker); mmpDo(evPBDarker); mmpDo(evSTDarker); end;
    koDeleteCurrentItem:  mmpDo(evVMDeleteCurrentItem, SS.shiftState);
    koEscape:             mmpDo(evVMDoEscapeKey);
    koExploreFolder:      mmpShellExec(mmpDo(evPLReqCurrentFolder).text);
    koFrameBackwards:     MP.notify(newNotice(evMPFrameBackwards));
    koFrameForwards:      MP.notify(newNotice(evMPFrameForwards));
    koFullscreen:         notifyApp(mmpDo(evVMToggleFullscreen));
    koGammaDn:            MP.notify(newNotice(evMPGammaDn));
    koGammaReset:         MP.notify(newNotice(evMPGammaReset));
    koGammaUp:            MP.notify(newNotice(evMPGammaUp));
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
    koNextChapter:        MP.notify(newNotice(evMPNextChapter));
    koPanDn:              MP.notify(newNotice(evMPPanDn));
    koPanLeft:            MP.notify(newNotice(evMPPanLeft));
    koPanReset:           MP.notify(newNotice(evMPPanReset));
    koPanRight:           MP.notify(newNotice(evMPPanRight));
    koPanUp:              MP.notify(newNotice(evMPPanUP));
    koPausePlay:          mmpDo(evPAPostToAll, WIN_PAUSE_PLAY);
    koPBReset:            begin mmpDo(evMCReset); mmpDo(evPBReset); mmpDo(evSTReset); end;
    koPrevChapter:        MP.notify(newNotice(evMPPrevChapter));
    koPlayFirst:          mmpDo(evVMMPPlayFirst);
    koPlayLast:           mmpDo(evVMMPPlayLast);
    koPlayNext:           mmpDo(evVMMPPlayNext);
    koPlayNextFolder:     mmpDo(evVMPlayNextFolder);
    koPlayPrev:           mmpDo(evVMMPPlayPrev);
    koPlayPrevFolder:     mmpDo(evVMPlayPrevFolder);
    koReloadPlaylist:     mmpDo(evVMReloadPlaylist);
    koRenameFile:         mmpDo(evVMRenameCurrentItem);
    koResetAll:           MP.notify(newNotice(evMPResetAll));
    koRotateL:            MP.notify(newNotice(evMPRotateLeft));
    koRotateR:            MP.notify(newNotice(evMPRotateRight));
    koRotateReset:        MP.notify(newNotice(evMPRotateReset));
    koRunCut:             mmpOpenExternalApp(F11_APP, mmpDo(evPLReqCurrentItem).text);
    koRunPot:             mmpOpenExternalApp(F10_APP, mmpDo(evPLReqCurrentItem).text);
    koRunShot:            mmpOpenExternalApp(F12_APP, mmpDo(evPLReqCurrentItem).text);
    koShowCaption:        mmpDo(evPAPostToAll, WIN_CAPTION);
    koSpeedDn:            MP.notify(newNotice(evMPSpeedDn));
    koSpeedReset:         MP.notify(newNotice(evMPSpeedReset));
    koSpeedUp:            MP.notify(newNotice(evMPSpeedUp));
    koStartOver:          mmpDo(evPAPostToAll, WIN_START_OVER);
    koSysVolMax:          mmpDo(evMXSysVolMax); // someone or something needs to know this but we don't know (or care) who or what
    koTab:                mmpDo(evPAPostToAll, WIN_TAB);
    koTabTab:             mmpDo(evPAPostToAll, WIN_TABTAB);
    koSaturationDn:       MP.notify(newNotice(evMPSaturationDn));
    koSaturationReset:    MP.notify(newNotice(evMPSaturationReset));
    koSaturationUp:       MP.notify(newNotice(evMPSaturationUp));
    koScreenshot:         MP.notify(newNotice(evMPScreenshot, mmpScreenshotFolder));
    koSyncMedia:          mmpDo(evPAPostToEveryEx, message(WIN_SYNC_MEDIA, MP.notify(newNotice(evMPReqPosition)).integer, 0));
    koThumbnails:         mmpDo(evVMShowThumbs);
    koToggleProgressBar:  mmpDo(evPBToggleProgressBar);
    koToggleControls:     mmpDo(evPAPostToAll, WIN_TOGGLE_CONTROLS);
    koToggleEditMode:     mmpDo(evVMToggleEditMode);
    koToggleFiltering:    mmpDo(evVMToggleFiltering);
    koToggleHelp:         mmpDo(evVMToggleHelp);
    koToggleNumlock:      mmpToggleNumlock;
    koTogglePlaylist:     mmpDo(evVMTogglePlaylist);
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
