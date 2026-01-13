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
  model.mmpConfigFile, model.mmpMediaPlayer;

function mmpProcessKeyOp(const MP: IMediaPlayer; var SS: TSnapshot): boolean;

implementation

uses
  bazCmd,
  mmpPostToAllUtils, mmpWindowUtils,
  model.mmpBookmark, model.mmpMixer, model.mmpPlaylistUtils,
  _debugWindow;

function sendOpInfo(const aOpInfo: string): boolean;
begin
  mmp.cmd(evSTOpInfo, aOpInfo);
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

    koAboutBox:           mmp.cmd(evAboutFormShow);
    koAdjustAspectRatio:  mmp.cmd(evVMAdjustAspectRatio);
    koArrangeAll:         mmp.cmd(evVMArrangeAll);
    koBookmarkDelete:     newBookmark.delete(MP.notify(newNotice(evMPReqFileName)).text);
    koBookmarkLoad:       MP.notify(newNotice(evPBClick, newBookmark.position(MP.notify(newNotice(evMPReqFileName)).text)));
    koBookmarkSave:       newBookmark.save(MP.notify(newNotice(evMPReqFileName)).text, MP.notify(newNotice(evMPReqPosition)).integer);
    koBrighterPB:         begin mmp.cmd(evMCBrighter); mmp.cmd(evPBBrighter); mmp.cmd(evSTBrighter); end;
    koBrightnessDn:       MP.notify(newNotice(evMPBrightnessDn));
    koBrightnessReset:    MP.notify(newNotice(evMPBrightnessReset));
    koBrightnessUp:       MP.notify(newNotice(evMPBrightnessUp));
    koCleanup:            mmp.cmd(evVMCleanup);
    koCentreWindow:       mmp.cmd(evVMCenterWindow);
    koClipboard:          sendOpInfo(mmp.cmd(evPLCopyToClipboard).text);
    koCloseEvery:         mmp.cmd(evPAPostToEvery, WIN_CLOSEAPP);
    koCloseApp:           mmp.cmd(evAppClose);
    koConfig:             mmp.cmd(evVMConfig);
    koContrastDn:         MP.notify(newNotice(evMPContrastDn));
    koContrastReset:      MP.notify(newNotice(evMPContrastReset));
    koContrastUp:         MP.notify(newNotice(evMPContrastUp));
    koCycleAudio:         MP.notify(newNotice(evMPCycleAudio));
    koCycleSubs:          MP.notify(newNotice(evMPCycleSubs));
    koDarkerPB:           begin mmp.cmd(evMCDarker); mmp.cmd(evPBDarker); mmp.cmd(evSTDarker); end;
    koDeleteCurrentItem:  mmp.cmd(evVMDeleteCurrentItem, SS.shiftState);
    koEscape:             mmp.cmd(evVMDoEscapeKey);
    koExploreFolder:      mmpShellExec(mmp.cmd(evPLReqCurrentFolder).text);
    koFrameBackwards:     MP.notify(newNotice(evMPFrameBackwards));
    koFrameForwards:      MP.notify(newNotice(evMPFrameForwards));
    koFullscreen:         notifyApp(mmp.cmd(evVMToggleFullscreen));
    koGammaDn:            MP.notify(newNotice(evMPGammaDn));
    koGammaReset:         MP.notify(newNotice(evMPGammaReset));
    koGammaUp:            MP.notify(newNotice(evMPGammaUp));
    koGreaterWindow:      mmp.cmd(evPAPostToAll, WIN_GREATER);
    koHelpFull:           mmp.cmd(evVMHelpFull);
    koKeep:               mmp.cmd(evVMKeepCurrentItem);
    koKeepCatF1:          mmp.cmd(evVMKeepCatF1);
    koKeepCatF2:          mmp.cmd(evVMKeepCatF2);
    koKeepCatF3:          mmp.cmd(evVMKeepCatF3);
    koKeepCatF4:          mmp.cmd(evVMKeepCatF4);
    koKeepDelete:         mmp.cmd(evVMKeepDelete);
    koKeepMove:           mmp.cmd(evVMKeepMove);
    koKeepSave:           mmp.cmd(evVMKeepSave);
    koImageInBrowser:     mmp.cmd(evVMImageInBrowser);
    koMaximize:           begin mmp.cmd(evGSAutoCenter, TRUE); mmp.cmd(evGSMaxSize, TRUE); mmp.cmd(evVMResizeWindow); end; // maximize the video according to the height of the screen
    koMinimizeWindow:     mmp.cmd(evVMMinimize);
    koMuteUnmute:         mmp.cmd(evPAPostToAll, WIN_MUTE_UNMUTE);
    koNextChapter:        MP.notify(newNotice(evMPNextChapter));
    koPanDn:              MP.notify(newNotice(evMPPanDn));
    koPanLeft:            MP.notify(newNotice(evMPPanLeft));
    koPanReset:           MP.notify(newNotice(evMPPanReset));
    koPanRight:           MP.notify(newNotice(evMPPanRight));
    koPanUp:              MP.notify(newNotice(evMPPanUP));
    koPausePlay:          mmp.cmd(evPAPostToAll, WIN_PAUSE_PLAY);
    koPBReset:            begin mmp.cmd(evMCReset); mmp.cmd(evPBReset); mmp.cmd(evSTReset); end;
    koPrevChapter:        MP.notify(newNotice(evMPPrevChapter));
    koPlayEdited:         mmp.cmd(evVMMPPlayEdited);
    koPlayFirst:          mmp.cmd(evVMMPPlayFirst);
    koPlayLast:           mmp.cmd(evVMMPPlayLast);
    koPlayNext:           mmp.cmd(evVMMPPlayNext, CF.asBoolean[CONF_NEXT_FOLDER_ON_END]);
    koPlayNextFolder:     mmp.cmd(evVMPlayNextFolder, TRUE);
    koPlayPrev:           mmp.cmd(evVMMPPlayPrev);
    koPlayPrevFolder:     mmp.cmd(evVMPlayPrevFolder);
    koReloadPlaylist:     mmp.cmd(evVMReloadPlaylist);
    koRenameCleanFile:    mmp.cmd(evVMRenameCleanFile);
    koRenameFile:         mmp.cmd(evVMRenameCurrentItem);
    koResetAll:           MP.notify(newNotice(evMPResetAll));
    koRotateL:            MP.notify(newNotice(evMPRotateLeft));
    koRotateR:            MP.notify(newNotice(evMPRotateRight));
    koRotateReset:        MP.notify(newNotice(evMPRotateReset));
    koRunCut:             mmpOpenExternalApp(F11_APP, mmp.cmd(evPLReqCurrentItem).text);
    koRunPot:             mmpOpenExternalApp(F10_APP, mmp.cmd(evPLReqCurrentItem).text);
    koRunShot:            mmpOpenExternalApp(F12_APP, mmp.cmd(evPLReqCurrentItem).text);
    koShowCaption:        mmp.cmd(evPAPostToAll, WIN_CAPTION);
    koSpeedDn:            MP.notify(newNotice(evMPSpeedDn));
    koSpeedReset:         MP.notify(newNotice(evMPSpeedReset));
    koSpeedUp:            MP.notify(newNotice(evMPSpeedUp));
    koStartOver:          mmp.cmd(evPAPostToAll, WIN_START_OVER);
    koSyncAudioUp:        MP.notify(newNotice(evMPSyncAudioUp));
    koSyncAudioDn:        MP.notify(newNotice(evMPSyncAudioDn));
    koSysVolMax:          mmp.cmd(evMXSysVolMax); // someone or something needs to know this but we don't know (or care) who or what
    koTab:                mmp.cmd(evPAPostToAll, WIN_TAB);
    koTabTab:             mmp.cmd(evPAPostToAll, WIN_TABTAB);
    koSaturationDn:       MP.notify(newNotice(evMPSaturationDn));
    koSaturationReset:    MP.notify(newNotice(evMPSaturationReset));
    koSaturationUp:       MP.notify(newNotice(evMPSaturationUp));
    koScreenshot:         MP.notify(newNotice(evMPScreenshot, mmpScreenshotFolder));
    koSyncMedia:          mmp.cmd(evPAPostToEveryEx, message(WIN_SYNC_MEDIA, MP.notify(newNotice(evMPReqPosition)).integer, 0));
    koThumbnails:         mmp.cmd(evVMShowThumbs);
    koToggleControls:     mmp.cmd(evPAPostToAll, WIN_TOGGLE_CONTROLS);
    koToggleEditMode:     mmp.cmd(evPAPostToAll, WIN_TOGGLE_EDIT_MODE);
    koToggleFiltering:    mmp.cmd(evVMToggleFiltering);
    koToggleHelp:         mmp.cmd(evVMToggleHelp);
    koToggleNumlock:      mmpToggleNumlock;
    koTogglePlaylist:     mmp.cmd(evVMTogglePlaylist);
    koToggleProgressBar:  mmp.cmd(evPBToggleProgressBar);
    koToggleRepeat:       mmp.cmd(evPAPostToAll, WIN_TOGGLE_REPEAT);
    koToggleShuffle:      mmp.cmd(evVMToggleShuffle);
    koToggleSkipExcluded: mmp.cmd(evVMToggleSkipExcluded);
    koToggleSubtitles:    MP.notify(newNotice(evMPToggleSubtitles));
    koVolDn:              MP.notify(newNotice(evMPVolDn));
    koVolUp:              MP.notify(newNotice(evMPVolUp));
    koWiki:               mmpShellExec(MMP_WIKI_URL);
    koZoomIn:             MP.notify(newNotice(evMPZoomIn));
    koZoomOut:            MP.notify(newNotice(evMPZoomOut));
    koZoomReset:          MP.notify(newNotice(evMPZoomReset));
  end;

  SS.handled := TRUE;
  result     := TRUE;
end;


end.
