{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda <bazzacuda@gmx.com>
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
program MinimalistMediaPlayer;

{$ifopt D+}
  {$define UseFastMM5MemoryLeakReporting}
  {$ifdef UseFastMM5MemoryLeakReporting}
    {$define FastMM_DebugLibraryStaticDependency}
  {$endif}
{$endif}

{$R *.res}

{$R *.dres}

uses
  FastMM5 in 'FastMM5\FastMM5.pas',
  _debugWindow in '_debugWindow\_debugWindow.pas',
  system.sysUtils,
  vcl.dialogs,
  vcl.extCtrls,
  vcl.forms,
  vcl.styles,
  vcl.themes,
  view.mmpFormMain in 'view.mmpFormMain.pas',
  mmpNotify.notices in 'mmpNotify.notices.pas',
  mmpNotify.notifier in 'mmpNotify.notifier.pas',
  mmpNotify.subscriber in 'mmpNotify.subscriber.pas',
  mmpGlobalState in 'mmpGlobalState.pas',
  view.mmpThemeUtils in 'view.mmpThemeUtils.pas',
  mmpDesktopUtils in 'mmpDesktopUtils.pas',
  view.mmpFormCaptions in 'view.mmpFormCaptions.pas',
  view.mmpFormMainCaption in 'view.mmpFormMainCaption.pas',
  mmpUtils in 'mmpUtils.pas',
  mmpPostToAllUtils in 'mmpPostToAllUtils.pas',
  MPVBasePlayer in 'libMPVDelphi\MPVBasePlayer.pas',
  MPVConst in 'libMPVDelphi\MPVConst.pas',
  model.mmpMediaPlayer in 'model.mmpMediaPlayer.pas',
  model.mmpConfigFile in 'model.mmpConfigFile.pas',
  viewModel.mmpVM in 'viewModel.mmpVM.pas',
  mmpFileUtils in 'mmpFileUtils.pas',
  ALProgressBar in 'ALProgressBar.pas',
  view.mmpProgressBar in 'view.mmpProgressBar.pas',
  model.mmpMPVFormatting in 'model.mmpMPVFormatting.pas',
  model.mmpParamStrings in 'model.mmpParamStrings.pas',
  mmpDialogs in 'mmpDialogs.pas',
  model.mmpPlaylist in 'model.mmpPlaylist.pas',
  TlistHelperClass in 'TlistHelperClass.pas',
  model.mmpMediaTypes in 'model.mmpMediaTypes.pas',
  model.mmpMPVCtrls in 'model.mmpMPVCtrls.pas',
  model.mmpMPVProperties in 'model.mmpMPVProperties.pas',
  mmpTickTimer in 'mmpTickTimer.pas',
  view.mmpKeyboardMain in 'view.mmpKeyboardMain.pas',
  mmpShellUtils in 'mmpShellUtils.pas',
  viewModel.mmpKeyboardOps in 'viewModel.mmpKeyboardOps.pas',
  mmpMMDevApi_tlb in 'mmpMMDevApi_tlb.pas',
  model.mmpMixer in 'model.mmpMixer.pas',
  view.mmpFormHelp in 'view.mmpFormHelp.pas',
  mmpMarkDownUtils in 'mmpMarkDownUtils.pas',
  MediaInfoDLL in 'MediaInfoDLL.pas',
  model.mmpMediaInfo in 'model.mmpMediaInfo.pas',
  TMediaStreamClass in 'TMediaStreamClass.pas',
  view.mmpFormPlaylist in 'view.mmpFormPlaylist.pas',
  model.mmpPlaylistUtils in 'model.mmpPlaylistUtils.pas',
  model.mmpBookmark in 'model.mmpBookmark.pas',
  view.mmpFormReleaseNotes in 'view.mmpFormReleaseNotes.pas',
  mmpProgramUpdates in 'mmpProgramUpdates.pas',
  view.mmpFormDownload in 'view.mmpFormDownload.pas',
  mmpFormInputBox in 'mmpFormInputBox.pas',
  view.mmpFormConfirmDelete in 'view.mmpFormConfirmDelete.pas',
  view.mmpFormProgress in 'view.mmpFormProgress.pas',
  mmpKeyboardUtils in 'mmpKeyboardUtils.pas',
  view.mmpFormAbout in 'view.mmpFormAbout.pas',
  mmpWindowUtils in 'mmpWindowUtils.pas',
  mmpFolderNavigation in 'mmpFolderNavigation.pas',
  mmpFolderUtils in 'mmpFolderUtils.pas',
  model.mmpUndoMove in 'model.mmpUndoMove.pas',
  view.mmpFormTimeline in 'view.mmpFormTimeline.pas',
  TSegmentClass in 'TSegmentClass.pas',
  view.mmpFormStreamList in 'view.mmpFormStreamList.pas',
  mmpImageUtils in 'mmpImageUtils.pas',
  view.mmpFormThumbs in 'view.mmpFormThumbs.pas',
  view.mmpKeyboardThumbs in 'view.mmpKeyboardThumbs.pas',
  TMPVHostClass in 'TMPVHostClass.pas',
  TThumbsClass in 'TThumbsClass.pas',
  TThumbClass in 'TThumbClass.pas',
  mmpThumbUtils in 'mmpThumbUtils.pas',
  mmpPanelCtrls in 'mmpPanelCtrls.pas',
  TStatusBarHelperClass in 'TStatusBarHelperClass.pas',
  mmpUserFolders in 'mmpUserFolders.pas',
  mmpTicker in 'mmpTicker.pas',
  mmpFormatting in 'mmpFormatting.pas',
  mmpShredUtils in 'mmpShredUtils.pas',
  TCleanupClass in 'TCleanupClass.pas',
  mmpMenu in 'mmpMenu.pas',
  MarkDownViewerComponents in 'MarkdownViewerComponents\MarkDownViewerComponents.pas',
  mmpConsts in 'mmpConsts.pas',
  mmpFuncProg in 'mmpFuncProg.pas',
  mmpStackTrace in 'mmpStackTrace.pas',
  mmpExceptionHandler in 'mmpExceptionHandler.pas',
  view.mmpFormConfig in 'view.mmpFormConfig.pas',
  model.mmpKeyFrames in 'model.mmpKeyFrames.pas';

procedure setupRunMode;
begin
  reportMemoryLeaksOnShutdown := mmpEnvironmentVariable; // has already been set in mmpStackTrace initialization section - unless that unit has been commented out
  FastMM_SetOptimizationStrategy(mmosOptimizeForSpeed);
  {$if BazDebugWindow} debugClear; {$endif}

  case reportMemoryLeaksOnShutdown of TRUE: begin
                                              {$if BazDebugWindow} debugBoolean('reportMemoryLeaksOnShutdown', reportMemoryLeaksOnShutdown); {$endif}
                                              {$ifdef useFastMM5MemoryLeakReporting}
                                              // FastMM_DeleteEventLogFile;
                                              FastMM_EnterDebugMode;
                                              FastMM_LogToFileEvents  := FastMM_LogToFileEvents   + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
                                              FastMM_MessageBoxEvents := FastMM_MessageBoxEvents  + [mmetUnexpectedMemoryLeakSummary];
                                              {$endif}
                                            end;end;
end;

function checkParam: boolean;
var T: TProc;
begin
  result := FALSE;
  T := procedure  begin
                    mmpShowOKCancelMsgDlg('Typically, you would use "Open with..." in your File Explorer / Manager, to open a media file'#13#10
                                        + 'or to permanently associate media file types with this application.'#13#10#13#10
                                        + 'Alternatively, you can drag and drop a media file onto the window.',
                                          mtInformation, [MBOK]); end;
  mmp.cmd(PS.noFile, T);
  result := TRUE;
end;

function initUI(const aMainForm: TForm): boolean;
var
  vVideoPanel: TPanel;
begin
  result := FALSE;

  vVideoPanel                   := mmpThemeCreateVideoPanel(aMainForm);

  MMPUI.viewModel               := newViewModel;
  MMPUI.viewModel.mediaPlayer   := newMediaPlayer;
  MMPUI.viewModel.initUI(aMainForm, vVideoPanel);

  MC(aMainForm).initCaption(vVideoPanel, CF.asInteger[CONF_MAIN_CAPTION]);  // Main Caption: single caption at the top of the window
  ST(aMainForm).initCaptions(vVideoPanel, CF.asInteger[CONF_TIME_CAPTION]); // Sub-Titles: multiple captions at the bottom of the window

  MMPUI.viewModel.progressBar   := newProgressBar.initProgressBar(ST.captionsForm, CF.asInteger[CONF_PROGRESS_BAR], 0);

  result := TRUE;
end;

begin
  setupRunMode;

  application.initialize;
  application.showMainForm      := FALSE;
  application.mainFormOnTaskbar := TRUE;
  TStyleManager.trySetStyle('Charcoal Dark Slate');
  application.title             := 'MMP: Minimalist Media Player';

  checkParam;

  CF.initConfigFile(mmpConfigFilePath);
  mmp.cmd(evGSRepeatDelayMs, CF.asInteger[CONF_REPEAT_DELAY_MS]);
  mmp.cmd(GS.repeatDelayMs <= 0, evGSRepeatDelayMs, DEFAULT_REPEAT_DELAY_MS);

  Application.CreateForm(TMMPUI, MMPUI);
  mmp.cmd(evGSMainForm, MMPUI);

  try
    application.onException := mmpException.handler;
  except end;

  initUI(MMPUI);

  MMPUI.viewModel.playlist := newPlaylist;

  mmp.cmd(evGSNoPlaylist, PS.noPlaylist);

  case GS.noPlaylist of FALSE: MMPUI.viewModel.playlist.notify(newNotice(evPLFillPlaylist, PS.fileFolder, CF.asMediaType[CONF_PLAYLIST_FORMAT])); end;

  MMPUI.viewModel.playlist.add(PS.fileFolderAndName); // always include the file the user used to launch MMP, which includes the [edited] file from the Timeline
  MMPUI.viewModel.playlist.sort;                      // force a resort now we've manually added a file at the end of the playlist

  mmp.cmd(mmp.cmd(evPLFind, PS.fileFolderAndName).tf, evVMMPPlayCurrent);
  mmp.cmd(evMPKeepOpen, GS.noPlaylist); // because MP.openURL will have set it to false for audio and video

  MMPUI.viewModel.showUI; // if we open an image in the browser (below), this gives us the window dimensions and desktop location to copy

  mmp.cmd(evSTForceCaptions);

  mmp.cmd((GS.mediaType = mtVideo) and CF.asBoolean[CONF_START_IN_EDITOR] and NOT mmpShiftKeyDown and NOT GS.noPlaylist, evVMToggleEditMode);

  mmp.cmd((lowerCase(CF[CONF_OPEN_IMAGE]) = 'browser') and (GS.mediaType = mtImage), [evMPStop, evVMImageInBrowser]);

  application.Run; // now it's ok to raise test exceptions
end.
