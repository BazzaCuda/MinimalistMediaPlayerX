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

{$ifdef FastMM5}
  {$ifopt D+}
    {$define UseFastMM5MemoryLeakReporting}
    {$ifdef UseFastMM5MemoryLeakReporting}
      {$define FastMM_DebugLibraryStaticDependency}
    {$endif}
  {$endif}
{$endif}

{$ifopt D+}
//  {$define designTimeThemes} // comment out when not required
{$endif}

{$R *.res}

{$R *.dres}

uses
  {$ifdef FastMM5}
  FastMM5 in '_FastMM5\FastMM5.pas',
  {$endif }
  {$ifdef designTimeThemes}
  vcl.forms,
  {$endif }
  _debugWindow in '_debugWindow\_debugWindow.pas',
  ALProgressBar in 'ALProgressBar.pas',
  MarkDownViewerComponents in '_MarkdownViewerComponents\MarkDownViewerComponents.pas',
  MediaInfoDLL in 'MediaInfoDLL.pas',
  MPVConst in '_libMPVDelphi\MPVConst.pas',
  MPVBasePlayer in '_libMPVDelphi\MPVBasePlayer.pas',
  TMPVHostClass in 'TMPVHostClass.pas',
  mmpAction in 'mmpAction.pas',
  mmpConsts in 'mmpConsts.pas',
  mmpDesktopUtils in 'mmpDesktopUtils.pas',
  mmpDialogs in 'mmpDialogs.pas',
  mmpExceptionHandler in 'mmpExceptionHandler.pas',
  mmpFileUtils in 'mmpFileUtils.pas',
  mmpFolderNavigation in 'mmpFolderNavigation.pas',
  mmpFolderUtils in 'mmpFolderUtils.pas',
  mmpFormatting in 'mmpFormatting.pas',
  mmpFormInputBox in 'mmpFormInputBox.pas',
  mmpCmd in 'mmpCmd.pas',
  mmpGlobalState in 'mmpGlobalState.pas',
  mmpImageUtils in 'mmpImageUtils.pas',
  mmpKeyboardUtils in 'mmpKeyboardUtils.pas',
  mmpMarkDownUtils in 'mmpMarkDownUtils.pas',
  mmpMenu in 'mmpMenu.pas',
  mmpMMDevApi_tlb in 'mmpMMDevApi_tlb.pas',
  mmpNotify.notices in 'mmpNotify.notices.pas',
  mmpNotify.notifier in 'mmpNotify.notifier.pas',
  mmpNotify.subscriber in 'mmpNotify.subscriber.pas',
  mmpPanelCtrls in 'mmpPanelCtrls.pas',
  mmpPostToAllUtils in 'mmpPostToAllUtils.pas',
  mmpProgramUpdates in 'mmpProgramUpdates.pas',
  mmpRTL in 'mmpRTL.pas',
  mmpShellUtils in 'mmpShellUtils.pas',
  mmpShredUtils in 'mmpShredUtils.pas',
  mmpStackTrace in 'mmpStackTrace.pas',
  mmpThumbUtils in 'mmpThumbUtils.pas',
  mmpTicker in 'mmpTicker.pas',
  mmpTickTimer in 'mmpTickTimer.pas',
  mmpUserFolders in 'mmpUserFolders.pas',
  mmpUtils in 'mmpUtils.pas',
  mmpVCL in 'mmpVCL.pas',
  mmpWindowUtils in 'mmpWindowUtils.pas',
  TCleanupClass in 'TCleanupClass.pas',
  TlistHelperClass in 'TlistHelperClass.pas',
  TMediaStreamClass in 'TMediaStreamClass.pas',
  TSegmentClass in 'TSegmentClass.pas',
  TStatusBarHelperClass in 'TStatusBarHelperClass.pas',
  TThumbClass in 'TThumbClass.pas',
  TThumbsClass in 'TThumbsClass.pas',
  model.mmpBookmark in 'model.mmpBookmark.pas',
  model.mmpConfigFile in 'model.mmpConfigFile.pas',
  model.mmpKeyFrames in 'model.mmpKeyFrames.pas',
  model.mmpMediaInfo in 'model.mmpMediaInfo.pas',
  model.mmpMediaPlayer in 'model.mmpMediaPlayer.pas',
  model.mmpMediaTypes in 'model.mmpMediaTypes.pas',
  model.mmpMixer in 'model.mmpMixer.pas',
  model.mmpMPVCtrls in 'model.mmpMPVCtrls.pas',
  model.mmpMPVFormatting in 'model.mmpMPVFormatting.pas',
  model.mmpMPVProperties in 'model.mmpMPVProperties.pas',
  model.mmpParamStrings in 'model.mmpParamStrings.pas',
  model.mmpPlaylist in 'model.mmpPlaylist.pas',
  model.mmpPlaylistUtils in 'model.mmpPlaylistUtils.pas',
  model.mmpUndoMove in 'model.mmpUndoMove.pas',
  view.mmpFormAbout in 'view.mmpFormAbout.pas',
  view.mmpFormCaptions in 'view.mmpFormCaptions.pas',
  view.mmpFormConfig in 'view.mmpFormConfig.pas',
  view.mmpFormConfirmDelete in 'view.mmpFormConfirmDelete.pas',
  view.mmpFormDownload in 'view.mmpFormDownload.pas',
  view.mmpFormHelp in 'view.mmpFormHelp.pas',
  view.mmpFormMain in 'view.mmpFormMain.pas',
  view.mmpFormMainCaption in 'view.mmpFormMainCaption.pas',
  view.mmpFormPlaylist in 'view.mmpFormPlaylist.pas',
  view.mmpFormProgress in 'view.mmpFormProgress.pas',
  view.mmpFormReleaseNotes in 'view.mmpFormReleaseNotes.pas',
  view.mmpFormStreamList in 'view.mmpFormStreamList.pas',
  view.mmpFormThumbs in 'view.mmpFormThumbs.pas',
  view.mmpFormTimeline in 'view.mmpFormTimeline.pas',
  view.mmpKeyboardMain in 'view.mmpKeyboardMain.pas',
  view.mmpKeyboardThumbs in 'view.mmpKeyboardThumbs.pas',
  view.mmpProgressBar in 'view.mmpProgressBar.pas',
  view.mmpThemeUtils in 'view.mmpThemeUtils.pas',
  viewModel.mmpKeyboardOps in 'viewModel.mmpKeyboardOps.pas',
  viewModel.mmpVM in 'viewModel.mmpVM.pas';

procedure setupRunMode;
begin
  reportMemoryLeaksOnShutdown := mmpEnvironmentVariable; // done already in mmpStackTrace initialization section - unless that unit has been commented out
  {$if BazDebugWindow} debugBoolean('reportMemoryLeaksOnShutdown', reportMemoryLeaksOnShutdown); {$endif}

  {$ifdef FastMM5}
  FastMM_SetOptimizationStrategy(mmosOptimizeForSpeed);
//  {$if BazDebugWindow} debugClear; {$endif}            // done already in mmpStackTrace initialization section - unless that unit has been commented out or we have D+ set

  case reportMemoryLeaksOnShutdown of TRUE: begin
                                              {$if BazDebugWindow} debugBoolean('reportMemoryLeaksOnShutdown', reportMemoryLeaksOnShutdown); {$endif}
                                              {$ifdef useFastMM5MemoryLeakReporting}
                                              // FastMM_DeleteEventLogFile;
                                              FastMM_EnterDebugMode;
                                              FastMM_LogToFileEvents  := FastMM_LogToFileEvents   + [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
                                              FastMM_MessageBoxEvents := FastMM_MessageBoxEvents  + [mmetUnexpectedMemoryLeakSummary];
                                              {$endif}
                                            end;end;
  {$endif}
end;

function checkParam: boolean;
var T: TProc;
begin
  result := FALSE;
  T := procedure  begin
                    mmpShowOKCancelMsgDlg('Typically, you would use "Open with..." in your File Explorer / Manager, to open a media file'#13#10
                                        + 'or to permanently associate media file types with this application.'#13#10#13#10
                                        + 'Alternatively, you can drag and drop a media file onto the window.'); end;
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
  {$ifdef designTimeThemes}
  // design-time only - not for actual builds
  // this is purely to get around the Delphi IDE's nonsense of needing these lines
  // in order to show Project/Options/Appearance and use the selected VCL style at design time
  application.Initialize;
  TStyleManager.trySetStyle('Charcoal Dark Slate');
  var mainUI: TMMPUI;
  application.createForm(TMMPUI, mainUI);
  application.run;
  {$else}

  setupRunMode;

  TStyleManager.trySetStyle(MMP_STYLE_DARKMODE);

  app.initialize;
  app.showMainForm      := FALSE;
  app.mainFormOnTaskbar := TRUE;
  app.title             := MMP_TITLE;

  checkParam;

  CF.initConfigFile(mmpConfigFilePath);
  mmp.cmd(evGSRepeatDelayMs, CF.asInteger[CONF_REPEAT_DELAY_MS]);
  mmp.cmd(GS.repeatDelayMs <= 0, evGSRepeatDelayMs, DEFAULT_REPEAT_DELAY_MS);

  app.createForm(TMMPUI, MMPUI);
  mmp.cmd(evGSMainForm, MMPUI);

  try
    app.onException := mmpException.handler;
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

  mmp.cmd((lowerCase(CF[CONF_OPEN_IMAGE]) = CONF_BROWSER) and (GS.mediaType = mtImage), [evMPStop, evVMImageInBrowser]);

  app.Run; // now it's ok to raise test exceptions
  {$endif}
end.
