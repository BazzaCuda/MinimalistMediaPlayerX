{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda <bazzacuda@gmx.com>
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

{$R *.res}

{$R *.dres}

uses
  system.sysUtils,
  vcl.dialogs,
  vcl.extCtrls,
  vcl.forms,
  vcl.styles,
  vcl.themes,
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
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
  MPVClient in 'libMPVDelphi\MPVClient.pas',
  MPVConst in 'libMPVDelphi\MPVConst.pas',
  MPVNode in 'libMPVDelphi\MPVNode.pas',
  MPVRender in 'libMPVDelphi\MPVRender.pas',
  MPVRenderGL in 'libMPVDelphi\MPVRenderGL.pas',
  MPVStreamCB in 'libMPVDelphi\MPVStreamCB.pas',
  MPVTrack in 'libMPVDelphi\MPVTrack.pas',
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
  view.mmpKeyboard in 'view.mmpKeyboard.pas',
  mmpConsts in 'mmpConsts.pas',
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
  view.mmpThumbsKeyboard in 'view.mmpThumbsKeyboard.pas',
  TMPVHostClass in 'TMPVHostClass.pas',
  TThumbsClass in 'TThumbsClass.pas',
  TThumbClass in 'TThumbClass.pas',
  mmpThumbUtils in 'mmpThumbUtils.pas',
  mmpPanelCtrls in 'mmpPanelCtrls.pas',
  TStatusBarHelperClass in 'TStatusBarHelperClass.pas',
  mmpUserFolders in 'mmpUserFolders.pas',
  mmpTicker in 'mmpTicker.pas',
  mmpFormatting in 'mmpFormatting.pas';

function checkParam: boolean;
begin
  result := FALSE;
  case PS.noFile of TRUE:  begin
                                mmpShowOKCancelMsgDlg('Typically, you would use "Open with..." in your File Explorer / Manager, to open a media file'#13#10
                                                    + 'or to permanently associate media file types with this application.'#13#10#13#10
                                                    + 'Alternatively, you can drag and drop a media file onto the window.',
                                                      mtInformation, [MBOK]); end;end;
  result := TRUE;

end;

function initUI(const aMainForm: TForm): boolean;
var
  vVideoPanel: TPanel;
begin
  result := FALSE;
  mmpThemeInitForm(aMainForm);
  vVideoPanel                   := mmpThemeCreateVideoPanel(aMainForm);
  MMPUI.viewModel               := newViewModel;
  MMPUI.viewModel.mediaPlayer   := newMediaPlayer;
  MMPUI.viewModel.initUI(aMainForm, vVideoPanel);

  MC(aMainForm).initCaption(vVideoPanel, CF.asInteger[CONF_MAIN_CAPTION]);  // single caption at the top of the window
  ST(aMainForm).initCaptions(vVideoPanel, CF.asInteger[CONF_TIME_CAPTION]); // multiple captions at the bottom of the window

  MMPUI.viewModel.progressBar   := newProgressBar.initProgressBar(ST.captionsForm, CF.asInteger[CONF_PROGRESS_BAR], 0);
  result := TRUE;
end;

function checkBrowser: boolean;
begin
  result := FALSE;

  case (lowerCase(CF[CONF_OPEN_IMAGE]) = 'browser') of FALSE: EXIT; end;

  case GS.mediaType of mtImage: notifyApp(newNotice(evVMImageInBrowser)); end;

  result := TRUE;
end;

begin
  reportMemoryLeaksOnShutdown   := TRUE;
  debugClear;

  application.initialize;
  application.showMainForm      := FALSE;
  application.mainFormOnTaskbar := TRUE;
  TStyleManager.trySetStyle('Charcoal Dark Slate');
  application.title             := 'MMP: Minimalist Media Player';

  checkParam;

  CF.initConfigFile(mmpConfigFilePath);

  Application.CreateForm(TMMPUI, MMPUI);
  notifyApp(newNotice(evGSMainForm, MMPUI));

  initUI(MMPUI);

  MMPUI.viewModel.playlist      := newPlaylist;

  MMPUI.viewModel.playlist.notify(newNotice(evPLFillPlaylist, PS.fileFolder));

  case notifyApp(newNotice(evPLFind, PS.fileFolderAndName)).tf of TRUE: notifyApp(newNotice(evVMMPPlayCurrent)); end;

  MMPUI.viewModel.showUI;

  notifyApp(newNotice(evSTForceCaptions));

  checkBrowser;

  application.Run;
end.
