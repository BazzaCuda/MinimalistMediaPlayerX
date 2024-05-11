{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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





{$R *.dres}

uses
  Vcl.Forms,
  formMain in 'formMain.pas' {MMPUI},
  consts in 'consts.pas',
  formAbout in 'formAbout.pas' {AboutForm},
  TUICtrlsClass in 'TUICtrlsClass.pas',
  ALProgressBar in 'ALProgressBar.pas',
  Vcl.Themes,
  Vcl.Styles,
  globalVars in 'globalVars.pas',
  TProgressBarClass in 'TProgressBarClass.pas',
  TKeyboardClass in 'TKeyboardClass.pas',
  appEvents in 'appEvents.pas',
  TSysCommandsClass in 'TSysCommandsClass.pas',
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
  formHelp in 'formHelp.pas' {HelpForm},
  TPlaylistClass in 'TPlaylistClass.pas',
  TMediaPlayerClass in 'TMediaPlayerClass.pas',
  formSubtitles in 'formSubtitles.pas' {subtitlesForm},
  TTickTimerClass in 'TTickTimerClass.pas',
  mediaInfo in 'mediaInfo.pas',
  MediaInfoDLL in 'MediaInfoDLL.pas',
  formCaption in 'formCaption.pas' {CaptionForm},
  MPVBasePlayer in 'libMPVDelphi\MPVBasePlayer.pas',
  MPVClient in 'libMPVDelphi\MPVClient.pas',
  MPVConst in 'libMPVDelphi\MPVConst.pas',
  MPVNode in 'libMPVDelphi\MPVNode.pas',
  MPVRender in 'libMPVDelphi\MPVRender.pas',
  MPVRenderGL in 'libMPVDelphi\MPVRenderGL.pas',
  MPVStreamCB in 'libMPVDelphi\MPVStreamCB.pas',
  MPVTrack in 'libMPVDelphi\MPVTrack.pas',
  TParamStringsClass in 'TParamStringsClass.pas',
  formInputBox in 'formInputBox.pas' {InputBoxForm},
  commonUtils in 'commonUtils.pas',
  TBookmarkClass in 'TBookmarkClass.pas',
  configFile in 'configFile.pas',
  mediaType in 'mediaType.pas',
  formPlaylist in 'formPlaylist.pas' {PlaylistForm},
  TSendAllClass in 'TSendAllClass.pas',
  formTimeline in 'formTimeline.pas' {TimelineForm},
  formStreamList in 'formStreamList.pas' {StreamListForm},
  formProgress in 'formProgress.pas' {ProgressForm},
  TSegmentClass in 'TSegmentClass.pas',
  TProgramUpdatesClass in 'TProgramUpdatesClass.pas',
  formDownload in 'formDownload.pas' {DownloadForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRUE;
  debugClear;

  Application.Initialize;
  Application.ShowMainForm := FALSE;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TMMPUI, MMPUI);
  Application.Run;
end.
