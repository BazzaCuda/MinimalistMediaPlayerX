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

uses
  Vcl.Forms,
  formMain in 'formMain.pas' {MMPUI},
  consts in 'consts.pas',
  formAbout in 'formAbout.pas' {AboutForm},
  uiCtrls in 'uiCtrls.pas',
  ALProgressBar in 'ALProgressBar.pas',
  Vcl.Themes,
  Vcl.Styles,
  globalVars in 'globalVars.pas',
  progressBar in 'progressBar.pas',
  keyboard in 'keyboard.pas',
  appEvents in 'appEvents.pas',
  sysCommands in 'sysCommands.pas',
  commonUtils in 'commonUtils.pas',
  _debugWindow in '..\DebugWindow\_debugWindow.pas',
  formHelp in 'formHelp.pas' {HelpForm},
  playlist in 'playlist.pas',
  mediaPlayer in 'mediaPlayer.pas',
  MMFMediaEngineClass in 'MMFMediaEngineClass.pas',
  MMFTimedTextNotifyClass in 'MMFTimedTextNotifyClass.pas',
  formSubtitles in 'formSubtitles.pas' {subtitlesForm},
  types in 'types.pas',
  tickTimer in 'tickTimer.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := TRUE;
  debugClear;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Charcoal Dark Slate');
  Application.CreateForm(TMMPUI, MMPUI);
  Application.Run;
end.
