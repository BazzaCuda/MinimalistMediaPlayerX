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
unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, consts;

type
  TMMPUI = class(TForm)
    procedure WMSysCommand(var message : TWMSysCommand); message WM_SYSCOMMAND;
    procedure WMTimerUpdate(var message: TMessage); message WM_TIMERUPDATE;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
  public
  end;

var
  MMPUI: TMMPUI;

implementation

uses
  {consts, }formAbout, uiCtrls, globalVars, progressBar, sysCommands, mediaPlayer, playlist, _debugWindow;

{$R *.dfm}

{ TMMPUI }

procedure TMMPUI.FormCreate(Sender: TObject);
begin
  initUI(SELF);
  PB.initProgressBar(SELF);
  GV.mainWnd        := SELF.handle;
  MP.initMediaPlayer(SELF);

  GV.mainForm := SELF;

  PL.add('B:\Movies\Blazing Saddles (1974).mp4');
  PL.add('B:\Movies\Nobody (2021).mp4');
  PL.add('B:\Movies\Leon The Professional (1994) extended.mp4');
  PL.add('B:\Movies\LOTR 1 - The Fellowship of the Ring (2001) extended.mp4');
  PL.add('B:\Movies\Blazing Saddles (1974).mp4');

  PL.first;

  case MP.openURL(PL.currentItem) of FALSE: EXIT; end;

  MP.play;
  MP.volume := 100;
end;

procedure TMMPUI.FormResize(Sender: TObject);
begin
  GV.mainTop   := top;
  GV.mainLeft  := left;
  GV.mainWidth := width;
end;

procedure TMMPUI.WMSysCommand(var message: TWMSysCommand);
// respond to the WM_SYSCOMMAND messages this app sends to itself
begin
  inherited;
  doSysCommand(message);
end;

procedure TMMPUI.WMTimerUpdate(var message: TMessage);
begin
  MP.setProgressBar;
end;

end.
