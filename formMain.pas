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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, alProgressBar;

type
  TMMPUI = class(TForm)
    procedure WMSysCommand(var Message : TWMSysCommand); Message WM_SYSCOMMAND;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    function  createProgressBar: boolean;
    procedure progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
  end;

var
  MMPUI: TMMPUI;

implementation

uses
  consts, formAbout, uiCtrls, globalVars;

{$R *.dfm}

{ TMMPUI }

function TMMPUI.createProgressBar: boolean;
begin
  initProgressBar(SELF);
  PB.OnMouseMove := progressBarMouseMove;
  PB.onMouseUp   := progressBarMouseUp;
end;

procedure TMMPUI.progressBarMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TMMPUI.progressBarMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
// calculate a new video position based on where the progress bar is clicked
begin
  PB.position := 75; // TEMPORARY TEST
end;

procedure TMMPUI.FormCreate(Sender: TObject);
begin
  initUI(SELF);
  createProgressBar;
end;

procedure TMMPUI.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case key = 'x' of TRUE: close; end; // TEMPORARY ESCAPE ROUTE
end;

procedure TMMPUI.WMSysCommand(var Message: TWMSysCommand);
// respond to the WM_SYSCOMMAND messages this app sends to itself
begin
  inherited;
  case Message.CmdType of MENU_ABOUT_ID:  showAboutBox; end;
end;

end.
