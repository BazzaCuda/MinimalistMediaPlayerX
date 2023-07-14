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
unit sysCommands;

interface

uses winAPI.messages, consts, formAbout, utils, winAPI.windows, formHelp, globalVars, mediaPlayer;

function doSysCommand(var Message: TWMSysCommand): boolean;
function sendSysCommandClose(aHWND: HWND): boolean;

implementation

function doSysCommand(var Message: TWMSysCommand): boolean;
begin
  case Message.CmdType of MENU_ABOUT_ID:  showAboutBox(getFileVersionFmt('', '%d.%d'), getFileVersionFmt); end;
  case Message.CmdType of MENU_HELP_ID:   showHelp(GV.MainTopRightPt); end;
end;

function sendSysCommandClose(aHWND: HWND): boolean;
begin
  MP.release;
  sendMessage(aHWND, WM_CLOSE, 0, 0);
end;

end.
