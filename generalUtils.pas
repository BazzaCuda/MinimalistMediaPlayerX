{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit generalUtils;

interface

uses
  vcl.forms, vcl.stdCtrls, system.classes, winApi.windows, playlist, vcl.dialogs, vcl.controls;


implementation

uses
  system.sysUtils, vcl.graphics, winApi.shellApi, formInputBox, globalVars, consts, winApi.messages, uiCtrls, _debugWindow;

function doCommandLine(aCommandLIne: string): boolean;
begin
// Create a cmd.exe process to execute any command line
// "Current Directory" defaults to the folder containing this application's executable.
var
  vStartInfo:  TStartupInfo;
  vProcInfo:   TProcessInformation;
begin
  result := FALSE;
  case trim(aCommandLIne) = ''  of TRUE: EXIT; end;

  FillChar(vStartInfo,  SizeOf(TStartupInfo), #0);
  FillChar(vProcInfo,   SizeOf(TProcessInformation), #0);
  vStartInfo.cb          := SizeOf(TStartupInfo);
  vStartInfo.wShowWindow := SW_HIDE;
  vStartInfo.dwFlags     := STARTF_USESHOWWINDOW;

  var vCmd := 'c:\windows\system32\cmd.exe';
  var vParams := '/c ' + aCommandLIne;

  result := CreateProcess(PWideChar(vCmd), PWideChar(vParams), nil, nil, FALSE,
                          CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS, nil, PWideChar(getExePath), vStartInfo, vProcInfo);
end;


function deleteThisFile(aFilePath: string; shift: TShiftState): string;
begin
// performs (in a separate process) the actual file/folder deletion initiated by deleteCurrentFile
begin
  case ssCtrl in Shift of  TRUE: doCommandLine('rot -nobanner -p 1 -r "' + ExtractFilePath(AFilePath) + '*.* "'); // folder contents but not subfolders
                          FALSE: doCommandLine('rot -nobanner -p 1 -r "' + AFilePath + '"'); end;                 // one individual file
end;

end.
