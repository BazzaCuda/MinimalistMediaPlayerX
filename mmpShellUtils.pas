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
unit mmpShellUtils;

interface

uses
  mmpConsts, mmpUtils,
  model.mmpConfigFile;

function mmpEnvironmentVariable: boolean;
function mmpExecCommandLine(const aCommandLine: string): boolean;
function mmpGetExternalApp(const aFnnKeyApp: TFnnKeyApp): string;
function mmpOpenExternalApp(const aFnnKeyApp: TFnnKeyApp; const aParams: string): boolean;
function mmpShellExec(const anExePath: string; const aParams: string = ''): boolean;

implementation

uses
  winApi.shellApi, winApi.windows,
  system.sysUtils,
  mmpFileUtils;

function mmpEnvironmentVariable: boolean;
var
  vBuf: array[0..1024] of char;
begin
  getEnvironmentVariable(MMP_CHECK, vBuf, sizeOf(vBuf) - 1);
  result := getLastError <> ERROR_ENVVAR_NOT_FOUND;
end;

function mmpExecCommandLine(const aCommandLine: string): boolean;
// Create a cmd.exe process to execute any command line
// "Current Directory" defaults to the folder containing this application's executable.
var
  vStartInfo:  TStartupInfo;
  vProcInfo:   TProcessInformation;
begin
  result := FALSE;
  case trim(aCommandLIne) = ''  of TRUE: EXIT; end;

  fillChar(vStartInfo,  sizeOf(TStartupInfo), #0);
  fillChar(vProcInfo,   sizeOf(TProcessInformation), #0);
  vStartInfo.cb          := sizeOf(TStartupInfo);
  vStartInfo.wShowWindow := SW_HIDE;
  vStartInfo.dwFlags     := STARTF_USESHOWWINDOW;

  var vCmd    := 'c:\windows\system32\cmd.exe';
  var vParams := '/c ' + aCommandLIne;

  result := createProcess(pWideChar(vCmd), pWideChar(vParams), nil, nil, FALSE, CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS, nil, pWideChar(mmpExePath), vStartInfo, vProcInfo);
end;

function mmpGetExternalApp(const aFnnKeyApp: TFnnKeyApp): string;
begin
  result := CF[mmpFnnKeyAppToString(aFnnKeyApp)]; // has the user overridden the default app in the config file?

  case result = '' of TRUE: case aFnnKeyApp of // No
                                F10_APP: result := POT_PLAYER;
                                F11_APP: result := LOSSLESS_CUT;
                                F12_APP: result := SHOTCUT; end;end;
end;

function mmpOpenExternalApp(const aFnnKeyApp: TFnnKeyApp; const aParams: string): boolean;
begin
  mmpShellExec(mmpGetExternalApp(aFnnKeyApp), aParams);
end;

function mmpShellExec(const anExePath: string; const aParams: string = ''): boolean;
begin
  shellExecute(0, 'open', pChar(anExePath), pChar('"' + aParams + '"'), '', SW_SHOW);
end;

end.
