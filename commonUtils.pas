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
unit commonUtils;

interface

uses
  vcl.forms, vcl.stdCtrls, system.classes, winApi.windows, playlist, vcl.dialogs, vcl.controls;

function delay(dwMilliseconds: DWORD): boolean;
function doCommandLine(aCommandLIne: string): boolean;
function fillPlaylist(aPL: TPlaylist; aFolder: string): boolean;
function formatFileSize(aSize: int64): string;
function formatSeconds(seconds: integer): string;
function formatTime(seconds: integer): string;
function getExePath: string;
function getFileVersionFmt(const aFilePath: string = ''; const fmt: string = '%d.%d.%d.%d'): string;
function getScreenHeight: integer;
function getScreenWidth: integer;
function initTransparentForm(aForm: TForm): TForm;
function initTransparentLabel(aLabel: TLabel): boolean;
function shellExec(anExePath, aParams: string): boolean;
function showOKCancelMsgDlg(aMsg: string;
                                msgDlgType: TMsgDlgType = mtConfirmation;
                                msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                                defButton: TMsgDlgBtn = MBCANCEL): TModalResult;


implementation

uses
  system.sysUtils, vcl.graphics, winApi.shellApi, _debugWindow;

function delay(dwMilliseconds: DWORD): boolean;
// Used to delay an operation; "sleep()" would suspend the thread, which is not what is required
var
  iStart, iStop: DWORD;
begin
  iStart := GetTickCount;
  repeat
    iStop  := GetTickCount;
    Application.ProcessMessages;
  until (iStop  -  iStart) >= dwMilliseconds;
end;


function doCommandLine(aCommandLIne: string): boolean;
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

function fillPlaylist(aPL: TPlaylist; aFolder: string): boolean;
const
  faFile  = faAnyFile - faDirectory - faHidden - faSysFile;
var
  vSR: TSearchRec;
begin
  result := FALSE;
  aPL.clear;
  case directoryExists(aFolder) of FALSE: EXIT; end;

  case FindFirst(aFolder + '*.*', faFile, vSR) = 0 of  TRUE:
    repeat
      case (vSR.attr AND faDirectory) = faDirectory of FALSE: aPL.Add(aFolder + vSR.Name); end;
    until FindNext(vSR) <> 0;
  end;

  FindClose(vSR);
  result := TRUE;
end;

function formatFileSize(aSize: int64): string;
begin
 case aSize >= 1052266987 of  TRUE:   try result := format('FS:  %.3f GB', [aSize / 1024 / 1024 / 1024]); except end;  // >= 0.98 of 1GB
                             FALSE:   try result := format('FS:  %d MB', [trunc(aSize / 1024 / 1024)]); except end;end;
end;

function formatSeconds(seconds: integer): string;
begin
  case seconds < 60 of  TRUE: result := format('%ds', [seconds]);
                       FALSE: result := format('%d:%.2d', [seconds div 60, seconds mod 60]);
  end;
end;

function formatTime(seconds: integer): string;
begin
  case seconds < 60 of  TRUE: result := format('%.2d:%.2d', [0, seconds]);
                       FALSE: case seconds < 3600 of  TRUE: result := format('%.2d:%.2d', [seconds div 60, seconds mod 60]);
                                                     FALSE: result := format('%.2d:%.2d:%.2d', [seconds div 3600, (seconds mod 3600) div 60, seconds mod 3600 mod 60]); end;end;
end;

function getExePath: string;
begin
  result := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
end;

function getFileVersionFmt(const aFilePath: string = ''; const fmt: string = '%d.%d.%d.%d'): string;
var
  vFilePath: string;
  iBufferSize: DWORD;
  iDummy: DWORD;
  pBuffer: Pointer;
  pFileInfo: Pointer;
  iVer: array[1..4] of Word;
begin
  // set default value
  Result := '';
  // get filename of exe/dll if no filename is specified
  vFilePath := aFilePath;
  case vFilePath = '' of TRUE:  begin
                                  // prepare buffer for path and terminating #0
                                  SetLength(vFilePath, MAX_PATH + 1);
                                  SetLength(vFilePath, GetModuleFileName(hInstance, PChar(vFilePath), MAX_PATH + 1));
                                end;end;

  // get size of version info (0 if no version info exists)
  iBufferSize := GetFileVersionInfoSize(PChar(vFilePath), iDummy);

  case iBufferSize > 0 of TRUE:   begin
                                    GetMem(pBuffer, iBufferSize);
                                    try
                                      // get fixed file info (language independent)
                                      GetFileVersionInfo(PChar(vFilePath), 0, iBufferSize, pBuffer);
                                      VerQueryValue(pBuffer, '\', pFileInfo, iDummy);
                                      // read version blocks
                                      iVer[1] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[2] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[3] := HiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                      iVer[4] := LoWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                    finally
                                      FreeMem(pBuffer);
                                    end;
                                    // format result string
                                    Result := Format(Fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
                                  end;end;
end;

function getScreenHeight: integer;
begin
  var rect := screen.WorkAreaRect; // the screen minus the taskbar
  result := rect.Bottom - rect.Top;
end;

function getScreenWidth: integer;
begin
  result := GetSystemMetrics(SM_CXVIRTUALSCREEN); // we'll assume that the taskbar is in it's usual place at the bottom of the screen
end;

function initTransparentForm(aForm: TForm): TForm;
begin
  aForm.align                  := alBottom;
//  aForm.height                 := 200;
  aForm.styleElements          := []; // don't allow any theme alterations
  aForm.borderStyle            := bsNone;
  aForm.color                  := clBlack;
  aForm.ctl3D                  := FALSE;
  aForm.doubleBuffered         := TRUE;
  aForm.margins.bottom         := 0;
//  aForm.oldCreateOrder         := TRUE; // Delphi11 doesn't like this
  aForm.formStyle              := fsStayOnTop; // Keep the form always on top - hmmm. How does this impact infoPanel?
  aForm.borderIcons            := [];
  aForm.alphaBlend             := True;
  aForm.alphaBlendValue        := 255;
  aForm.transparentColorValue  := clBlack;
  aForm.transparentColor       := TRUE;
  result := aForm;
end;

function initTransparentLabel(aLabel: TLabel): boolean;
begin
  aLabel.align             := alClient;
  aLabel.alignment         := taCenter;
  aLabel.alignWithMargins  := TRUE;
  aLabel.color             := clBlack;
  aLabel.font.color        := clGray;
  aLabel.font.size         := 14;
  aLabel.font.style        := [fsBold];
  aLabel.layout            := tlBottom;
  aLabel.margins.Bottom    := 6;
  aLabel.parentColor       := FALSE;
  aLabel.parentCustomHint  := FALSE;
  aLabel.parentFont        := FALSE;
  aLabel.ParentShowHint    := FALSE;
  aLabel.showAccelChar     := FALSE;
  aLabel.showHint          := FALSE;
  aLabel.transparent       := TRUE;
  aLabel.wordWrap          := TRUE;
end;

function shellExec(anExePath, aParams: string): boolean;
begin
  shellExecute(0, 'open', pchar(anExePath), pchar('"' + aParams + '"'), '', SW_SHOW);
end;

function showOKCancelMsgDlg(aMsg: string;
                                msgDlgType: TMsgDlgType = mtConfirmation;
                                msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                                defButton: TMsgDlgBtn = MBCANCEL): TModalResult;
// used for displaying the delete file/folder confirmation dialog
// We modify the standard dialog to make everything bigger, especially the width so that long folder names and files display properly
// The standard dialog would unhelpfully truncate them.
begin
  with CreateMessageDialog(aMsg, msgDlgType, msgDlgButtons, defButton) do
  try
    font.name := 'Segoe UI';
    font.size := 12;
    height    := height + 50;
    width     := width + 200;
    for var i := 0 to controlCount - 1 do begin
      case controls[i] is TLabel  of   TRUE: with Controls[i] as TLabel do Width := Width + 200; end;
      case controls[i] is TButton of   TRUE: with Controls[i] as TButton do begin
                                                                                top  := top + 60;
                                                                                left := left + 100;
                                                                            end;end;
    end;
    result := ShowModal;
  finally
    Free;
  end;
end;







end.
