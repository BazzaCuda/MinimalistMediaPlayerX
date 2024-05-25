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
unit mmpFileUtils;

interface

uses
  system.classes;

function mmpConfigFilePath: string;
function mmpCopyFile(aFilePath: string; aFolder: string; deleteIt: boolean): boolean;
function mmpDeleteThisFile(const aFilePath: string; const shift: TShiftState): boolean;
function mmpExePath: string;
function mmpFileNameWithoutExtension(const aFilePath: string): string;
function mmpFileSize(const aFilePath: string): int64;
function mmpFileVersionFmt(const aFilePath: string = ''; const fmt: string = 'v%d.%d.%d.%d'): string;
function mmpIsEditFriendly(const aFilePath: string): boolean;
function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = ''): string;

implementation

uses
  winApi.windows,
  system.sysUtils, system.IOUtils,
  vcl.dialogs, vcl.forms,
  mmpFolderUtils, mmpShellUtils,
  formInputBox,
  TConfigFileClass;

function mmpConfigFilePath: string;
begin
  result := mmpExePath + 'MinimalistMediaPlayer.conf';
end;

function mmpCopyFile(aFilePath: string; aFolder: string; deleteIt: boolean): boolean;
var
  vDestFile: string;
  vDestFolder: string;
  vCancel: PBOOL;
  i: integer;
  vExt: string;
begin
  result := FALSE;
  try
    vDestFolder := ITBS(CF.value['baseFolder'] + aFolder);
    vDestFile   := vDestFolder + ExtractFileName(aFilePath);
    forceDirectories(vDestFolder);
    vCancel := PBOOL(FALSE);
    i := 0;
    case directoryExists(CF.value['baseFolder']) of TRUE: begin
      vExt := extractFileExt(aFilePath);
      while fileExists(vDestFile) do begin
        inc(i);
        vDestFile := vDestFolder + TPath.getFileNameWithoutExtension(aFilePath) + ' ' + IntToStr(i) + vExt;
      end;

      result := copyFileEx(PChar(aFilePath), PChar(vDestFile), NIL, NIL, vCancel, 0);
      case result and DeleteIt of TRUE: mmpDeleteThisFile(aFilePath, keyboardStateToShiftState); end;
    end;end;
  finally
  end;
end;

function mmpDeleteThisFile(const aFilePath: string; const shift: TShiftState): boolean;
// performs (in a separate process) the actual file/folder deletion initiated by deleteCurrentFile
begin
  case ssCtrl in Shift of  TRUE: mmpDoCommandLine('rot -nobanner -p 1 -r "' + ExtractFilePath(AFilePath) + '*.* "'); // folder contents but not subfolders
                          FALSE: mmpDoCommandLine('rot -nobanner -p 1 -r "' + AFilePath + '"'); end;                 // one individual file
end;

function mmpExePath: string;
begin
  result := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
end;

function mmpFileNameWithoutExtension(const aFilePath: string): string;
begin
  result := TPath.GetFileNameWithoutExtension(aFilePath);
end;

function mmpFileSize(const aFilePath: string): int64;
var
  vHandle:  THandle;
  vRec:     TWin32FindData;
begin
  vHandle := findFirstFile(PChar(aFilePath), vRec);
  case vHandle <> INVALID_HANDLE_VALUE of TRUE: begin
                                                  winAPI.windows.findClose(vHandle);
                                                  case (vRec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 of TRUE:
                                                    result := (int64(vRec.nFileSizeHigh) shl 32) + vRec.nFileSizeLow; end;end;end;
end;

function mmpFileVersionFmt(const aFilePath: string = ''; const fmt: string = 'v%d.%d.%d.%d'): string;
var
  vFilePath:    string;
  iBufferSize:  DWORD;
  iDummy:       DWORD;
  pBuffer:      pointer;
  pFileInfo:    pointer;
  iVer:         array[1..4] of WORD;
begin
  // set default value
  result := '';
  // get filename of exe/dll if no filename is specified
  vFilePath := aFilePath;
  case vFilePath = '' of TRUE:  begin
                                  // prepare buffer for path and terminating #0
                                  setLength(vFilePath, MAX_PATH + 1);
                                  setLength(vFilePath, getModuleFileName(hInstance, PChar(vFilePath), MAX_PATH + 1));
                                end;end;

  // get size of version info (0 if no version info exists)
  iBufferSize := getFileVersionInfoSize(PChar(vFilePath), iDummy);

  case iBufferSize > 0 of TRUE:   begin
                                    getMem(pBuffer, iBufferSize);
                                    try
                                      // get fixed file info (language independent)
                                      getFileVersionInfo(PChar(vFilePath), 0, iBufferSize, pBuffer);
                                      verQueryValue(pBuffer, '\', pFileInfo, iDummy);
                                      // read version blocks
                                      iVer[1] := hiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[2] := loWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionMS);
                                      iVer[3] := hiWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                      iVer[4] := loWord(PVSFixedFileInfo(pFileInfo)^.dwFileVersionLS);
                                    finally
                                      freeMem(pBuffer);
                                    end;
                                    // format result string
                                    result := format(fmt, [iVer[1], iVer[2], iVer[3], iVer[4]]);
                                  end;end;
end;


function mmpIsEditFriendly(const aFilePath: string): boolean;
begin
  result := NOT aFilePath.contains('''') AND NOT aFilePath.contains('&');
end;

function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = ''): string;
// the user gets to edit the filename part without the path and the extension
var
  vOldFileNamePart: string;
  vExt:             string;
  s:                string;
  vNewFilePath:     string;
begin
  result := aFilePath; // indicates failure
  try
    vOldFileNamePart  := extractFileName(aFilePath);
    vExt              := extractFileExt(vOldFileNamePart);
    vOldFileNamePart  := mmpFileNameWithoutExtension(vOldFileNamePart);

    case aNewFileNamePart <> '' of  TRUE: s := aNewFileNamePart;
                                   FALSE: begin
                                            try
                                              s := mmpInputBoxForm(vOldFileNamePart); // the form returns the edited filename or the original if the user pressed cancel
                                            finally
                                            end;end;end;
  except
    s := '';   // any funny business, force the rename to be abandoned
  end;
  case (s = '') OR (s = vOldFileNamePart) of TRUE: EXIT; end; // nothing to do

  vNewFilePath := extractFilePath(aFilePath) + s + vExt;  // construct the full path and new filename with the original extension
  case system.sysUtils.renameFile(aFilePath, vNewFilePath) of  TRUE: result := vNewFilePath;
                                                              FALSE: ShowMessage('Rename failed:' + #13#10 +  SysErrorMessage(getlasterror)); end;
end;


end.
