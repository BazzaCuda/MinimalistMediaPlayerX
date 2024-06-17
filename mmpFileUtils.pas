{   MMP: Minimalist Media Player
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

function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpConfigFilePath: string;
function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const deleteIt: boolean = FALSE; const aRecordUndo: boolean = TRUE): boolean;
function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpExePath: string;
function mmpFileNameWithoutExtension(const aFilePath: string): string;
function mmpFileSize(const aFilePath: string): int64;
function mmpFileVersionFmt(const aFilePath: string = ''; const fmt: string = 'v%d.%d.%d.%d'): string;
function mmpIsEditFriendly(const aFilePath: string): boolean;
function mmpIsFileInUse(const aFilePath: string; var aSysErrorMessage: string): boolean;
function mmpKeepDelete(const aFolderPath: string): boolean;
function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = ''): string;

implementation

uses
  winApi.windows,
  system.sysUtils, system.IOUtils,
  vcl.dialogs, vcl.forms,
  mmpConsts, mmpDialogs, mmpShellUtils, mmpSingletons, mmpUserFolders, mmpUtils,
  formInputBox,
  _debugWindow;

function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
begin
  result := FALSE;

  var vExt := lowerCase(extractFileExt(aFilePath));
  var vMT  := MT.mediaType(vExt);

  case ssCtrl in aShiftState of TRUE:   case CF.asBoolean['folderDelete'] of FALSE: EXIT; end;end;
  case ssCtrl in aShiftState of FALSE:  case vMT of
                                          mtAudio: case CF.asBoolean['audioDelete'] of FALSE: EXIT; end;
                                          mtImage: case CF.asBoolean['imageDelete'] of FALSE: EXIT; end;
                                          mtVideo: case CF.asBoolean['videoDelete'] of FALSE: EXIT; end;end;end;
  result := TRUE;
end;

function mmpConfigFilePath: string;
begin
  result := mmpExePath + 'MinimalistMediaPlayer.conf';
end;

function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const deleteIt: boolean = FALSE; const aRecordUndo: boolean = TRUE): boolean;
var
  vDestFile: string;
  vDestFolder: string;
  vCancel: PBOOL;
  i: integer;
  vExt: string;
begin
  result := FALSE;
  try
    vDestFolder := aDstFolder;
    vDestFile   := vDestFolder + ExtractFileName(aFilePath);
    try
      forceDirectories(vDestFolder);
    except end;                        // this will get picked up by the failed directoryExists below
    vCancel := PBOOL(FALSE);
    i := 0;
    case directoryExists(vDestFolder) of TRUE: begin
      vExt := extractFileExt(aFilePath);
      while fileExists(vDestFile) do begin
        inc(i);
        vDestFile := vDestFolder + TPath.getFileNameWithoutExtension(aFilePath) + ' ' + IntToStr(i) + vExt;
      end;

      result := copyFileEx(PChar(aFilePath), PChar(vDestFile), NIL, NIL, vCancel, 0);
      case result and DeleteIt of TRUE: begin
                                          mmpDeleteThisFile(aFilePath, []);
                                          case aRecordUndo of TRUE: UM.recordUndo(aFilePath, vDestFile); end;
                                        end;end;
    end;end;
  finally
  end;
end;

function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState): boolean;
// performs (in a separate process) the actual file/folder deletion initiated by deleteCurrentFile
var vSysMessage: string;
begin
  result := FALSE;

  case mmpIsFileInUse(aFilePath, vSysMessage) of TRUE:  begin
                                                          mmpShowOkCancelMsgDlg(aFilePath + #13#10#13#10 +
                                                                                'This file won''t be deleted'#13#10#13#10 +
                                                                                vSysMessage, TMsgDlgType.mtWarning, [mbOK]);
                                                          EXIT; end;end;

  case mmpCanDeleteThis(aFilePath, aShiftState) of FALSE: begin
                                                            mmpShowOKCancelMsgDlg('MinimalistMediaPlayer.conf settings prevented this deletion operation', mtInformation, [mbOK]);
                                                            EXIT; end;end;

  case ssCtrl in aShiftState of  TRUE: mmpDoCommandLine('rot -nobanner -p 1 -r "' + ExtractFilePath(AFilePath) + '*.* "'); // folder contents but not subfolders
                                FALSE: mmpDoCommandLine('rot -nobanner -p 1 -r "' + AFilePath + '"'); end;                 // one individual file

  result := TRUE;
end;

function mmpExePath: string;
begin
  result := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
end;

function mmpFileNameWithoutExtension(const aFilePath: string): string;
begin
  result := TPath.getFileNameWithoutExtension(aFilePath);
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
                                                              FALSE: showMessage('Rename failed:' + #13#10 +  SysErrorMessage(getlasterror)); end;
end;

function mmpIsFileInUse(const aFilePath: string; var aSysErrorMessage: string): boolean;
begin
  result := FALSE;
  aSysErrorMessage := '';
  setLastError(ERROR_SUCCESS);

  var hFile := createFile(PWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  result := hFile = INVALID_HANDLE_VALUE;
  case result of  TRUE: aSysErrorMessage := sysErrorMessage(getLastError);
                 FALSE: closeHandle(hFile); end;
end;

function mmpKeepDelete(const aFolderPath: string): boolean;
const
  faFile  = faAnyFile - faDirectory - faHidden - faSysFile;
var
  vSR: TSearchRec;
  vExt: string;

  function fileOK: boolean;
  begin
    result := vSR.name[1] <> '_';
  end;

begin
  result := FALSE;
  case directoryExists(aFolderPath) of FALSE: EXIT; end;
  case CF.asBoolean['keepDelete'] of FALSE: begin
                                              var vMsg := 'keepDelete=no'#13#10#13#10;
                                              vMsg := vMsg + 'To use this functionality, you must explicitly'#13#10;
                                              vMsg := vMsg + 'enable it in MinimalistMediaPlayer.conf with keepDelete=yes';
                                              mmpShowOKCancelMsgDlg(vMsg, TMsgDlgType.mtInformation, [mbOK]);
                                              EXIT; end;end;

  var vMsg := 'KEEP/DELETE '#13#10#13#10'Folder: ' + aFolderPath + '*.*';
  vMsg := vMsg + #13#10#13#10'WARNING: This will delete every file in the folder'#13#10;
  vMsg := vMsg + 'that doesn''t start with an underscore character _'#13#10#13#10;
  vMsg := vMsg + 'Only click OK if you are ABSOLUTELY SURE'#13#10#13#10;
  vMsg := vMsg + 'Once they''re gone, they are GONE!';
  case mmpShowOkCancelMsgDlg(vMsg) = IDOK of  TRUE:;
                                             FALSE: EXIT; end;

  case FindFirst(aFolderPath + '*.*', faFile, vSR) = 0 of  TRUE:
    repeat
      case fileOK of TRUE: mmpDoCommandLine('rot -nobanner -p 1 -r "' + aFolderPath + vSR.Name +  '"'); end;
    until FindNext(vSR) <> 0;
  end;

  system.sysUtils.FindClose(vSR);
  result := TRUE;
end;

end.
