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
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpConfigFilePath: string;
function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const bDeleteIt: boolean = FALSE; const bRecordUndo: boolean = TRUE): boolean;
function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState; const bSilentDelete: boolean = FALSE; const bRunTasks: boolean = TRUE): boolean;
function mmpExePath: string;
function mmpFileNameWithoutExtension(const aFilePath: string): string;
function mmpFileSize(const aFilePath: string): int64;
function mmpFileVersionFmt(const aFilePath: string = ''; const fmt: string = 'v%d.%d.%d.%d'): string;
function mmpIsEditFriendly(const aFilePath: string): boolean;
function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
function mmpKeepDelete(const aFolderPath: string): boolean;
function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = ''): string;
function mmpRunTasks: boolean;

implementation

uses
  winApi.windows,
  system.ioUtils,
  vcl.dialogs,
  mmpConsts, mmpDialogs, mmpFolderUtils, mmpFormInputBox, mmpShellUtils, mmpShredUtils, mmpUtils,
  model.mmpConfigFile, model.mmpMediaTypes, model.mmpUndoMove;


function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
begin
  result    := FALSE;

  var vMT   := MT.mediaType(aFilePath);

  case ssCtrl in aShiftState of  TRUE:  case CF.asBoolean[CONF_FOLDER_DELETE] of FALSE: EXIT; end;end;
  case ssCtrl in aShiftState of FALSE:  case vMT of
                                          mtAudio: case CF.asBoolean[CONF_AUDIO_DELETE] of FALSE: EXIT; end;
                                          mtImage: case CF.asBoolean[CONF_IMAGE_DELETE] of FALSE: EXIT; end;
                                          mtVideo: case CF.asBoolean[CONF_VIDEO_DELETE] of FALSE: EXIT; end;end;end;
  result    := TRUE;
end;

function mmpConfigFilePath: string;
begin
  result := mmpExePath + 'MinimalistMediaPlayer.conf';
end;

function mmpConfirmDelete(const aFilePath: string; const aShiftState: TShiftState): boolean;
begin
  result := FALSE;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(aFilePath);
  case ssCtrl in aShiftState of  TRUE: vMsg := 'DELETE folder contents '#13#10#13#10'Folder: ' + extractFilePath(aFilePath) + '*.*';
                                  FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(aFilePath); end;

  case CF.asDeleteMethod[CONF_DELETE_METHOD] in [dmStandard, dmShred] of TRUE: vMsg := vMsg + #13#10#13#10'Only click OK if you are ABSOLUTELY SURE'; end;
  case CF.asDeleteMethod[CONF_DELETE_METHOD] of dmShred: vMsg := vMsg + #13#10'Shred: Once they''re gone, they are GONE!'; end;

  case ssCtrl in aShiftState of TRUE: vMsg := vMsg + #13#10#13#10'(doesn''t affect the contents of subfolders)'; end;

  result := mmpShowOkCancelMsgDlg(vMsg) = IDOK;
end;

function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const bDeleteIt: boolean = FALSE; const bRecordUndo: boolean = TRUE): boolean;
var
  vDestFile:    string;
  vDestFolder:  string;
  vCancel:      PBOOL;
  i:            integer;
  vExt:         string;
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
        vDestFile := vDestFolder + TPath.getFileNameWithoutExtension(aFilePath) + ' ' + intToStr(i) + vExt;
      end;

      result := copyFileEx(PChar(aFilePath), PChar(vDestFile), NIL, NIL, vCancel, 0);

      case result and bDeleteIt of TRUE: begin
                                          mmpDeleteThisFile(aFilePath, [], bDeleteIt);
                                          case bRecordUndo of TRUE: UM.recordUndo(aFilePath, vDestFile); end;
                                        end;end;
    end;end;
  finally
  end;
end;

function mmpExePath: string;
begin
  result := mmpITBS(extractFilePath(paramStr(0)));
end;

function mmpFileNameWithoutExtension(const aFilePath: string): string;
begin
  result := TPath.getFileNameWithoutExtension(aFilePath);
end;

function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState; const bSilentDelete: boolean = FALSE; const bRunTasks: boolean = TRUE): boolean;
var vSysMessage: string;
begin
  result := FALSE;

  case bSilentDelete of FALSE: begin
    case mmpCanDeleteThis(aFilePath, aShiftState) of FALSE: begin
                                                              mmpShowOKCancelMsgDlg('MinimalistMediaPlayer.conf settings prevented this deletion operation', mtInformation, [mbOK]);
                                                              EXIT; end;end;

    case mmpConfirmDelete(aFilePath, aShiftState) of FALSE: EXIT; end;
  end;end;

  case mmpIsFileInUse(aFilePath, vSysMessage) of TRUE:  begin
                                                          mmpShowOkCancelMsgDlg(aFilePath + #13#10#13#10 +
                                                                                'This file won''t be deleted or moved'#13#10#13#10 +
                                                                                vSysMessage, TMsgDlgType.mtWarning, [mbOK]);
                                                          EXIT; end;end;

  case ssCtrl in aShiftState of  TRUE: mmpShredThis(extractFilePath(aFilePath), CF.asDeleteMethod[CONF_DELETE_METHOD]); // folder contents but not subfolders
                                FALSE: mmpShredThis(aFilePath, CF.asDeleteMethod[CONF_DELETE_METHOD]); end;             // one individual file

  case bRunTasks of  TRUE: result := mmpRunTasks;
                    FALSE: result := TRUE; end;
end;

function mmpFileSize(const aFilePath: string): int64;
var
  vHandle:  THandle;
  vRec:     TWin32FindData;
begin
  result := -1;
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
  result    := '';
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
  result := NOT aFilePath.contains('''') and NOT aFilePath.contains('&');
end;

function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
begin
  result            := FALSE;
  aSysErrorMessage  := '';
  setLastError(ERROR_SUCCESS);

  var hFile := createFile(PWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  result := hFile = INVALID_HANDLE_VALUE;
  case result of  TRUE: aSysErrorMessage := sysErrorMessage(getLastError);
                 FALSE: closeHandle(hFile); end;
end;

function mmpKeepDelete(const aFolderPath: string): boolean;
const
  faFilesOnly = faAnyFile AND NOT faDirectory AND NOT faHidden AND NOT faSysFile;
var
  vSR: TSearchRec;

  function fileOK: boolean;
  begin
    result := vSR.name[1] <> '!';
  end;

begin
  result := FALSE;
  case directoryExists(aFolderPath) of FALSE: EXIT; end;

  case CF.asBoolean[CONF_KEEP_DELETE] of FALSE: begin
                                                  var vMsg := 'keepDelete=no'#13#10#13#10;
                                                      vMsg := vMsg + 'To use this functionality, you must explicitly enable it'#13#10;
                                                      vMsg := vMsg + 'in MinimalistMediaPlayer.conf with keepDelete=yes';
                                                  mmpShowOKCancelMsgDlg(vMsg, TMsgDlgType.mtInformation, [mbOK]);
                                                  EXIT; end;end;

  var vMsg := 'KEEP/DELETE '#13#10#13#10'Folder: ' + aFolderPath + '*.*';
      vMsg := vMsg + #13#10#13#10'WARNING: This will delete every file in the folder'#13#10;
      vMsg := vMsg + 'that doesn''t start with an exclamation point !';

  case CF.asDeleteMethod[CONF_DELETE_METHOD] in [dmStandard, dmShred] of TRUE: vMsg := vMsg + #13#10#13#10'Only click OK if you are ABSOLUTELY SURE'; end;
  case CF.asDeleteMethod[CONF_DELETE_METHOD] of dmShred: vMsg := vMsg + #13#10'Shred: Once they''re gone, they are GONE!'; end;

  case mmpShowOkCancelMsgDlg(vMsg) = IDOK of  TRUE:;
                                             FALSE: EXIT; end;

  case findFirst(aFolderPath + '*.*', faFilesOnly, vSR) = 0 of  TRUE:
    repeat
      case fileOK of TRUE: mmpShredThis(aFolderPath + vSR.Name, CF.asDeleteMethod[CONF_DELETE_METHOD]); end;
    until findNext(vSR) <> 0;
  end;

  system.sysUtils.findClose(vSR);
  result := TRUE;
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
                                                              FALSE: mmpShowOKCancelMsgDlg('Rename failed:' + #13#10 +  sysErrorMessage(getlasterror), mtError, [mbOK]); end;
end;

function mmpRunTasks: boolean;
begin
  result := FALSE;
  mmpStartTasks;
  result := TRUE;
end;

end.
