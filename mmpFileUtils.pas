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
unit mmpFileUtils;

interface

uses
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpCheckIfEditFriendly(const aFilePath: string): boolean;
function mmpCleanFile(const aFileName: string): string;
function mmpCompareFileTimestamps(const aFile1: string; const aFile2: string): boolean;
function mmpConfigFilePath: string;
function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const bDeleteIt: boolean = FALSE; const bRecordUndo: boolean = TRUE): boolean;
function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState; const bSilentDelete: boolean = FALSE; const bRunTasks: boolean = TRUE): boolean;
function mmpExePath: string;
function mmpFileNameWithoutExtension(const aFilePath: string): string;
function mmpFileSize(const aFilePath: string): int64;
function mmpFileVersionFmt(const aFilePath: string = ''; const fmt: string = 'v%d.%d.%d.%d'): string;
function mmpIsEditFriendly(const aFilePath: string): boolean;
function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
function mmpIsFileInUseExclusive(const aFilePath: string; out aSysErrorMessage: string): boolean;
function mmpKeepDelete(const aFolderPath: string): boolean;
function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = ''): string;
function mmpRenameMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
function mmpRunTasks: boolean;

implementation

uses
  winApi.windows,
  system.ioUtils,
  vcl.controls, vcl.dialogs,
  mmpConsts, mmpDialogs, mmpFolderUtils, mmpFormInputBox, mmpFuncProg, mmpShellUtils, mmpShredUtils, mmpUtils,
  view.mmpFormConfirmDelete,
  model.mmpConfigFile, model.mmpMediaTypes, model.mmpUndoMove,
  _debugWindow;


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

function mmpCheckIfEditFriendly(const aFilePath: string): boolean;
var F: TProc;
begin
  F := procedure  begin
                    mmp.cmd(evMPPause);
                    mmpShowOKCancelMsgDlg(aFilePath    + #13#10#13#10
                                                       + 'The <path>\<filename> contains at least one single quote '' '#13#10
                                                       + 'A single quote will cause the Export and Join command line operations to fail.'#13#10#13#10
                                                       + 'Rename the path or filename first to remove the dirty characters.'#13#10#13#10
                                                       + 'Ctrl-Shift-[R] will cleanup the file name (but not the path) for you '
                                                       + 'by replacing each dirty char with a space.', mtInformation, [MBOK]); end;

  result := mmpIsEditFriendly(aFilePath);
  mmp.cmd(result, NIL, F);
end;

function mmpConfigFilePath: string;
begin
  result := mmpExePath + 'MinimalistMediaPlayer.conf';
end;

function mmpCleanFile(const aFileName: string): string;
begin
  var vDirtyChars := mmpIfThenElse(trim(CF[CONF_DIRTY_CHARS]) <> '', DIRTY_CHARS + trim(CF[CONF_DIRTY_CHARS]), DIRTY_CHARS);
  result := aFileName;
  for var i := 1 to length(result) do
    case vDirtyChars.contains(result[i]) of TRUE: result[i] := ' '; end;
end;

function mmpCompareFileTimestamps(const aFile1: string; const aFile2: string): boolean;
begin
  result := FALSE;

  case NOT TFile.exists(aFile1) of TRUE: EXIT; end;
  case NOT TFile.exists(aFile2) of TRUE: EXIT; end;

  var vTimestamp1: TDateTime := TFile.getLastWriteTime(aFile1);
  var vTimestamp2: TDateTime := TFile.getLastWriteTime(aFile2);

  result := vTimestamp1 < vTimestamp2;
end;

function mmpConfirmDelete(const aFilePath: string; const aShiftState: TShiftState): boolean;
var vDeletionObject: TDeletionObject;
begin
  result := FALSE;

  case ssCtrl in aShiftState of  TRUE: vDeletionObject := doFolder;
                                FALSE: vDeletionObject := doFile; end;

  result := mmpShowConfirmDelete(aFilePath, vDeletionObject, CF.asDeleteMethod[CONF_DELETE_METHOD], CF[CONF_DELETE_METHOD], CF.asInteger[CONF_SCALE_FACTOR]) = mrYes;
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
    vDestFile   := vDestFolder + extractFileName(aFilePath);
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
  result := FALSE;
  var vDirtyChars:string := DIRTY_CHARS;
  var vNoExt      := extractFilePath(aFilePath) + mmpFileNameWithoutExtension(aFilePath);
  for var i := 1 to length(vNoExt) do
    case vDirtyChars.contains(vNoExt[i]) of TRUE: EXIT; end;
  result := TRUE;
end;

function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
begin
  result            := FALSE;
  aSysErrorMessage  := '';
  setLastError(ERROR_SUCCESS);

  try
    var hFile := createFile(PWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ OR FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
    result := hFile = INVALID_HANDLE_VALUE;
    case result of  TRUE: aSysErrorMessage := sysErrorMessage(getLastError);
                   FALSE: closeHandle(hFile); end;
  except
    result := FALSE; // hopefully only an EFOpenError
  end;
end;

function mmpIsFileInUseExclusive(const aFilePath: string; out aSysErrorMessage: string): boolean;
var
  hFile: THandle;
  vLastError: DWORD;
begin
  aSysErrorMessage := '';
  setLastError(ERROR_SUCCESS);

  // Attempt to open the file with exclusive access (dwShareMode = 0).
  // GENERIC_READ is sufficient for just checking if a lock can be acquired.
  // OPEN_EXISTING means the file must already exist for this check to be meaningful.
  hFile := createFile(PWideChar(aFilePath), GENERIC_READ, 0, NIL, OPEN_EXISTING, 0, 0);

  // Determine if the file is "in use" based on the handle status
  result := hFile = INVALID_HANDLE_VALUE; // Initial assumption if CreateFile fails

  case result of
     TRUE:  begin // CreateFile failed, meaning hFile is INVALID_HANDLE_VALUE
              vLastError := getLastError;
              aSysErrorMessage := sysErrorMessage(vLastError);
              // Refine 'result' based on the specific error code
              case vLastError of
                ERROR_SHARING_VIOLATION, // File is locked by another process
                ERROR_ACCESS_DENIED:     // Access denied can also mean a lock (e.g., by system or AV)
                  ; // result is already TRUE, so do nothing.
                else
                  // Any other error (e.g., file not found, path invalid, access denied for other reasons)
                  // means it's NOT "in use" due to a lock, but rather some other issue.
                  result := FALSE;
              end;
              EXIT;
            end;
    FALSE:  begin // createFile succeeded, meaning hFile is a valid handle
              closeHandle(hFile); // Immediately close the handle we opened
              // result is already FALSE, as the file was NOT in use by another process.
              EXIT;
            end;
  end;
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

  case mmpShowConfirmDelete(aFolderPath, doKeepDelete, CF.asDeleteMethod[CONF_DELETE_METHOD], CF[CONF_DELETE_METHOD], CF.asInteger[CONF_SCALE_FACTOR]) = mrYES of FALSE: EXIT; end;

  case findFirst(aFolderPath + '*.*', faFilesOnly, vSR) = 0 of  TRUE:
    repeat
      case fileOK of TRUE: mmpShredThis(aFolderPath + vSR.Name, CF.asDeleteMethod[CONF_DELETE_METHOD]); end;
    until findNext(vSR) <> 0;
  end;

  system.sysUtils.findClose(vSR);
  mmpRunTasks;
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

    case aNewFileNamePart <> '' of  TRUE: s := aNewFileNamePart; // the calling code has already supplied the new name without the extension
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
                                                              FALSE: mmpShowOKCancelMsgDlg('Rename failed:' + #13#10#13#10 +  sysErrorMessage(getlasterror), mtError, [mbOK]); end;
end;

function mmpRenameMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
// leave the original .seg file so cleanUp will delete any leftover .segnn. files listed in it
begin
  result := aNewFilePath;
  var vOldMMP := changeFileExt(aOldFilePath, '.mmp');
  var vNewMMP := changeFileExt(aNewFilePath, '.mmp');
  case fileExists(vOldMMP) of TRUE: renameFile(vOldMMP, vNewMMP); end;
end;


function mmpRunTasks: boolean;
begin
  result := FALSE;
  mmpStartTasks;
  result := TRUE;
end;

end.
