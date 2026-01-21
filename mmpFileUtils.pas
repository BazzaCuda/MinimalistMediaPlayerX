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
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

function mmpCanDeleteThis(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpChapterContainer(const aFolderPath: string; const aMediaType: TMediaType): string;
function mmpChapterFile(const aFolderPath: string): string;
function mmpCheckIfEditFriendly(const aFilePath: string): boolean;
function mmpCheckRecycleBin(const aFilePath: string): boolean;
function mmpCleanFile(const aFileName: string): string;
function mmpCompareFileTimestamps(const aFile1: string; const aFile2: string): boolean;
function mmpConfigFilePath: string;
function mmpCopyFile(const aFilePath: string; const aDstFolder: string; const bDeleteIt: boolean = FALSE; const bRecordUndo: boolean = TRUE): boolean;
function mmpCopyMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState; const bSilentDelete: boolean = FALSE; const bRunTasks: boolean = TRUE; const bStopMPV: boolean = TRUE): boolean;
function mmpExePath: string;
function mmpFileNameWithoutExtension(const aFilePath: string): string;
function mmpFileSize(const aFilePath: string): int64;
function mmpFileVersionFmt(const aFilePath: string = EMPTY; const fmt: string = 'v%d.%d.%d.%d'): string;
function mmpFixedDrive(const aFilePath: string): boolean;
function mmpFixedDriveRecycles(const aFilePath: string): boolean;
function mmpIsEditFriendly(const aFilePath: string): boolean;
function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
function mmpIsFileInUseExclusive(const aFilePath: string; out aSysErrorMessage: string): boolean;
function mmpKeepDelete(const aFolderPath: string): boolean;
function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = EMPTY): string;
function mmpRenameMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
function mmpRunTasks: boolean;

implementation

uses
  winApi.windows,
  system.ioUtils, system.win.registry,
  vcl.controls, vcl.dialogs,
  bazCmd,
  mmpDialogs, mmpFolderUtils, mmpFormInputBox, mmpGlobalState, mmpShellUtils, mmpShredUtils, mmpUtils,
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


function mmpChapterContainer(const aFolderPath: string; const aMediaType: TMediaType): string;
begin
  result := aFolderPath;
  case GS.mediaType of
    mtAudio: result := changeFileExt(aFolderPath, '.mkv');
    mtVideo: result := changeFileExt(aFolderPath, '.mkv'); end;
end;

function mmpChapterFile(const aFolderPath: string): string;
begin
  result := changeFileExt(aFolderPath, '.chp');
end;

function mmpCheckIfEditFriendly(const aFilePath: string): boolean;
var F: TProc;
begin
  F := procedure  begin
                    mmpDelay(500); // when launching MMP with startInEditor, allow the video to start playing
                    var vWasPlaying := (GS.mediaType in [mtAudio, mtVideo]) and mmp.cmd(evMPReqPlaying).tf;
                    mmp.cmd(vWasPlaying, evMPPause);

                    mmpShowOKCancelMsgDlg(aFilePath    + #13#10#13#10
                                                       + 'The <path>\<filename> contains at least one single quote '' '#13#10
                                                       + 'A single quote will cause the Export and Join command line operations to fail.'#13#10#13#10
                                                       + 'Rename the path or filename first to remove the dirty characters.'#13#10#13#10
                                                       + 'Ctrl-Shift-[R] will cleanup the file name (but not the path) for you '
                                                       + 'by replacing each dirty char with a space.', 'Audio & Video Timeline Editor');

                   mmp.cmd(vWasPlaying, evMPResume); end;

  result := mmpIsEditFriendly(aFilePath);
  mmp.cmd(result, NIL, F);
end;

function mmpCheckRecycleBin(const aFilePath: string): boolean;
begin
  result := TRUE;
  case (CF.asDeleteMethod[CONF_DELETE_METHOD] = dmRecycle) of FALSE: EXIT; end;

  var vMsg:string := EMPTY;

  case mmpFixedDrive(aFilePath)         of FALSE: vMsg := 'Windows has this as a REMOVABLE drive and won''t use the Recycle Bin'#13#10#13#10; end;
  case mmpFixedDriveRecycles(aFilePath) of FALSE: vMsg := 'This FIXED drive is set to not use the Recycle Bin'#13#10#13#10; end;

  case vMsg = EMPTY of FALSE: result := mmpUserOK(aFilePath + #13#10#13#10 +
                                                  vMsg +
                                                  'If you continue, Windows will simply delete the file(s)'#13#10#13#10 +
                                                  'Do you want to continue?'); end;
end;

function mmpConfigFilePath: string;
begin
  result := mmpExePath + 'MinimalistMediaPlayer.conf';
end;

function mmpCleanFile(const aFileName: string): string;
begin
  var vDirtyChars := mmpIfThenElse(trim(CF[CONF_DIRTY_CHARS]) <> EMPTY, DIRTY_CHARS + trim(CF[CONF_DIRTY_CHARS]), DIRTY_CHARS);
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

function mmpConfirmDelete(const aFilePath: string; const aShiftState: TShiftState; const aDeleteMethod: TDeleteMethod): boolean;
var vDeletionObject: TDeletionObject;
begin
  result := FALSE;

  case ssCtrl in aShiftState of  TRUE: vDeletionObject := doFolder;
                                FALSE: vDeletionObject := doFile; end;

  result := mmpShowConfirmDelete(aFilePath, vDeletionObject, aDeleteMethod, CF[CONF_DELETE_METHOD], CF.asInteger[CONF_SCALE_FACTOR], ssShift in aShiftState) = mrYes;
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
                                          mmpDeleteThisFile(aFilePath, [], bDeleteIt); // defaults to mpvStop as it's the current file
                                          case bRecordUndo of TRUE: UM.recordUndo(aFilePath, vDestFile); end;
                                        end;end;
    end;end;
  finally
  end;
end;

function mmpCopyMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
// leave the original .seg file so cleanUp will delete any leftover .segnn. files listed in it
begin
  result := aNewFilePath;
  var vOldMMP := changeFileExt(aOldFilePath, '.mmp');
  var vNewMMP := changeFileExt(aNewFilePath, '.mmp');
  case fileExists(vOldMMP) of TRUE: TFile.copy(vOldMMP, vNewMMP, TRUE); end;

  var vOldKey := changeFileExt(aOldFilePath, '.key');
  var vNewKey := changeFileExt(aNewFilePath, '.key');
  case fileExists(vOldKey) of TRUE: TFile.copy(vOldKey, vNewKey, TRUE); end;
end;

function mmpExePath: string;
begin
  result := mmpITBS(extractFilePath(paramStr(0)));
end;

function mmpFileNameWithoutExtension(const aFilePath: string): string;
begin
  result := TPath.getFileNameWithoutExtension(aFilePath);
end;

function mmpDeleteThisFile(const aFilePath: string; const aShiftState: TShiftState; const bSilentDelete: boolean = FALSE; const bRunTasks: boolean = TRUE; const bStopMPV: boolean = TRUE): boolean;
var vSysMessage: string;
begin
  // {$if BazDebugWindow} debugString('mmpDeleteThisFile', aFilePath); {$endif}
  result := FALSE;

  var vDeleteMethod := CF.asDeleteMethod[CONF_DELETE_METHOD];
  case (NOT mmpFixedDrive(aFilePath)) or (NOT mmpFixedDriveRecycles(aFilePath)) of TRUE: vDeleteMethod := dmStandard; end;
  // TDebug.debugEnum<TDeleteMethod>('vDeleteMethod', vDeleteMethod);

  case bSilentDelete of FALSE: begin
    case mmpCanDeleteThis(aFilePath, aShiftState) of FALSE: begin
                                                              mmpShowOKCancelMsgDlg('MinimalistMediaPlayer.conf settings prevented this deletion operation');
                                                              EXIT; end;end;

    case mmpConfirmDelete(aFilePath, aShiftState, vDeleteMethod) of FALSE: EXIT; end;
  end;end;

  case mmpIsFileInUse(aFilePath, vSysMessage) of TRUE:  begin
                                                          mmpShowOkCancelMsgDlg(aFilePath + #13#10#13#10 +
                                                                                'This file won''t be deleted or moved'#13#10#13#10 +
                                                                                vSysMessage, MMP_TITLE, TMsgDlgType.mtWarning, [mbOK]);
                                                          EXIT; end;end;

  case bStopMPV of TRUE:  begin
                            // {$if BazDebugWindow} debug('mmpDeleteThisFile evMPStop'); {$endif}
                            mmp.cmd(evGSSuspended, TRUE); // prevent evMPStop from triggering next media file in TVM.onMPNotify
                            mmp.cmd(evMPStop); end;end;

  try

    case ssCtrl in aShiftState of  TRUE: mmpShredThis(extractFilePath(aFilePath), vDeleteMethod); // folder contents but not subfolders
                                  FALSE: mmpShredThis(aFilePath, vDeleteMethod); end;             // one individual file

    case bRunTasks of  TRUE: result := mmpRunTasks;
                      FALSE: result := TRUE; end; // mmpRunTasks can be delayed and then called manually after a series of calls to mmpDeleteThisFile (see TCleanupClass)

  finally
    mmp.cmd(evGSSuspended, FALSE); // this won't have any effect until evVMMPPlayNext etc
  end;
  // {$if BazDebugWindow} debug('exit mmpDeleteThisFile'); {$endif}
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

function mmpFileVersionFmt(const aFilePath: string = EMPTY; const fmt: string = 'v%d.%d.%d.%d'): string;
var
  vFilePath:    string;
  iBufferSize:  DWORD;
  iDummy:       DWORD;
  pBuffer:      pointer;
  pFileInfo:    pointer;
  iVer:         array[1..4] of WORD;
begin
  // set default value
  result    := EMPTY;
  // get filename of exe/dll if no filename is specified
  vFilePath := aFilePath;
  case vFilePath = EMPTY of TRUE: begin
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

function mmpFixedDrive(const aFilePath: string): boolean;
begin
  result := FALSE;

  var vDriveRoot := extractFileDrive(aFilePath);

  result := getDriveType(PChar(vDriveRoot)) = DRIVE_FIXED; // large external USB SSDs present as fixed; micro SDs etc don't
end;

function mmpFixedDriveRecycles(const aFilePath: string): boolean;
// only fixed drives will have a registry volume key
// assume TRUE unless NukeOnDelete = 1, meaning the user has turned off recycling for this fixed drive
const NUKE_ON_DELETE = 'NukeOnDelete';
var vVolumeFull: array[0..MAX_PATH] of char;
begin
  result := TRUE; // this drive uses the Recycle Bin...

  var vDriveRoot := mmpITBS(extractFileDrive(aFilePath));

  case getVolumeNameForVolumeMountPoint(pchar(vDriveRoot), vVolumeFull, MAX_PATH) of FALSE: EXIT; end;

  var vVolumeGUID:string  := vVolumeFull;
  var vPos1               := pos('{', vVolumeGUID);
  var vPos2               := pos('}', vVolumeGUID);
  vVolumeGUID             := copy(vVolumeGUID, vPos1, vPos2 - vPos1 + 1);

  var vVolumeKey := 'Software\Microsoft\Windows\CurrentVersion\Explorer\BitBucket\Volume\' + vVolumeGUID;

  var vReg := TRegistry.create(KEY_READ);
  try
    vReg.RootKey := HKEY_CURRENT_USER;
    case vReg.openKeyReadOnly(vVolumeKey) of FALSE: EXIT; end;

    case vReg.valueExists(NUKE_ON_DELETE) of TRUE: result := vReg.readInteger(NUKE_ON_DELETE) <> 1; end; // <> 1 = drive recycles
  finally
    vReg.free;
  end;
end;

function mmpIsEditFriendly(const aFilePath: string): boolean;
begin
  result := FALSE;
  var vDirtyChars:string  := DIRTY_CHARS;
  var vNoExt              := extractFilePath(aFilePath) + mmpFileNameWithoutExtension(aFilePath);
  for var i := 1 to length(vNoExt) do
    case vDirtyChars.contains(vNoExt[i]) of TRUE: EXIT; end;
  result := TRUE;
end;

function mmpIsFileInUse(const aFilePath: string; out aSysErrorMessage: string): boolean;
begin
  result            := FALSE;
  aSysErrorMessage  := EMPTY;
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
  aSysErrorMessage := EMPTY;
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
                                                  mmpShowOKCancelMsgDlg(vMsg);
                                                  EXIT; end;end;

  var vWasPlaying := (GS.mediaType in [mtAudio, mtVideo]) and mmp.cmd(evMPReqPlaying).tf;
  mmp.cmd(vWasPlaying, evMPPause);

  case mmpCheckRecycleBin(aFolderPath) of FALSE: EXIT; end;
  case mmpShowConfirmDelete(aFolderPath, doKeepDelete, CF.asDeleteMethod[CONF_DELETE_METHOD], CF[CONF_DELETE_METHOD], CF.asInteger[CONF_SCALE_FACTOR], FALSE) = mrYES of FALSE: EXIT; end;

  mmp.cmd(evGSSuspended, TRUE); // prevent evMPStop from triggering next media file in TVM.onMPNotify
  mmp.cmd(evMPStop);
  mmpDelay(250);                // give MPV time to stop

  try

    case findFirst(aFolderPath + '*.*', faFilesOnly, vSR) = 0 of  TRUE:
      repeat
        case fileOK of TRUE: mmpShredThis(aFolderPath + vSR.Name, CF.asDeleteMethod[CONF_DELETE_METHOD]); end;
      until findNext(vSR) <> 0;
    end;

    system.sysUtils.findClose(vSR);
    mmpRunTasks;

  finally
    mmp.cmd(evGSSuspended, FALSE);
  end;

  result := TRUE;
end;

function mmpRenameFile(const aFilePath: string; const aNewFileNamePart: string = EMPTY): string;
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

    case aNewFileNamePart <> EMPTY of  TRUE:  s := aNewFileNamePart; // the calling code has already supplied the new name part without the extension
                                      FALSE:  begin
                                              try
                                                mmp.cmd(evGSUserInput, TRUE);
                                                s := mmpInputBoxForm(vOldFileNamePart); // the form returns the edited filename or the original if the user pressed cancel
                                              finally
                                                mmp.cmd(evGSUserInput, FALSE);
                                              end;end;end;
  except
    s := EMPTY;   // any funny business, force the rename to be abandoned
  end;
  case (s = EMPTY) OR (s = vOldFileNamePart) of TRUE: EXIT; end; // nothing to do

  vNewFilePath := extractFilePath(aFilePath) + s + vExt;  // construct the full path and new filename with the original extension

  case system.sysUtils.renameFile(aFilePath, vNewFilePath) of  TRUE: result := vNewFilePath;
                                                              FALSE: mmpShowOKCancelMsgDlg('Rename failed:' + #13#10#13#10 +  sysErrorMessage(getlasterror), MMP_TITLE, mtError, [mbOK]); end;
end;

function mmpRenameMMPFile(const aOldFilePath: string; const aNewFilePath: string): string;
// leave the original .seg file so cleanUp will delete any leftover .segnn. files listed in it
begin
  result := aNewFilePath;
  var vOldMMP := changeFileExt(aOldFilePath, '.mmp');
  var vNewMMP := changeFileExt(aNewFilePath, '.mmp');
  case fileExists(vOldMMP) of TRUE: renameFile(vOldMMP, vNewMMP); end;

  var vOldKey := changeFileExt(aOldFilePath, '.key');
  var vNewKey := changeFileExt(aNewFilePath, '.key');
  case fileExists(vOldKey) of TRUE: renameFile(vOldKey, vNewKey); end;
end;

function mmpRunTasks: boolean;
begin
  result := FALSE;
  mmpStartTasks;
  result := TRUE;
end;

end.
