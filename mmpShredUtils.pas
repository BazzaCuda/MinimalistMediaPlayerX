{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA

    Ported to Delphi from Mark Russinovich's SDelete v1.5 (1999) in the [good old] days when he provided the source code.
}
unit mmpShredUtils;

interface

uses
  winApi.windows,
  system.math, system.sysUtils,
  vcl.dialogs,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpFolderUtils, mmpUtils;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;
function mmpStartTasks: boolean;

implementation

uses
  winApi.shellApi,
  system.generics.collections, system.threading,
  bazCmd,
  mmpDialogs, mmpGlobalState,
  _debugWindow;

//=====

type
  TFileLevelTrimRange = packed record
    Offset: int64;
    Length: int64;
  end;

  TFileLevelTrim = packed record
    Key:        cardinal;
    NumRanges:  cardinal;
    Ranges: array[0..0] of TFileLevelTrimRange;
  end;

const
  FSCTL_FILE_LEVEL_TRIM = $00098208;

function  trimFileRange(const aFilePath: string): boolean;
begin
  result     := FALSE;
  var vHFile := createFile(pWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  case vHFile = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  try
    var  vFileLength: int64 := 0;
    case getFileSizeEx(vHFile, vFileLength) of FALSE: EXIT; end;

    var vTrim:          TFileLevelTrim;
    var vBytesReturned: cardinal;

    vTrim.Key              := 0;
    vTrim.NumRanges        := 1;
    vTrim.Ranges[0].Offset := 0;
    vTrim.Ranges[0].Length := vFileLength;

    // We call the control code but don't force a FALSE result if the drive simply doesn't support TRIM (e.g., an HDD).
    deviceIoControl(vHFile, FSCTL_FILE_LEVEL_TRIM, @vTrim, sizeof(vTrim), nil, 0, vBytesReturned, nil);
    result := TRUE;
  finally
    closeHandle(vHFile); end;
end;

procedure renameDelay(const aMilliseconds: Cardinal);
var
  vStart: uint64;
begin
  vStart := getTickCount64;
  repeat
    { A 1ms sleep is the most effective way to yield a background thread }
    { while ensuring the CPU doesn't spike to 100% during the wait }
    sleep(1);
  until (getTickCount64 - vStart) >= aMilliseconds;
end;

function overwriteFileName(const aFilePath:  string): string;
begin
  result        := aFilePath;
  var lastSlash := lastDelimiter('\', result);
	var ix        := lastSlash + 1;

	// rename the file 26 times
	var newName := aFilePath;
	for var i := 0 to 25 do begin
		// Replace each non-'.' character with a random alphabetic
		for var j := ix to length(aFilePath) do
			case aFilePath[j] = '.' of FALSE: newName[j] := chr(ord('A') + random(26)); end;
		// Got a new name so rename
    case moveFile(PWideChar(result), PWideChar(newName)) of FALSE:  begin
                                                                      renameDelay(10); // don't compete with anti-virus scanners now we've closed the file handle
                                                                      case moveFile(PWideChar(result), PWideChar(newName)) of FALSE: EXIT; end;end;end; // OK, they win!

	  result := newName;

	end;
end;

function secureOverwrite(const aFileHandle: THandle; const aLength: ULONGLONG): boolean;
const
  CLEAN_BUF_SIZE = 65536;
begin
  result := FALSE;
  var vCleanBuffer: PBYTE := virtualAlloc(NIL, CLEAN_BUF_SIZE, MEM_COMMIT, PAGE_READWRITE);
  case vCleanBuffer = NIL of TRUE: EXIT; end;

  try
    for var i: WORD := 0 to CLEAN_BUF_SIZE - 1 do vCleanBuffer[i] := 0;

    var vTotalWritten: ULONGLONG := 0;
    while vTotalWritten < aLength do begin
      var vBytesToWrite:  DWORD := DWORD(min(ULONGLONG(CLEAN_BUF_SIZE), aLength - vTotalWritten));
      var vWritten:       DWORD := 0;

      case writeFile(aFileHandle, vCleanBuffer^, vBytesToWrite, vWritten, NIL) of FALSE: EXIT; end;
      case (vWritten = 0) and (vBytesToWrite > 0) of TRUE: EXIT; end;

      vTotalWritten := vTotalWritten + ULONGLONG(vWritten); end;

    case flushFileBuffers(aFileHandle) of FALSE: EXIT; end;

    result := TRUE;
  finally
    virtualFree(vCleanBuffer, 0, MEM_RELEASE); end;
end;

function secureDelete(const aFilePath: string): integer;
begin
  result     := -1;
  var vHFile := createFile(pWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  case vHFile = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  try
    var vFileLength: int64 := 0;
    result                 := -2;
    case getFileSizeEx(vHFile, vFileLength) of FALSE: EXIT; end;

    var vBytesWritten: int64 := 0;
    while vBytesWritten < vFileLength do begin
      var vBytesToWrite: ULONGLONG := ULONGLONG(min(int64(65536), vFileLength - vBytesWritten));
      result                       := -3;
      case secureOverwrite(vHFile, vBytesToWrite) of FALSE: EXIT; end;

      vBytesWritten := vBytesWritten + int64(vBytesToWrite);
      case vFileLength > 0 of TRUE: mmp.cmd(evGSActiveTaskPercent, trunc((vBytesWritten * 100) / vFileLength)); end;end;
  finally
    closeHandle(vHFile);
  end;

  var vScrambledPath := overwriteFileName(aFilePath);
  // debugString('scrambledPath', vScrambledPath);

  result := -6;
  case trimFileRange(vScrambledPath) of FALSE: EXIT; end;

  result := -7;
  case deleteFile(pWideChar(vScrambledPath)) of FALSE: EXIT; end;

  result := 0;
end;

function secureDeleteFile(const aFilePath: string): integer;
begin
  // debugString('secureDeleteFile', aFilePath);
  result := -10;
  case fileExists(aFilePath) of TRUE: result := secureDelete(aFilePath); end;
  // debugInteger('secureDeleteFile', result);
end;

//=====

function recycleFile(const aFilePath: string): boolean;
var
  vFileOp: TSHFileOpStructW;
begin
  result := FALSE;

  fillChar(vFileOp, sizeOf(vFileOp), 0);
  vFileOp.wFunc   := FO_DELETE;
  vFileOp.pFrom   := PWideChar(aFilePath + #0); // double-null-terminated
  vFileOp.fFlags  := FOF_ALLOWUNDO OR FOF_SILENT OR FOF_NOCONFIRMATION; // FOF_ALLOWUNDO is a request not a demand

  result          := SHFileOperationW(vFileOp) = 0;
end;

function driveSupportsRecycleBin(const aFilePath: string): boolean;
// currently redundant. Superseded by mmpCheckRecycleBin and mmpDriveHasRecycleBin in mmpFileUtils
begin
  var vDrive      := mmpITBS(extractFileDrive(aFilePath));
  var vRecycleBin := vDrive + '$RECYCLE.BIN';

  result := TRUE;
  case directoryExists(vRecycleBin) of TRUE: EXIT; end;

  var vTempFilePath := vDrive + '__MMP_RECYCLE_BIN_TEST__';

  var vHandle: THandle := createFileW(PWideChar(vTempFilePath),
    GENERIC_WRITE,
    0,
    NIL,
    CREATE_ALWAYS,
    FILE_ATTRIBUTE_NORMAL,
    0);

  result := FALSE;
  case vHandle = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  closeHandle(vHandle);

  recycleFile(vTempFilePath);

  result := directoryExists(vRecycleBin);
end;

function recycleDeleteFile(const aFilePath: string): boolean;
var
  vFileOp: TSHFileOpStructW;
begin
  result := FALSE;
  case integer(getFileAttributesW(PWideChar(aFilePath))) = -1 of TRUE: EXIT; end;
  result := recycleFile(aFilePath);
end;

function standardDeleteFile(const aFilePath: string): boolean;
begin
  result := FALSE;
  case deleteFile(aFilePath) of FALSE: EXIT; end;
  result := TRUE;
end;

type
  PThreadRec  = ^TThreadRec;
  TThreadRec  = record
    trFilePath: string;
  end;

var gTasks: TList<ITask>;
    gCount: integer = 0;
function threadIt(const aFilePath: string): boolean;
var vTask: ITask;
begin
  vTask := TTask.create(
                        procedure
                        begin
                          try
                            interlockedIncrement(gCount);
                            secureDeleteFile(aFilePath);
                            interlockedDecrement(gCount);
                          except end;
                        end);
  gTasks.add(vTask);
  result := TRUE;
end;

function shredIt(const aFilePath: string; const aDeleteMethod: TDeleteMethod): boolean;
var
  threadID:   LONGWORD;
  vThreadRec: PThreadRec;
begin
  result := FALSE;
  case aDeleteMethod of
    dmRecycle:  result := recycleDeleteFile(aFilepath);
    dmStandard: result := standardDeleteFile(aFilePath);
    dmShred:    result := threadIt(aFilePath);
  end;
end;

function shredFolderFiles(const aFolderPath: string; const aDeleteMethod: TDeleteMethod): boolean;
const
  faFilesOnly = faAnyFile AND NOT faDirectory AND NOT faHidden AND NOT faSysFile;
var
  vFolderPath:  string;
  SR:           TSearchRec;
begin
  result := FALSE;
  vFolderPath := mmpITBS(aFolderPath);
  case findFirst(vFolderPath + '*.*', faFilesOnly, SR) = 0 of TRUE:
    repeat
      result := shredIt(vFolderPath + SR.name, aDeleteMethod);
    until findNext(SR) <> 0;
  end;
  findClose(SR);
end;

function monitorTasks: boolean;
var i: integer;
begin
  result := FALSE;

  for i := 0 to gTasks.count - 1 do gTasks[i].start;

  repeat
    mmp.cmd(evGSActiveTasks, gCount);
    mmpDelay(100);
  until gCount = 0;

  mmp.cmd(evSTOpInfo2, -1);
  mmp.cmd(evGSActiveTaskPercent, -1); // suppress mmpVM from sending anything to mmpFormCaptions.OpInfo2 for display
  mmp.cmd(evGSActiveTasks, 0);        // was gCount
  gTasks.clear;

  result := TRUE;
end;

function mmpStartTasks: boolean;
begin
  result := monitorTasks;
end;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;
begin
  result := FALSE;
  case fileExists(aFullPath) of  TRUE: result := shredIt(aFullPath, aDeleteMethod);
                                FALSE: case directoryExists(aFullPath) of TRUE: result := shredFolderFiles(aFullPath, aDeleteMethod); end;end;
end;

initialization
  gTasks  := TList<ITask>.create;

finalization
  gTasks.free;

end.
