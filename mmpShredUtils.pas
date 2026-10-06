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
  mmpAction, mmpConsts, mmpFolderUtils, mmpUtils;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;
function mmpStartTasks: TVoid;

implementation

uses
  winApi.shellApi,
  system.classes, system.generics.collections, system.threading,
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

function trimFileRange(const aFilePath: string): boolean;
begin
  result := FALSE;
  var vHFile := createFile(pWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  case vHFile = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  try
    var vFileLength: int64 := 0;
    case getFileSizeEx(vHFile, vFileLength) of FALSE: EXIT; end;

    var vTrim: TFileLevelTrim;
    var vBytesReturned: cardinal;

    vTrim.Key := 0;
    vTrim.NumRanges := 1;
    vTrim.Ranges[0].Offset := 0;
    vTrim.Ranges[0].Length := vFileLength;

    deviceIoControl(vHFile, FSCTL_FILE_LEVEL_TRIM, @vTrim, sizeOf(vTrim), NIL, 0, vBytesReturned, NIL);
    result := TRUE;
  finally
    closeHandle(vHFile);
  end;
end;

procedure renameDelay(const aMilliseconds: cardinal);
begin
  var vStart: uint64 := getTickCount64;
  repeat
    sleep(1);
  until (getTickCount64 - vStart) >= aMilliseconds;
end;

function overwriteFileName(const aFilePath: string): string;
begin
  result := aFilePath;
  var vLastSlash := lastDelimiter('\', result);
  var vIx := vLastSlash + 1;
  var vNewName := aFilePath;

  for var i := 0 to 25 do begin
    for var j := vIx to length(aFilePath) do
      case aFilePath[j] = '.' of FALSE: vNewName[j] := chr(ord('A') + random(26)); end;

    var vMoved := moveFile(pWideChar(result), pWideChar(vNewName));
    case vMoved of FALSE: renameDelay(10); end;
    case not vMoved and not moveFile(pWideChar(result), pWideChar(vNewName)) of TRUE: EXIT; end;

    result := vNewName;
  end;
end;

function secureOverwrite(const aFileHandle: THandle; const aLength: ULONGLONG): boolean;
const
  CLEAN_BUF_SIZE = 1048576;
begin
  result := FALSE;
  var vCleanBuffer: PBYTE := virtualAlloc(NIL, CLEAN_BUF_SIZE, MEM_COMMIT, PAGE_READWRITE);
  case vCleanBuffer = NIL of TRUE: EXIT; end;

  try
    var vTotalWritten: ULONGLONG := 0;
    while vTotalWritten < aLength do begin
      var vBytesToWrite: DWORD := DWORD(min(uint64(CLEAN_BUF_SIZE), aLength - vTotalWritten));
      var vWritten: DWORD := 0;

      case writeFile(aFileHandle, vCleanBuffer^, vBytesToWrite, vWritten, NIL) of FALSE: EXIT; end;
      case (vWritten = 0) and (vBytesToWrite > 0) of TRUE: EXIT; end;

      vTotalWritten := vTotalWritten + ULONGLONG(vWritten);
    end;

    case flushFileBuffers(aFileHandle) of FALSE: EXIT; end;
    result := TRUE;
  finally
    virtualFree(vCleanBuffer, 0, MEM_RELEASE);
  end;
end;

function secureDelete(const aFilePath: string): integer;
begin
  result := -1;
  var vHFile := createFile(pWideChar(aFilePath), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);
  case vHFile = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  try
    var vFileLength: int64 := 0;
    result := -2;
    case getFileSizeEx(vHFile, vFileLength) of FALSE: EXIT; end;

    var vBytesWritten: int64 := 0;
    while vBytesWritten < vFileLength do begin
      var vBytesToWrite: ULONGLONG := ULONGLONG(min(int64(1048576), vFileLength - vBytesWritten));
      result := -3;
      case secureOverwrite(vHFile, vBytesToWrite) of FALSE: EXIT; end;

      vBytesWritten := vBytesWritten + int64(vBytesToWrite);
      case vFileLength > 0 of TRUE: mmp.cmd(evGSActiveTaskPercent, trunc((vBytesWritten * 100) / vFileLength)); end;
    end;
  finally
    closeHandle(vHFile);
  end;

  var vScrambledPath := overwriteFileName(aFilePath);
  result := -6;
  case trimFileRange(vScrambledPath) of FALSE: EXIT; end;

  result := -7;
  case deleteFile(pWideChar(vScrambledPath)) of FALSE: EXIT; end;

  result := 0;
end;

function secureDeleteFile(const aFilePath: string): integer;
begin
  result := -10;
  case fileExists(aFilePath) of TRUE: result := secureDelete(aFilePath); end;
end;

//=====

function recycleFile(const aFilePath: string): boolean;
var
  vFileOp: TSHFileOpStructW;
begin
  fillChar(vFileOp, sizeOf(vFileOp), 0);
  vFileOp.wFunc  := FO_DELETE;
  vFileOp.pFrom  := pWideChar(aFilePath + #0);
  vFileOp.fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;

  result := shFileOperationW(vFileOp) = 0;
end;

function driveSupportsRecycleBin(const aFilePath: string): boolean;
begin
  var vDrive := mmpITBS(extractFileDrive(aFilePath));
  var vRecycleBin := vDrive + '$RECYCLE.BIN';

  result := TRUE;
  case directoryExists(vRecycleBin) of TRUE: EXIT; end;

  var vTempFilePath := vDrive + '__MMP_RECYCLE_BIN_TEST__';
  var vHandle: THandle := createFileW(pWideChar(vTempFilePath), GENERIC_WRITE, 0, NIL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

  result := FALSE;
  case vHandle = INVALID_HANDLE_VALUE of TRUE: EXIT; end;

  closeHandle(vHandle);
  recycleFile(vTempFilePath);
  result := directoryExists(vRecycleBin);
end;

function recycleDeleteFile(const aFilePath: string): boolean;
begin
  result := FALSE;
  case integer(getFileAttributesW(pWideChar(aFilePath))) = -1 of TRUE: EXIT; end;
  result := recycleFile(aFilePath);
end;

function standardDeleteFile(const aFilePath: string): boolean;
begin
  result := FALSE;
  case deleteFile(aFilePath) of FALSE: EXIT; end;
  result := TRUE;
end;

var
  gTasks: TList<ITask>;
  gCount: integer = 0;
  gShredThreadPool: TThreadPool;

function threadIt(const aFilePath: string): boolean;
begin
  var vTask: ITask := TTask.create(
    procedure
    begin
      try
        try
          secureDeleteFile(aFilePath);
        finally
          interlockedDecrement(gCount);
        end;
      except
      end;
    end, gShredThreadPool);

  gTasks.add(vTask);
  result := TRUE;
end;

function shredIt(const aFilePath: string; const aDeleteMethod: TDeleteMethod): boolean;
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
  {$WARN SYMBOL_PLATFORM OFF}
  faFilesOnly = faAnyFile and not faDirectory and not faHidden and not faSysFile;
  {$WARN SYMBOL_PLATFORM ON}
begin
  result := FALSE;
  var vFolderPath := mmpITBS(aFolderPath);
  var SR: TSearchRec;

  var vFound := findFirst(vFolderPath + '*.*', faFilesOnly, SR) = 0;
  try
    case vFound of
      TRUE: repeat
        result := shredIt(vFolderPath + SR.name, aDeleteMethod);
      until findNext(SR) <> 0;
    end;
  finally
    case vFound of TRUE: findClose(SR); end;
  end;
end;

function monitorTasks: TVoid;
begin
  gCount := gTasks.count;
  mmp.cmd(evGSActiveTasks, gCount);

  for var i := 0 to gTasks.count - 1 do gTasks[i].start;

  repeat
    mmp.cmd(evGSActiveTasks, gCount);
    mmpDelay(100);
  until gCount = 0;

  mmp.cmd(evSTOpInfo2, -1);
  mmp.cmd(evGSActiveTaskPercent, -1);
  mmp.cmd(evGSActiveTasks, 0);
  gTasks.clear;
end;

function mmpStartTasks: TVoid;
begin
  monitorTasks;
end;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;
begin
  result := FALSE;
  case fileExists(aFullPath) of
    TRUE:  result := shredIt(aFullPath, aDeleteMethod);
    FALSE: case directoryExists(aFullPath) of TRUE: result := shredFolderFiles(aFullPath, aDeleteMethod); end;
  end;
end;

initialization
  gShredThreadPool := TThreadPool.create;
  gShredThreadPool.setMaxWorkerThreads(10);
  gTasks := TList<ITask>.create;

finalization
  gTasks.free;
  gShredThreadPool.free;

end.
