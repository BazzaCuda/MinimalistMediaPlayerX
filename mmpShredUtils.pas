{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
  mmpConsts, mmpFolderUtils;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;

const
  NUMPASSES = 1;

implementation

uses
  winApi.shellApi,
  _debugWindow;

const
  CLEANINGBUFFERS = 3;

var
  cleanBuffer:    array[1..CLEANINGBUFFERS] of PBYTE;
  buffersAlloced: boolean;

//--------------------------------------------------------------------
//
// OverwriteFileName
//
// Securely deletes a file's original name by renaming it several
// times. This works by changing each non-'.' character in the file's
// name to successive alphabetic characters, thus overwriting the
// name 26 times.
//
//--------------------------------------------------------------------
function overwriteFileName(const aFilePath:  string): string;
var
  newName:    string;
  lastSlash:  integer;
	i, j, ix:   integer;
begin
  result    := aFilePath;
  lastSlash := lastDelimiter('\', result);
	ix        := lastSlash + 1;

	// rename the file 26 times
	newName := aFilePath;
	for i := 0 to 25 do begin
		// Replace each non-'.' character with the same letter
		for j := ix to length(aFilePath) do
			case aFilePath[j] = '.' of FALSE: newName[j] := chr(ord('A') + random(25)); end; //  was chr(ord('A') + i);
		// Got a new name so rename
		case moveFile(PWideChar(result), PWideChar(newName)) of FALSE: EXIT; end;

		result := newName;
	end;
end;

//--------------------------------------------------------------------
//
// SecureOverwrite
//
// This function implements a secure sanitize of rigid (removable
// and fixed) disk media as per the Department of Defense clearing
// and sanitizing standard: DOD 5220.22-M
//
// The standard states that hard disk media is sanatized by
// overwriting with a character, then the character's complement,
// and then a random character. Note that the standard specifically
// states that this method is not suitable for TOP SECRET information.
// TOP SECRET data sanitizing is only achievable by a Type 1 or 2
// degauss of the disk, or by disintegrating, incinerating,
// pulverizing, shredding or melting the disk.
//
//--------------------------------------------------------------------
function secureOverwrite(aFileHandle: THandle; aLength: ULONGLONG): boolean;
const
  CLEANBUFSIZE = 65536;
var
	i, j, passes: DWORD;
	totalWritten: ULONGLONG;
	bytesWritten,
  bytesToWrite: ULONG;
	seekLength:   LONG;
	status:       BOOLEAN;
begin
  result := FALSE;

	// Allocate our cleaning buffers if necessary (we just let program exit free the buffers).
	if (NOT buffersAlloced) then begin

    randomize; // Seed the random number generator
    for i := 1 to CLEANINGBUFFERS do begin
      cleanBuffer[i] := virtualAlloc(NIL, CLEANBUFSIZE, MEM_COMMIT, PAGE_READWRITE);
      if (cleanBuffer[i] = NIL) then begin
        for j := 1 to i do virtualFree(cleanBuffer[j], 0, MEM_RELEASE);
        EXIT;
      end;

      // Fill each buffer with a different signature
      case i of
        1:   for j := 0 to CLEANBUFSIZE - 1 do PBYTE(cleanBuffer[i] + j)^ := BYTE(0);           // fill with zeroes
        2:   for j := 0 to CLEANBUFSIZE - 1 do PBYTE(cleanBuffer[i] + j)^ := BYTE(255);         // fill with complement of 0 - 0xFF
        3:   for j := 0 to CLEANBUFSIZE - 1 do PBYTE(cleanBuffer[i] + j)^ := BYTE(Random(255)); // fill with a random value
      end;
    end;
    buffersAlloced := TRUE;
  end;

	// Do the overwrite
  for passes := 1 to NUMPASSES do begin
		if (passes <> 1) then begin
      seekLength := aLength;
			setFilePointer(aFileHandle, -seekLength, NIL, FILE_CURRENT);
    end;

		for i := 1 to CLEANINGBUFFERS do begin
			// Move back to the start of where we're overwriting
			if (i <> 1) then begin
        seekLength := aLength;
				setFilePointer(aFileHandle, -seekLength, NIL, FILE_CURRENT);
      end;
			// Loop and overwrite
			totalWritten := 0;
			while (totalWritten < aLength) do begin
				if (aLength - totalWritten > 1024*1024) then bytesToWrite := 1024*1024
                                                else bytesToWrite := ULONG(aLength - totalWritten);
				if (bytesToWrite > CLEANBUFSIZE )       then bytesToWrite := CLEANBUFSIZE;

				status := writeFile(aFileHandle, cleanBuffer[i]^, bytesToWrite, &bytesWritten, nil);
				if (not status) then EXIT;

				// Note: no need to flush since the file is opened with write-through or no cache buffering
				totalWritten := totalWritten + bytesWritten;
			end;
		end;
	end;

	result := TRUE;
end;

function secureDelete(const aFilePath: pWideChar; fileLengthHi: DWORD; fileLengthLo: DWORD): integer;
var
  hFile:        THandle;
  FileLength:   ULARGE_INTEGER;
  bytesWritten: uLARGE_INTEGER;
  bytesToWrite: ULONGLONG;
  lastFileName: string;
  vFileLength: ULARGE_INTEGER;
begin
	// Open the file in overwrite mode
	hFile := createFile(aFilePath, GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, NIL, OPEN_EXISTING, FILE_FLAG_WRITE_THROUGH, 0);

  result := -1;
	case (hFile = INVALID_HANDLE_VALUE) of TRUE: EXIT; end;

	// If the file has a non-zero length, fill it with 0's first in order to
  // preserve its cluster allocation.
	if (fileLengthLo + fileLengthHi) <> 0 then begin
		// Seek to the last byte of the file
//		dec(fileLengthLo);
//		if (fileLengthLo = DWORD(-1) AND fileLengthHi) then dec(fileLengthHi);
    vFileLength.lowPart   := fileLengthLo;
    vFileLength.highPart  := fileLengthHi;
    setFilePointerEx(hFile, fileLengthLo, @vFileLength.highPart, FILE_BEGIN);
		// Write one zero byte, which causes the file system to fill the entire file's on-disk contents with 0
    result := -2;
		case secureOverwrite(hFile, DWORD(1)) of FALSE: EXIT; end;
  end;

	// Now go back to the start of the file and overwrite the rest of the file.
	setFilePointer(hFile, 0, NIL, FILE_BEGIN);
	fileLength.lowPart  := fileLengthLo;
	fileLength.highPart := fileLengthHi;
  bytesWritten.quadPart := 0;
  while (bytesWritten.quadPart < fileLength.quadPart) do begin
    bytesToWrite := min(fileLength.quadPart - bytesWritten.quadPart, 65536);
    result := -3;
    if (NOT secureOverwrite(hFile, DWORD(bytesToWrite))) then begin
      closeHandle(hFile);
      EXIT;
    end;
    bytesWritten.quadPart := bytesWritten.quadPart + bytesToWrite;
  end;

	// Done!
	closeHandle(hFile);

	// Rename the file a few times
	lastFileName := overwriteFileName(aFilePath);

  EXIT;

	// Now we can delete the file
	if (NOT deleteFile(lastFileName)) then begin
    result := -4;
		// Rename back to original name so as not to confuse the user
		case moveFile(PWideChar(lastFileName), PWideChar(aFilePath)) of FALSE: result := -5; end;
    EXIT;
  end;

  result := 0;
end;

function secureDeleteFile(const aFilePath: string): integer;
var
  vSR: TSearchRec;
begin
  result := -1;
  case findFirst(aFilePath, faAnyFile, vSR) = 0 of FALSE: EXIT; end;
  result := secureDelete(pWideChar(aFilePath), vSR.findData.nFileSizeHigh, vSR.findData.nFileSizeLow);
  findClose(vSR);
end;

function recycleDeleteFile(const aFilePath: string): integer;
var
  vFileOp: TSHFileOpStruct;
begin
  result := -1;
  case integer(getFileAttributes(PChar(aFilePath))) = -1 of TRUE: EXIT; end;

  zeroMemory(@vFileOp, sizeOf(vFileOp));
  vFileOp.wFunc   := FO_DELETE;
  vFileOp.pFrom   := PChar(aFilePath);
  vFileOp.fFlags  := FOF_ALLOWUNDO OR FOF_SILENT OR FOF_NOCONFIRMATION;
  result := SHFileOperation(vFileOp);
end;

function standardDeleteFile(const aFilePath: string): integer;
begin
  result := -1;
  case deleteFile(aFilePath) of FALSE: EXIT; end;
  result := 0;
end;

type
  PThreadRec  = ^TThreadRec;
  TThreadRec  = record
   trFilePath:      string;
   trDeleteMethod:  TDeleteMethod;
  end;

function threadIt(pThreadRec: PThreadRec): integer;
begin
  try
    case pThreadRec.trDeleteMethod of
      dmRecycle:  result := recycleDeleteFile(pThreadRec.trFilePath);
      dmStandard: result := standardDeleteFile(pThreadRec.trFilePath);
      dmShred:    result := secureDeleteFile(pThreadRec.trFilePath);
    end;
  finally
    dispose(pThreadRec);
  end;
end;

function shredIt(const aFilePath: string; const aDeleteMethod: TDeleteMethod): boolean;
var
  threadID:   LONGWORD;
  vThreadRec: PThreadRec;
begin
  new(vThreadRec);
  vThreadRec.trFilePath     := aFilePath;
  vThreadRec.trDeleteMethod := aDeleteMethod;
  closeHandle(beginThread(NIL, 0, @threadIt, vThreadRec, 0, threadID));
end;

function shredFolderFiles(const aFolderPath: string; const aDeleteMethod: TDeleteMethod): integer;
const
  faFilesOnly = faAnyFile AND NOT faDirectory AND NOT faHidden AND NOT faSysFile;
var
  vFolderPath:  string;
  SR:           TSearchRec;
begin
  vFolderPath := mmpITBS(aFolderPath);
  case findFirst(vFolderPath + '*.*', faFilesOnly, SR) = 0 of TRUE:
    repeat
      shredIt(vFolderPath + SR.name, aDeleteMethod);
    until findNext(SR) <> 0;
  end;
  findClose(SR);
end;

function mmpShredThis(const aFullPath: string; const aDeleteMethod: TDeleteMethod): boolean;
begin
  case fileExists(aFullPath) of  TRUE: shredIt(aFullPath, aDeleteMethod);
                                FALSE: case directoryExists(aFullPath) of TRUE: shredFolderFiles(aFullPath, aDeleteMethod); end;end;
end;

end.
