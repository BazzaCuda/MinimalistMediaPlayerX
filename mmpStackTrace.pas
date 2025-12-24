{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda <bazzacuda@gmx.com>
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
unit mmpStackTrace;

interface

{$ifopt D+}

uses
  SysUtils, Classes, JclDebug;

implementation

uses
  mmpShellUtils;

function GetExceptionStackInfoProc(P: PExceptionRecord): Pointer;
var
  LLines: TStringList;
  LText: String;
  LResult: PChar;
  jcl_sil: TJclStackInfoList;
begin
  LLines := TStringList.Create;
  try
    jcl_sil:=TJclStackInfoList.Create(True, 7, p.ExceptAddr, False, nil, nil);
    try
      jcl_sil.AddToStrings(LLines, true, true, true, true);
    finally
      FreeAndNil(jcl_sil);
    end;
    LText := LLines.Text;
    LResult := StrAlloc(Length(LText));
    StrCopy(LResult, PChar(LText));
    Result := LResult;
  finally
    LLines.Free;
  end;
end;

function GetStackInfoStringProc(Info: Pointer): string;
begin
  Result := string(PChar(Info));
end;

procedure CleanUpStackInfoProc(Info: Pointer);
begin
  StrDispose(PChar(Info));
end;

initialization
  reportMemoryLeaksOnShutdown := mmpEnvironmentVariable;
// Start the Jcl exception tracking and register our Exception stack trace provider
  case reportMemoryLeaksOnShutdown and JclStartExceptionTracking of  TRUE:  begin
                                                                              Exception.GetExceptionStackInfoProc := GetExceptionStackInfoProc;
                                                                              Exception.GetStackInfoStringProc    := GetStackInfoStringProc;
                                                                              Exception.CleanUpStackInfoProc      := CleanUpStackInfoProc; end;
end;

finalization
// Stop Jcl exception tracking and unregister our provider
  case reportMemoryLeaksOnShutdown and JclExceptionTrackingActive of  TRUE: begin
                                                                              Exception.GetExceptionStackInfoProc := NIL;
                                                                              Exception.GetStackInfoStringProc    := NIL;
                                                                              Exception.CleanUpStackInfoProc      := NIL;
                                                                              JclStopExceptionTracking; end;
end;
{$else}
implementation
{$endif}
end.
