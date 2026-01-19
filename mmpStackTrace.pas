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

implementation

uses
  system.classes, system.sysUtils,
  jclDebug,
  mmpConsts, mmpShellUtils,
  _debugWindow;

function getExceptionStackInfoProc(P: PExceptionRecord): pointer;
var
  vLines:   TStringList;
  vText:    string;
  vResult:  PChar;
  vJcl_sil: TJclStackInfoList;
begin
  vLines := TStringList.create;
  try
    vJcl_sil:=TJclStackInfoList.Create(True, 7, p.ExceptAddr, FALSE, NIL, NIL);
    try
      vJcl_sil.addToStrings(vLines, TRUE, TRUE, TRUE, TRUE);
    finally
      freeAndNil(vJcl_sil);
    end;
    vText := vLines.Text;
    vResult := strAlloc(length(vText));
    strCopy(vResult, PChar(vText));
    result := vResult;
  finally
    vLines.free;
  end;
end;

function getStackInfoStringProc(info: pointer): string;
begin
  result := string(PChar(info));
end;

procedure cleanUpStackInfoProc(info: pointer);
begin
  strDispose(PChar(info));
end;

initialization
  // {$if BazDebugWindow} debugClear; {$endif}
  reportMemoryLeaksOnShutdown := mmpEnvironmentVariable;
  {$if BazDebugWindow} debugBoolean('reportMemoryLeaksOnShutdown', reportMemoryLeaksOnShutdown); {$endif}

// Start the Jcl exception tracking and register our Exception stack trace provider
  case reportMemoryLeaksOnShutdown and jclStartExceptionTracking of  TRUE:  begin
                                                                              {$if BazDebugWindow} debugBoolean('jclStartExceptionTracking', TRUE); {$endif}
                                                                              exception.getExceptionStackInfoProc := getExceptionStackInfoProc;
                                                                              exception.getStackInfoStringProc    := getStackInfoStringProc;
                                                                              exception.cleanUpStackInfoProc      := cleanUpStackInfoProc; end;end;
  {$if BazDebugWindow} debugBoolean('jclExceptionTrackingActive', JclExceptionTrackingActive); {$endif}
  {$if BazDebugWindow} debug(EMPTY); {$endif}

finalization
// Stop Jcl exception tracking and unregister our provider
  {$if BazDebugWindow} debug(EMPTY); {$endif}
  {$if BazDebugWindow} debugBoolean('jclExceptionTrackingActive', JclExceptionTrackingActive); {$endif}
  case reportMemoryLeaksOnShutdown and jclExceptionTrackingActive of  TRUE: begin
                                                                              {$if BazDebugWindow} debugBoolean('jclStopExceptionTracking', TRUE); {$endif}
                                                                              exception.getExceptionStackInfoProc := NIL;
                                                                              exception.getStackInfoStringProc    := NIL;
                                                                              exception.cleanUpStackInfoProc      := NIL;
                                                                              jclStopExceptionTracking; end;end;
{$else}
implementation
{$endif}
end.
