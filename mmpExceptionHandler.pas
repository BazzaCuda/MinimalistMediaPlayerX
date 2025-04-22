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
unit mmpExceptionHandler;

interface

uses
  system.sysUtils,
  vcl.forms;

type
  IMMPExceptionHandler = interface
    ['{FB33F347-340D-4E75-929D-36AC2880FA49}']
    function handler: TExceptionEvent;
  end;

function mmpException: IMMPExceptionHandler;

implementation

uses
  system.ioUtils,
  _debugWindow;

type
  TMMPExceptionHandler = class(TInterfacedObject, IMMPExceptionHandler)
  public
    procedure appExceptionHandler(sender: TObject; e: Exception);
    function  handler: TExceptionEvent;
  end;

function mmpException: IMMPExceptionHandler;
{$J+} const gEH: IMMPExceptionHandler = NIL; {$J-}
begin
  case gEH = NIL of TRUE: gEH := TMMPExceptionHandler.create; end;
  result := gEH;
end;

{ TMMPExceptionHandler }

procedure TMMPExceptionHandler.appExceptionHandler(sender: TObject; e: Exception); // must be procedure of object
begin
   // trap all exceptions but don't inconvenience the user
   // the developer on the other hand...meh! let him have it.
  case reportMemoryLeaksOnShutdown of FALSE: EXIT; end;

  debugFormat('e: class=%s, %s', [e.className, e.message]);
  var vLogFileName := changeFileExt(application.exeName, '.log');

  var vLogEntry := format('%s%s: %s%s',
    [formatDateTime('yyyy-mm-dd hh:nn:ss.zzz: ', now),
     e.className,
     e.message,
     sLineBreak]);

  var vCallStackPrefix := 'Call Stack:';
  case e is EAccessViolation of TRUE: vCallStackPrefix := 'Access Violation:'; end;
  case e is EInvalidPointer  of TRUE: vCallStackPrefix := 'Invalid Pointer Operation:'; end;

  vLogEntry := vLogEntry + format('%s%s%s%s', [vCallStackPrefix, sLineBreak, e.stackTrace, sLineBreak]); // stackTrace is populated by mmpStackTrace

  try
    TFile.appendAllText(vLogFileName, vLogEntry);
  except
    debug('Error writing to log file: ' + e.message);
  end;
end;

function TMMPExceptionHandler.handler: TExceptionEvent;
begin
  result := appExceptionHandler;
end;

end.
