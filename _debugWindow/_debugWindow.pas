(*
 * GExperts Debug Window Interface
 * http://www.gexperts.org
 *
 * You are free to use this code in any application to send commands to the
 * GExperts debug window.  This includes usage in commercial, shareware,
 * freeware, public domain, and other applications.
 *)

 //
 // Baz Cuda, 2022: Renamed, rejigged, camelhumped and augmented.
 //                 All credit rests with the original authors for this very handy tool.
unit _debugWindow;

interface

uses
  windows, dialogs, system.classes, system.generics.collections; // We need "Dialogs" for TMsgDlgType

{$I BazDebugWindow.inc}

{$if BazDebugWindow}
type
   TDebug = class(TObject)
     class procedure debugEnum<T>(const identifier: string; const value: T); // because...E2530 Type parameters not allowed on global procedure or function
   end;

procedure debug(const msg: string);
procedure debugBoolean(const identifier: string; const value: boolean);
procedure debugCardinal(const identifier: string; const value: cardinal);
procedure debugClear;
procedure debugDateTime(const identifier: string; const value: TDateTime);
procedure debugDouble(const identifier: string; const value: double);
procedure debugError(const msg: string);
procedure debugEx(const msg: string; MType: TMsgDlgType);
procedure debugFormat(const msg: string; const args: array of const);
procedure debugFormatEx(const msg: string; const args: array of const; MType: TmsgDlgType);
procedure debugIndent;
procedure debugint64(const identifier: string; const value: int64);
procedure debuginteger(const identifier: string; const value: integer);
procedure debugLongint(const identifier: string; const value: longint);
procedure debugMethodEnter(const methodName: string);
procedure debugMethodExit(const methodName: string);
procedure debugOutdent;
procedure debugPause;
procedure debugResume;
procedure debugSeparator;
procedure debugString(const identifier: string; const value: string);
procedure debugStringList(const identifier: string; const value: TStringList);
procedure debugWarning(const msg: string);
function  showDebugMessage(msg: string): boolean;
function  startDebugWin: hWnd;

implementation

uses
  Messages,
  SysUtils,
  Registry,
  Forms, // We need "Forms" for the Application object
  RTTI;

threadvar
  msgPrefix: string;

const
  chrStringCommand: AnsiChar = #4;
  chrSQLCommand:    AnsiChar = #2; // Old, unused type
  chrClearCommand:  AnsiChar = #3;
  chrNull:          AnsiChar = #0;

var
  pastFailedAttemptTostartDebugWin: boolean = False;
  debugPaused: boolean = False;

//===========================
function showDebugMessage(msg: string): boolean;
begin
  debug(msg);
  showMessage(msg);
end;

function startDebugWin: hWnd;
var
  debugFileName: string;
  buf: array[0..MAX_PATH + 1] of char;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  msgPrefix := '';

  result := 0;
  if pastFailedAttemptTostartDebugWin then
    EXIT;

  with TRegIniFile.Create('\Software\Baz') do // Do not localize.
  try
    debugFileName := ReadString('Debug', 'FilePath', ''); // Do not localize.
  finally
    free;
  end;

  if trim(DebugFileName) = '' then
  begin
    getModuleFileName(HINSTANCE, buf, sizeOf(buf) - 1);
    debugFileName := extractFilePath(strPas(buf)) + 'BazDebugWindow.exe'; // Do not localize.
  end;

  if (trim(debugFileName) = '') or NOT fileExists(debugFileName) then
  begin
    pastFailedAttemptTostartDebugWin := TRUE;
    EXIT;
  end;

  var vExeDir := extractFilePath(debugFileName);

  fillChar(si, sizeOf(si), #0);
  si.cb := sizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  if NOT createProcess(PChar(debugFileName), NIL, NIL, NIL,
                       FALSE, 0, NIL, PChar(vExeDir), si, pi) then
  begin
    pastFailedAttemptTostartDebugWin := TRUE;
    EXIT;
  end;

  try
    waitForInputIdle(pi.hProcess, 3 * 1000); // wait for 3 seconds to get idle
  finally
    closeHandle(pi.hThread);
    closeHandle(pi.hProcess);
  end;

  result := findWindow('TfmDebug', NIL);
end;
//===========================

procedure debug(const msg: string);
begin
  debugEx(msg, mtInformation);
end;

procedure debugBoolean(const identifier: string; const value: boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if value then
    debugEx(identifier + ' = TRUE', mtInformation)
  else
    debugEx(identifier + ' = FALSE', mtInformation);
end;

procedure debugCardinal(const identifier: string; const value: cardinal);
begin
  debugEx(format('%s = %d', [identifier, value]), mtInformation);
end;

procedure debugClear;
begin
  debug(string(chrClearCommand));
end;

procedure debugDateTime(const identifier: string; const value: TDateTime);
begin
  debugEx(identifier + ' = ' + DateTimeToStr(value), mtInformation);
end;

procedure debugDouble(const identifier: string; const value: double);
begin
  debugEx(identifier + ' = ' + floatToStr(value), mtInformation);
end;

procedure debugError(const msg: string);
begin
  debugEx(msg, mtError);
end;

procedure debugEx(const msg: string; MType: TMsgDlgType);
var
  CDS: TCopyDataStruct;
  debugWin: hWnd;
  messageString: string;
  msgBytes: array of Byte;
  msgType: AnsiChar;
  byteIndex: integer;

  procedure addByte(B: Byte);
  begin
    msgBytes[byteIndex] := B;
    inc(byteIndex);
  end;

  procedure addStringBytes(const str: string); overload;
  var
    i: integer;
    c: WideChar;
  begin
    for i := 1 to length(str) do begin
      c := str[i];
      addByte(word(c) and $FF);
      addByte(word(c) shr 8);
    end;
  end;

begin
  if debugPaused then
    Exit;

  debugWin := findWindow('TfmDebug', NIL);

  if debugWin = 0 then
    debugWin := startDebugWin;

  if debugWin <> 0 then
  begin
    byteIndex := 0;
    messageString := msgPrefix + msg;
    setLength(msgBytes, 1 + 1 + (length(messageString)* sizeOf(char)) + 1); // Payload, type, message, null
    CDS.cbData := length(msgBytes);
    CDS.dwData := 0;
    msgType := AnsiChar(Ord(MType) + 1);
    if msg = string(chrClearCommand) then
      addByte(byte(chrClearCommand))
    else
      addByte(byte(chrStringCommand));
    addByte(byte(msgType));
    addStringBytes(messageString);
    addByte(byte(chrNull));
    CDS.lpData := pointer(msgBytes);
    sendMessage(debugWin, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(@CDS));
  end;
end;

procedure debugFormat(const msg: string; const args: array of const);
begin
  debugFormatEx(msg, args, mtInformation);
end;

procedure debugFormatEx(const msg: string; const args: array of const; MType: TmsgDlgType);
begin
  debugEx(format(msg, args), MType);
end;

const
  INDENTATION = '    ';

procedure debugIndent;
begin
  msgPrefix := msgPrefix + INDENTATION;
end;

procedure debugint64(const identifier: string; const value: int64);
begin
  debugEx(format('%s = %d', [identifier, value]), mtInformation);
end;

procedure debuginteger(const identifier: string; const value: integer);
begin
  debugEx(format('%s = %d', [identifier, value]), mtInformation);
end;

procedure debugLongint(const identifier: string; const value: longint);
begin
  debugEx(format('%s = %d', [identifier, value]), mtInformation);
end;

procedure debugMethodEnter(const methodName: string);
begin
  debugEx('Enter ' + methodName, mtInformation);
  debugIndent;
end;

procedure debugMethodExit(const methodName: string);
begin
  debugOutdent;
  debugEx('Leave ' + methodName, mtInformation);
end;

procedure debugOutdent;
begin
  delete(msgPrefix, 1, Length(INDENTATION));
end;

procedure debugPause;
begin
  debugPaused := True;
end;

procedure debugResume;
begin
  debugPaused := False;
end;

procedure debugSeparator;
const
  separatorString = '------------------------------';
begin
  debugEx(separatorString, mtInformation);
end;

procedure debugString(const identifier: string; const value: string);
begin
  debugEx(format('%s = %s', [identifier, value]), mtInformation);
end;

procedure debugStringList(const identifier: string; const value: TStringList);
var
  i: integer;
  msg: string;
begin
  for i := 0 to value.Count - 1 do begin
    case i = 0 of TRUE: begin msg := identifier + ' = ' + IntToStr(i) + ': ' + value[i]; CONTINUE; end;end;
    msg := msg + ' ' + IntToStr(i) + ': ' + value[i];
  end;
  debugEx(msg, mtInformation);
end;

procedure debugWarning(const msg: string);
begin
  debugEx(msg, mtWarning);
end;

{ TDebug }

class procedure TDebug.debugEnum<T>(const identifier: string; const value: T);
begin
  var vResult := TRttiEnumerationType.getName(value);
  debugEx(format('%s = %s', [identifier, vResult]), mtInformation);
end;
{$else}
implementation
{$endif}

end.

