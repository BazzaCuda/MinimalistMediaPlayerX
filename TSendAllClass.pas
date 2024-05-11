unit TSendAllClass;

interface

uses winAPI.windows;

type
  TSendAll = class(TObject)
  strict private
    FHWNDs: array of HWND;
  constructor Create;
  destructor  Destroy;  override;
  protected
  private
    function getHWND(index: integer): HWND;
  public
    function add(const aHWND: HWND): boolean;
    function clear: boolean;
    function count: integer;
    function postToAll(const cmd: WORD): boolean;
    function postToAllEx(const cmd: WORD; const pt: TPoint): boolean;
    function sendToAll(const cmd: WORD): boolean;
    function sendToAllEx(const cmd: WORD; const pt: TPoint): boolean;
    property HWNDs[index: integer]: HWND read getHWND;
  end;

function SA: TSendAll;

implementation

uses
  system.sysUtils, consts, system.classes, vcl.forms, _debugWindow;

var
  gSendAll: TSendAll;

function SA: TSendAll;
begin
  case gSendAll = NIL of TRUE: gSendAll := TSendAll.Create; end;
  result := gSendAll;
end;

function enumAllWindows(handle: HWND; lparam: LPARAM): BOOL; stdcall;
var
  windowName : array[0..255] of char;
  className  : array[0..255] of char;
begin
  case getClassName(handle, className, sizeOf(className)) > 0 of TRUE:
    case strComp(className, 'TMMPUI') = 0 of TRUE: SA.add(handle); end;end;

  result := TRUE;
end;

{ TSendAll }

function TSendAll.add(const aHWND: HWND): boolean;
begin
  setLength(FHWNDs, length(FHWNDs) + 1);
  FHWNDs[high(FHWNDs)] := aHWND;
  result := TRUE;
end;

function TSendAll.clear: boolean;
begin
  setLength(FHWNDs, 0);
  result := TRUE;
end;

function TSendAll.count: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);
  result := length(FHWNDs);
end;

constructor TSendAll.Create;
begin
  inherited;
  clear;
end;

destructor TSendAll.Destroy;
begin
  clear;
  inherited;
end;

function TSendAll.getHWND(index: integer): HWND;
begin
  result := FHWNDS[index - 1]; // for simplicity in UI.arrangeAll, index parameter is 1-based
end;

function TSendAll.postToAll(const cmd: WORD): boolean;
begin
  postToAllEx(cmd, point(0, 0));
end;

function TSendAll.sendToAll(const cmd: WORD): boolean;
begin
  sendToAllEx(cmd, point(0, 0));
end;

function TSendAll.postToAllEx(const cmd: WORD; const pt: TPoint): boolean;
var
  i: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);

  for i := low(FHWNDs) to high(FHWNDs) do
    postMessage(FHWNDs[i], cmd, pt.x, pt.y);

  clear;
  result := TRUE;
end;

function TSendAll.sendToAllEx(const cmd: WORD; const pt: TPoint): boolean;
var
  i: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);

  for i := low(FHWNDs) to high(FHWNDs) do
    sendMessage(FHWNDs[i], cmd, pt.x, pt.y);

  clear;
  result := TRUE;
end;

initialization
  gSendAll := NIL;

finalization
  case gSendAll <> NIL of TRUE: gSendAll.free; end;

end.
