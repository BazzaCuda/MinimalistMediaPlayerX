unit TSendAllClass;

interface

uses winAPI.windows;

type
  TSendAll = class(TObject)
  strict private
    FHWNDs: array of HWND;
  protected
  private
    function getHWND(index: integer): HWND;
  public
    constructor Create;
    destructor  Destroy;  override;
    function add(const aHWND: HWND): boolean;
    function clear: boolean;
    function count: integer;
    function postToAll(const aCmd: WORD; const aPostToAll: boolean = FALSE): boolean;
    function postToAllEx(const aCmd: WORD; const pt: TPoint; const aPostToAll: boolean = FALSE): boolean;
    function sendToAll(const aCmd: WORD): boolean;
    function sendToAllEx(const aCmd: WORD; const pt: TPoint): boolean;
    property HWNDs[index: integer]: HWND read getHWND;
  end;

implementation

uses
  system.sysUtils, system.classes,
  vcl.forms,
  mmpConsts, mmpSingletons,
  _debugWindow;

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

function TSendAll.postToAll(const aCmd: WORD; const aPostToAll: boolean = FALSE): boolean;
begin
  postToAllEx(aCmd, point(0, 0), aPostToAll);
end;

function TSendAll.sendToAll(const aCmd: WORD): boolean;
begin
  sendToAllEx(aCmd, point(0, 0));
end;

function TSendAll.postToAllEx(const aCmd: WORD; const pt: TPoint; const aPostToAll: boolean = FALSE): boolean;
var
  i: integer;
begin
  case aPostToAll of FALSE: begin postMessage(GV.appWnd, aCmd, pt.x, pt.y);
                                  EXIT; end;end;

  clear;
  enumWindows(@enumAllWindows, 0);

  for i := low(FHWNDs) to high(FHWNDs) do
    postMessage(FHWNDs[i], aCmd, pt.x, pt.y);

  clear;
  result := TRUE;
end;

function TSendAll.sendToAllEx(const aCmd: WORD; const pt: TPoint): boolean;
var
  i: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);

  for i := low(FHWNDs) to high(FHWNDs) do
    sendMessage(FHWNDs[i], aCmd, pt.x, pt.y);

  clear;
  result := TRUE;
end;

end.
