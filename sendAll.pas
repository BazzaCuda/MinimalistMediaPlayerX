unit sendAll;

interface

uses winAPI.windows;

type
  TSendAll = class(TObject)
  strict private
  constructor Create;
  destructor  Destroy;  override;
  protected
  private
  public
    function sendToAll(cmd: WORD): boolean;
  end;

function SA: TSendAll;

implementation

uses
  system.sysUtils, consts;

var
  gSendAll: TSendAll;
  FHWNDs: array of HWND;

function SA: TSendAll;
begin
  case gSendAll = NIL of TRUE: gSendAll := TSendAll.Create; end;
  result := gSendAll;
end;

{ TSendAll }

constructor TSendAll.Create;
begin
  inherited;
  setLength(FHWNDs, 0);
end;

destructor TSendAll.Destroy;
begin

  inherited;
end;

function enumAllWindows(handle: HWND; lparam: LPARAM): BOOL; stdcall;
var
  windowName : array[0..255] of char;
  className  : array[0..255] of char;
begin
  case getClassName(handle, className, sizeOf(className)) > 0 of TRUE:
    case strComp(className, 'TMMPUI') = 0 of TRUE: begin setLength(FHWNDs, length(FHWNDs) + 1);
                                                         FHWNDs[high(FHWNDs)] := handle; end;end;end;

  result := TRUE;
end;

function TSendAll.sendToAll(cmd: WORD): boolean;
var
  i: integer;
begin
  enumWindows(@enumAllWindows, 0);

  for i := low(FHWNDs) to high(FHWNDs) do
    postMessage(FHWNDs[i], cmd, 0, 0);

  setLength(FHWNDs, 0);
  result := TRUE;
end;

initialization
  gSendAll := NIL;

finalization
  case gSendAll <> NIL of TRUE: gSendAll.free; end;

end.
