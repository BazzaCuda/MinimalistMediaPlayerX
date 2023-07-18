unit tickTimer;

interface

uses
  vcl.extCtrls;

type
  TTickTimer = class(TTimer)
  private
    constructor create;
    procedure timerEvent(aSender: TObject);
  end;

implementation

uses
  globalVars, consts, winApi.windows;

var
  gTT: TTickTimer;

{ TTickTimer }

constructor TTickTimer.create;
begin
  inherited create(NIL);
  interval := 999;
  onTimer := timerEvent;
end;

procedure TTickTimer.timerEvent(aSender: TObject);
begin
  postMessage(GV.mainWnd, WM_TICK, 0, 0);
end;

initialization
   gTT := TTickTimer.create;

finalization
  case gTT <> NIL of TRUE: gTT.free; end;

end.
