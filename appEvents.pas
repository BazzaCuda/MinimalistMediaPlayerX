{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit appEvents;

interface

uses
  vcl.appEvnts, winAPI.windows, vcl.forms;

type
  TAppEvents = class(TObject)
  strict private
    FAppEvents: TApplicationEvents;
    FDragging: boolean;
  private
    procedure appEventsMessage(var msg: tagMSG; var handled: boolean);
  protected
    property appEvents: TApplicationEvents read FAppEvents write FAppEvents;
  public
    constructor create;
    destructor  Destroy; override;
  end;

implementation

uses
  system.classes, winAPI.messages, sysCommands, globalVars, system.sysUtils, keyboard, formSubtitles, consts, progressBar, mediaPlayer, _debugWindow;

var gAE: TAppEvents;

{ TAppEvents }

var lastLocation: TPoint;

procedure setLastLocation;
var
  vRect: TRect;
begin
  getCursorPos(lastLocation);
  getWindowRect(GV.UIWnd, vRect);
  lastLocation.x := lastLocation.x - vRect.left;
  lastLocation.y := lastLocation.y - vRect.top;
end;

procedure dragUI;
var
  currentPos: TPoint;
  x, y: integer;
begin
  getCursorPos(currentPos);
  x := currentpos.x - lastLocation.x;
  y := currentpos.y - lastLocation.y;
  MoveWindow(GV.UIWnd, x, y, GV.mainForm.width, GV.mainForm.height, FALSE);
end;

procedure TAppEvents.appEventsMessage(var msg: tagMSG; var handled: boolean);
// main event handler for capturing keystrokes
var
  key: word;
  shiftState: TShiftState;
  keyDnHandled: boolean;
  sysCommand: TWMSysCommand;

  function msgIs(aMessage: UINT): boolean;
  begin
    result := msg.message = aMessage;
  end;
begin
  keyDnHandled := FALSE;
  case msgIs(WM_KEYDOWN) of TRUE: begin
                                    shiftState   := KeyboardStateToShiftState;
                                    key          := msg.WParam;
                                    handled      := KB.processKeyStroke(key, shiftState, kdDown);
                                    keyDnHandled := handled; end;end;

  case msgIs(WM_KEYUP) of TRUE: begin
                                  case keyDnHandled of TRUE: EXIT; end; // Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
                                  shiftState  := KeyboardStateToShiftState;
                                  key         := msg.WParam;
                                  handled     := KB.processKeyStroke(key, shiftState, kdUp); end;end;

  case msgIs(WM_TIMERUPDATE) of TRUE: MP.setProgressBar; end;

  case msgIs(WM_SYSCOMMAND) of TRUE:  begin
                                        sysCommand.CmdType := msg.wParam;
                                        doSysCommand(sysCommand); end;end;

  case msgIs(WM_PROGRESSBAR_CLICK) of TRUE: MP.position := PB.position; end;

  case msgIs(WM_TIMEDTEXTNOTIFY) of TRUE: ST.subTitle := MP.subTitle; end;

  case msgIs(WM_TICK) of TRUE: ST.displayTime := MP.formattedTime + ' / ' + MP.formattedDuration; end;

  case msgIs(WM_LBUTTONDOWN) of TRUE: begin GV.mouseDown := TRUE; setLastLocation;  end;end;
  case msgIs(WM_LBUTTONUP)   of TRUE: GV.mouseDown := FALSE; end;
  case GV.mouseDown and msgIs(WM_MOUSEMOVE) and NOT PB.inClientArea(msg.pt.y) of TRUE: dragUI; end;
end;

constructor TAppEvents.create;
begin
  inherited;
  FAppEvents := TApplicationEvents.create(NIL);
  FAppEvents.onMessage := appEventsMessage;
end;

destructor TAppEvents.Destroy;
begin
  case FAppEvents <> NIL of TRUE: FAppEvents.free; end;
  inherited;
end;

initialization
  gAE := TAppEvents.create;

finalization
  case gAE <> NIL of TRUE: gAE.free; end;

end.
