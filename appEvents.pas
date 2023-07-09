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
  private
    procedure appEventsMessage(var msg: tagMSG; var handled: boolean);
  protected
    constructor create;
    destructor  Destroy; override;
    property appEvents: TApplicationEvents read FAppEvents write FAppEvents;
  end;

implementation

uses
  system.classes, winAPI.messages, sysCommands, globalVars, system.sysUtils, keyboard, _debugWindow;

var gAE: TAppEvents;

{ TAppEvents }

procedure TAppEvents.appEventsMessage(var msg: tagMSG; var handled: boolean);
// main event handler for capturing keystrokes
var
  key: word;
  shiftState: TShiftState;
begin
  case msg.message = WM_KEYDOWN of TRUE:  begin
                                            shiftState  := KeyboardStateToShiftState;
                                            key         := msg.WParam;
                                            KB.processKeyStroke(key, shiftState, kdDown);
                                            handled     := TRUE;
                                          end;end;

  case msg.message = WM_KEYUP of TRUE:    begin
                                            case key = 0 of TRUE: EXIT; end; // Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
                                            shiftState  := KeyboardStateToShiftState;
                                            key         := msg.WParam;
                                            KB.processKeyStroke(key, shiftState, kdUp);
                                            handled     := TRUE;
                                          end;end;
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
