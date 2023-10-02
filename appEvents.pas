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
    property appEvents: TApplicationEvents read FAppEvents write FAppEvents;
  public
    constructor create;
    destructor  Destroy; override;
  end;

implementation

uses
  system.classes, winAPI.messages, sysCommands, globalVars, system.sysUtils, keyboard, formSubtitles, consts, progressBar, mediaPlayer,
  UICtrls, commonUtils, vcl.controls, formHelp, playlist, formCaption, formPlaylist, _debugWindow;

var gAE: TAppEvents;

{ TAppEvents }

// Mouse Drag Control
var
  mouseDown: Boolean;
  mouseStart: TPoint;

procedure setStartPoint;
var
  wndRect: TRect;
begin
  GetCursorPos(mouseStart);
  getWindowRect(UI.handle, wndRect);
  mouseStart.X := mouseStart.X - wndRect.left;
  mouseStart.Y := mouseStart.Y - wndRect.top;
end;

procedure dragUI;
var
  newMouse: TPoint;
  wndRect: TRect;
  dx, dy: integer;
begin
  getCursorPos(newMouse);
  dx := newMouse.X - mouseStart.X;
  dy := newMouse.Y - mouseStart.Y;

  getWindowRect(UI.handle, wndRect);

  moveWindow(UI.handle, dx, dy, wndRect.right - wndRect.left, wndRect.bottom - wndRect.top, FALSE);
  UI.moveHelpWindow(FALSE);
  UI.movePlaylistWindow(FALSE);
  UI.autoCentre := FALSE;
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

  shiftState   := KeyboardStateToShiftState;
  case getKeyState(VK_CONTROL) < 0 of TRUE: include(shiftState, ssCtrl); end;

  focusPlaylist; // if it's being displayed, so it keys keystrokes

  case msgIs(WM_KEYDOWN) of TRUE: begin
                                    case GV.userInput of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                    key          := msg.WParam;
                                    handled      := KB.processKeyStroke(key, shiftState, kdDown);
                                    keyDnHandled := handled; end;end;


  case msgIs(WM_KEY_UP)  of TRUE: begin
                                    case GV.userInput of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                    key         := msg.WParam; // e.g. VK_F10;
                                    handled     := KB.processKeyStroke(key, shiftState, kdUp);
                                    EXIT;       end;end;

  case msgIs(WM_KEYUP) of TRUE: begin
                                  case GV.userInput  of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                  case keyDnHandled of TRUE: EXIT; end; // Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
                                  key         := msg.WParam;
                                  handled     := KB.processKeyStroke(key, shiftState, kdUp); end;end;

  case msgIs(WM_SYSCOMMAND) of TRUE:  begin
                                        sysCommand.CmdType := msg.wParam;
                                        doSysCommand(sysCommand); end;end;

  case msgIs(WM_PROGRESSBAR_CLICK) of TRUE: MP.position := PB.position; end;

  case msgIs(WM_TICK) of TRUE: MP.setProgressBar; end;
  case msgIs(WM_TICK) of TRUE: ST.displayTime := MP.formattedTime + ' / ' + MP.formattedDuration; end;
  case msgIs(WM_TICK) of TRUE: case (screen <> NIL) and NOT GV.userInput and NOT showingPlaylist and (screen.cursor <> crHandPoint) of TRUE: screen.cursor := crNone; end;end;

  case msgIs(WM_LBUTTONDOWN)             of TRUE: begin mouseDown := TRUE; setStartPoint;  end;end;
  case msgIs(WM_LBUTTONUP)               of TRUE: mouseDown := FALSE; end;
  case mouseDown and msgIs(WM_MOUSEMOVE) of TRUE: dragUI; end;

  case msgIs(WM_RBUTTONUP)               of TRUE: MP.pausePlay; end;
  case msgIs(WM_LBUTTONDBLCLK) and NOT showingPlaylist of TRUE: MP.toggleFullscreen; end;

  // these four messages trigger each other in a loop until the video fits on the screen
  case msgIs(WM_ADJUST_ASPECT_RATIO)  of TRUE: begin CU.delay(1000); UI.adjustAspectRatio(UI.handle, MP.videoWidth, MP.videoHeight); end;end; // the delay is vital!
  case msgIs(WM_CENTRE_WINDOW)        of TRUE: UI.centreWindow(UI.handle); end;
  case msgIs(WM_CHECK_SCREEN_LIMITS)  of TRUE: UI.checkScreenLimits(UI.handle, CU.getScreenWidth, CU.getScreenHeight); end;
  case msgIs(WM_SMALLER_WINDOW)       of TRUE: UI.smallerWindow(UI.handle); end;

  case msgIs(WM_PLAY_CURRENT_ITEM)    of TRUE: MP.play(PL.currentItem);  end;
  case msgIs(WM_SHOW_WINDOW)          of TRUE: UI.showWindow; end;

  case msgIs(WIN_RESTART)             of TRUE: ST.opInfo := MP.startOver; end;
  case msgIs(WIN_CAPTION)             of TRUE: MC.caption := PL.formattedItem; end;
  case msgIs(WIN_CLOSEAPP)            of TRUE: begin MP.dontPlayNext := TRUE; MP.stop; sendSysCommandClose(UI.handle); end;end;
  case msgIs(WIN_CONTROLS)            of TRUE: UI.toggleCaptions(shiftState); end;
  case msgIs(WIN_PAUSE_PLAY)          of TRUE: MP.pausePlay; end;
  case msgIs(WIN_TAB)                 of TRUE: ST.opInfo := MP.tab(shiftState, KB.capsLock); end;
  case msgIs(WIN_TABTAB)              of TRUE: ST.opInfo := MP.tab(shiftState, KB.capsLock, -1); end;
  case msgIs(WIN_TABALT)              of TRUE: ST.opInfo := MP.tab(shiftState, KB.capsLock, 200); end;
  case msgIs(WIN_GREATER)             of TRUE: UI.greaterWindow(UI.handle, shiftState); end;
  case msgIs(WIN_RESIZE)              of TRUE: UI.resize(UI.handle, point(msg.wParam, msg.lParam), MP.videoWidth, MP.videoHeight); end;
  case msgIs(WIN_AUTOCENTER_OFF)      of TRUE: UI.autoCentre := FALSE; end;
  case msgIs(WIN_SYNC_MEDIA)          of TRUE: begin MP.position := msg.wParam; ST.opInfo := 'Synced'; end;end;
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
