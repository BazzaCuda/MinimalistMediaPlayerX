{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit TAppEventsClass;

interface

uses
  winAPI.windows,
  system.sysUtils,
  vcl.appEvnts, vcl.forms;

type
  TAppEvents = class(TObject)
  strict private
    FAppEvents: TApplicationEvents;
  private
    procedure appEventsMessage(var msg: tagMSG; var handled: boolean);
    procedure appException(sender: TObject; e: exception);
  protected
    property appEvents: TApplicationEvents read FAppEvents write FAppEvents;
  public
    constructor create;
    destructor  Destroy; override;
  end;

implementation

uses
  winAPI.messages,
  system.classes, system.types, system.typInfo,
  vcl.controls, vcl.dialogs,
  mmpConsts, mmpDesktopUtils, mmpDialogs, mmpKeyboard, mmpSingletons, mmpSysCommands, mmpUtils,
  formCaptions, formHelp, formMediaCaption, formPlaylist, formThumbs, formTimeline,
  _debugWindow;

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
  GV.autoCentre := FALSE;

  getCursorPos(newMouse);
  dx := newMouse.X - mouseStart.X;
  dy := newMouse.Y - mouseStart.Y;

  getWindowRect(UI.handle, wndRect);

  moveWindow(UI.handle, dx, dy, wndRect.width, wndRect.height, FALSE);
  UI.moveHelpWindow(FALSE);
  UI.movePlaylistWindow(FALSE);
  UI.moveTimelineWindow(FALSE);
end;

procedure TAppEvents.appEventsMessage(var msg: tagMSG; var handled: boolean);
// main event handler for capturing keystrokes
var
  key: word;
  keyDnHandled: boolean;
  sysCommand: TWMSysCommand;

  function msgIs(aMessage: UINT): boolean;
  begin
    result := msg.message = aMessage;
  end;
begin
  keyDnHandled := FALSE;

  var vShiftState   := mmpShiftState;

  case getKeyState(VK_CONTROL) < 0 of TRUE: include(vShiftState, ssCtrl); end;

  focusPlaylist; // if it's being displayed, so it gets keystrokes
  focusTimeLine;

  case GV.showingThumbs of TRUE: EXIT; end;

  case msgIs(WM_KEYDOWN) of TRUE: begin
                                    case GV.userInput of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                    key          := msg.WParam;
                                    handled      := KBProcessKeyStroke(key, vShiftState, kdDn);
                                    keyDnHandled := handled; end;end;

  case msgIs(WM_KEY_UP)  of TRUE: begin
                                    case GV.userInput of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                    key         := msg.WParam; // e.g. VK_F10;
                                    handled     := KBProcessKeyStroke(key, vShiftState, kdUp);
                                    EXIT;       end;end;

  case msgIs(WM_KEYUP) of TRUE: begin
                                  case GV.userInput  of TRUE: EXIT; end; // don't trap keystrokes when the inputBoxForm is being displayed
                                  case keyDnHandled of TRUE: EXIT; end; // Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
                                  case GV.showingTimeline and TL.validKey(msg.WParam) of TRUE: EXIT; end; // Timeline keystrokes take precedence
                                  key         := msg.WParam;
                                  handled     := KBProcessKeyStroke(key, vShiftState, kdUp); end;end;

  case msgIs(WM_SYSCOMMAND) of TRUE:  begin
                                        sysCommand.CmdType := msg.wParam;
                                        mmpDoSysCommand(sysCommand); end;end;

  case msgIs(WM_PROGRESSBAR_CLICK) of TRUE: MP.position := PB.position; end;

  case msgIs(WM_TICK) of TRUE: MP.setProgressBar; end;
  case msgIs(WM_TICK) of TRUE: ST.displayTime := MP.formattedTime + ' / ' + MP.formattedDuration; end;
  case msgIs(WM_TICK) of TRUE: case (screen <> NIL) and NOT GV.userInput and NOT GV.showingPlaylist and ((screen.cursor <> crHandPoint) AND NOT GV.showingTimeline
                                                                                                                                        AND NOT GV.showingThumbs) of TRUE: screen.cursor := crNone; end;end;

  case msg.hwnd = UI.videoPanel.handle of TRUE: begin
    case msgIs(WM_LBUTTONDOWN) and NOT GV.showingPlaylist  of TRUE: begin mouseDown := TRUE; setStartPoint;  end;end;
    case msgIs(WM_LBUTTONDOWN) and (ssCtrl in vShiftState) of TRUE: begin mouseDown := TRUE; setStartPoint;  end;end;
    case msgIs(WM_LBUTTONUP)                               of TRUE:       mouseDown := FALSE; end;
    case mouseDown and msgIs(WM_MOUSEMOVE)                 of TRUE: dragUI; end;
  end;end;

  case msgIs(WM_RBUTTONUP) and (msg.hwnd = UI.videoPanel.handle)                     of TRUE: postMessage(msg.hwnd, WIN_PAUSE_PLAY, 0, 0); end;
  case msgIs(WM_LBUTTONDBLCLK) and NOT GV.showingPlaylist and NOT GV.showingTimeline of TRUE: MP.toggleFullscreen; end;

  case msgIs(WM_PLAY_CURRENT_ITEM)    of TRUE: MP.play(PL.currentItem);  end;
  case msgIs(WM_SHOW_WINDOW)          of TRUE: UI.showWindow; end;

  case msgIs(WIN_AUTOCENTRE_OFF)      of TRUE: GV.autoCentre := FALSE; end;
  case msgIs(WIN_RESTART)             of TRUE: ST.opInfo := MP.startOver; end;
  case msgIs(WIN_CAPTION)             of TRUE: begin UI.showXY; MC.caption := PL.formattedItem; end;end;
  case msgIs(WIN_CLOSEAPP)            of TRUE: begin MP.ceaseOps; mmpSendSysCommandClose; end;end;
  case msgIs(WIN_CONTROLS)            of TRUE: UI.toggleCaptions; end;
  case msgIs(WIN_PAUSE_PLAY)          of TRUE: MP.pausePlay; end;
  case msgIs(WIN_TAB)                 of TRUE: ST.opInfo := MP.tab(KBCapsLock); end;
  case msgIs(WIN_TABTAB)              of TRUE: ST.opInfo := MP.tab(KBCapsLock, -1); end;
  case msgIs(WIN_TABALT)              of TRUE: ST.opInfo := MP.tab(KBCapsLock, 200); end;
  case msgIs(WIN_GREATER)             of TRUE: UI.greaterWindow(UI.handle, vShiftState); end;
  case msgIs(WIN_RESIZE)              of TRUE: UI.resize(UI.handle, point(msg.wParam, msg.lParam), MP.videoWidth, MP.videoHeight); end;
  case msgIs(WIN_SYNC_MEDIA)          of TRUE: begin MP.position := msg.wParam; ST.opInfo := 'Synced'; end;end;

  case msgIs(WM_USER_CENTRE_WINDOW)   of TRUE: UI.centreWindow(UI.handle); end;
  case msgIs(WM_PROCESS_MESSAGES)     of TRUE: mmpProcessMessages; end;

  case msgIs(WM_CENTRE_CURSOR)        of TRUE: PB.centreCursor; end;
end;

procedure TAppEvents.appException(sender: TObject; e: exception);
  function splitMessage: string;
  begin
    result := e.message;
    var vPos := pos('. ', result);
    case vPos > 0 of TRUE: result := copy(result, 1, vPos) + #13#10 + copy(result, vPos + 2, length(result)); end;
  end;
begin
  var vMsg := 'Well that''s unfortunate.'#13#10#13#10;
  vMsg := vMsg + format('%s%s', [splitMessage, #13#10#13#10]);
  vMsg := vMsg + format('class: %s%s', [sender.toString, #13#10]);
  vMsg := vMsg + format('address: $%p%s', [exceptAddr, #13#10]);
  mmpShowOKCancelMsgDlg(vMsg, TMsgDlgType.mtInformation, [mbOK],  mbOK);
  mmpSendSysCommandClose; // attempt an elegant withdrawal
  halt;                   // if not
end;

constructor TAppEvents.create;
begin
  inherited;
  FAppEvents              := TApplicationEvents.create(NIL);
  FAppEvents.onMessage    := appEventsMessage;
  FAppEvents.onException  := appException;
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
