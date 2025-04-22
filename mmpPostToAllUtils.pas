{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
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
unit mmpPostToAllUtils;

interface

uses
  winApi.windows;

type
  IPostToAll = interface
    ['{CBB88DF1-32F6-4669-8F3A-CCE148884AF9}']
    function  addWnd(const aHWND: HWND): boolean;
    function  getHWND(ix: integer): HWND;
    property  HWND[ix: integer]:    HWND read getHWND; default;
  end;

function PA: IPostToAll;

implementation

uses
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpGlobalState, mmpKeyboardUtils,
  _debugWindow;

type
  TPostToAll = class(TInterfacedObject, IPostToAll)
  strict private
    FSubscriber:  ISubscriber;
    FWNDs:        array of HWND;
  private
    function    clear: boolean;
    function    onNotify(const aNotice: INotice): INotice;
    function    postToAllCount: integer;
    function    postToAllEx(const aCmd: WORD; const WParam: NativeUInt; const LParam: NativeInt; const bPostToAll: boolean = FALSE): boolean;
  public
    constructor create;
    destructor  Destroy; override;

    function    addWnd(const aHWND: HWND): boolean;
    function    getHWND(ix: integer): HWND;
    function    notify(const aNotice: INotice): INotice;
  end;

function PA: IPostToAll;
{$J+} const gPA: IPostToAll = NIL; {$J-}
begin
  case gPA = NIL of TRUE: gPA := TPostToAll.create; end;
  result := gPA;
end;

function enumAllWindows(handle: HWND; LParam: LPARAM): BOOL; stdcall;
var
  windowName : array[0..255] of char;
  className  : array[0..255] of char;
begin
  case getClassName(handle, className, sizeOf(className)) > 0
    of TRUE: case strComp(className, 'TMMPUI') = 0 of TRUE: PA.addWnd(handle); end;end;
  result := TRUE;
end;

{ TPostToAll }

function TPostToAll.addWnd(const aHWND: HWND): boolean;
begin
  setLength(FWNDs, length(FWNDs) + 1);
  FWNDs[high(FWNDs)] := aHWND;
  result := TRUE;
end;

function TPostToAll.clear: boolean;
begin
  setLength(FWNDs, 0);
  result := TRUE;
end;

constructor TPostToAll.create;
begin
  inherited;
  FSubscriber := appEvents.subscribe(newSubscriber(onNotify));
end;

destructor TPostToAll.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
  inherited;
end;

function TPostToAll.getHWND(ix: integer): HWND;
begin
  result := FWNDs[ix - 1];  // for simplicity in mmpWindowUtils.mmpArrangeAll, the ix parameter is 1-based
end;

function TPostToAll.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TPostToAll.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evPAPostToAll:      postToAllEx(aNotice.integer, 0, 0, mmpNumLockOn);
    evPAPostToAllEx:    postToAllEx(aNotice.msg.msg, aNotice.msg.WParam, aNotice.msg.LParam, mmpNumLockOn);
    evPAPostToEvery:    postToAllEx(aNotice.integer, 0, 0, TRUE); // Chambers Thesaurus was no help at all!
    evPAPostToEveryEx:  postToAllEx(aNotice.msg.msg, aNotice.msg.WParam, aNotice.msg.LParam, TRUE);
    evPAReqCount:       aNotice.integer := postToAllCount;
  end;
end;

function TPostToAll.postToAllCount: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);
  result := length(FWNDs);
end;

function TPostToAll.postToAllEx(const aCmd: WORD; const WParam: NativeUInt; const LParam: NativeInt; const bPostToAll: boolean = FALSE): boolean;
var
  i: integer;
begin
  result := FALSE;
  case bPostToAll of FALSE: begin
                              case GS.mainForm = NIL of TRUE: EXIT; end;
                              postMessage(GS.mainForm.handle, aCmd, WParam, LParam);
                              EXIT;
                            end;end;

  clear;
  enumWindows(@enumAllWindows, 0);

  for i := low(FWNDs) to high(FWNDs) do
    postMessage(FWNDs[i], aCmd, WParam, LParam);

  clear;
  result := TRUE;
end;

initialization
  PA; // create a subscription to appNotifier

end.
