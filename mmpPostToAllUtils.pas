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
unit mmpPostToAllUtils;

interface

uses
  winApi.windows;

type
  IPostToAll = interface
    ['{CBB88DF1-32F6-4669-8F3A-CCE148884AF9}']
  end;

  TWnds = array of HWND;

function mmpHWND(index: integer): HWND;

implementation

uses
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpKeyboardUtils,
  viewModel.mmpGlobalState,
  _debugWindow;

  type TPostToAll = class(TInterfacedObject, IPostToAll)
  private
    function onNotify(const aNotice: INotice): INotice;
  protected
    constructor create;
    function notify(const aNotice: INotice): INotice;
  end;

var
  gPA: IPostToAll;
  HWNDs: TWnds;

function PA: IPostToAll;
begin
  case gPA = NIL of TRUE: gPA := TPostToAll.create; end;
  result := gPA;
end;

function addWnd(const aHWND: HWND): boolean;
begin
  setLength(HWNDs, length(HWNDs) + 1);
  HWNDs[high(HWNDs)] := aHWND;
  result := TRUE;
end;

function clear: boolean;
begin
  setLength(HWNDs, 0);
  result := TRUE;
end;

function enumAllWindows(handle: HWND; lparam: LPARAM): BOOL; stdcall;
var
  windowName : array[0..255] of char;
  className  : array[0..255] of char;
begin
  case getClassName(handle, className, sizeOf(className)) > 0 of TRUE:
    case strComp(className, 'TMMPUI') = 0 of TRUE: addWnd(handle); end;end;

  result := TRUE;
end;

function mmpPostToAllCount: integer;
begin
  clear;
  enumWindows(@enumAllWindows, 0);
  result := length(HWNDs);
end;

function mmpPostToAllEx(const aCmd: WORD; const WParam: NativeUInt; const LParam: NativeInt; const bPostToAll: boolean = FALSE): boolean;
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

  for i := low(HWNDs) to high(HWNDs) do
    postMessage(HWNDs[i], aCmd, WParam, LParam);

  clear;
  result := TRUE;
end;

function mmpHWND(index: integer): HWND;
begin
  result := HWNDS[index - 1]; // for simplicity in mmpWindowUtils.mmpArrangeAll, index parameter is 1-based
end;

{ TPostToAll }

constructor TPostToAll.create;
begin
  inherited;
  appNotifier.subscribe(newSubscriber(onNotify));
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
    evPAPostToAll:      mmpPostToAllEx(aNotice.integer, 0, 0, mmpNumLockOn);
    evPAPostToAllEx:    mmpPostToAllEx(aNotice.msg.msg, aNotice.msg.WParam, aNotice.msg.LParam, mmpNumLockOn);
    evPAPostToEvery:    mmpPostToAllEx(aNotice.integer, 0, 0, TRUE); // Chambers Thesaurus was no help at all!
    evPAPostToEveryEx:  mmpPostToAllEx(aNotice.msg.msg, aNotice.msg.WParam, aNotice.msg.LParam, TRUE);
    evPAReqCount:       aNotice.integer := mmpPostToAllCount;
  end;
end;

initialization
  gPA := NIL;
  PA; // create a subscription to appNotifier

end.
