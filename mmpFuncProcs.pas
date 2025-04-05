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
unit mmpFuncProcs;

interface

uses
  winApi.messages,
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

const
  doNowt = NIL;

type
  void    = boolean;
  TOProc  = TProc<TObject>;
  TRFunc  = TFunc<boolean>;
  TProcVar = procedure;

var
  T:      TProc;
  F:      TProc;

//===== TProcs
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc;          const falseProc: TProc                            ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc                                                             ): boolean; overload;

//===== TOProcs with a TObject parameter
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;                                   const aObject: TObject  ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;         const falseProc: TOProc;  const aObject: TObject  ): boolean; overload;

//===== TRFuncs which return a boolean result
function mmpDo(const aBoolean: boolean;     const trueFunc:     TRFunc;         const falseFunc: TRFunc                           ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueFunc:     TRFunc                                                            ): boolean; overload;

//===== Event Notices with no result
function mmpDo(const aBoolean: boolean;     const trueEvent:    TNoticeEvent;   const falseEvent: TNoticeEvent                    ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueNotices:  array of TNoticeEvent                                             ): boolean; overload;

//===== Event Notices with the INotice returned
function mmpDo(const aBoolean: boolean;     const trueEvent:    TNoticeEvent                                                      ): INotice; overload;
function mmpDo(const aBoolean: boolean;     const trueEvent:    TNoticeEvent;                             const aInteger: integer ): INotice; overload;

//===== Event Notices with the INotice returned (no boolean test)
function mmpDo(const aEvent: TNoticeEvent                                                                                         ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aBoolean:     boolean                                                           ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aInteger:     integer                                                           ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aString:      string                                                            ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aText:        string; const aMediaType: TMediaType                              ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aWndRec:      TWndRec                                                           ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aComponent:   TComponent                                                        ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aMediaType:   TMediaType                                                        ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aMsg:         TMessage                                                          ): INotice; overload;
function mmpDo(const aEvent: TNoticeEvent;  const aShiftState:  TShiftState                                                       ): INotice; overload;


//===== Misc
procedure mmpFree(const aBoolean: boolean;  const aObject: TObject);
procedure mmpDo(const aBoolean: boolean; const aProcVar: TProcVar); overload;

implementation

//===== TProcs

function mmpDo(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): boolean;
var doProc: array[boolean] of TProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case doProc[aBoolean] = NIL of FALSE: doProc[aBoolean](); end;
end;

function mmpDo(const aBoolean: boolean; const trueProc: TProc): boolean; overload;
begin
  mmpDo(aBoolean, trueProc, NIL);
end;

//===== TOProcs with a TObject parameter

function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): boolean;
var doProc: array[boolean] of TOProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case doProc[aBoolean] = NIL of FALSE: doProc[aBoolean](aObject); end;
end;

function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): boolean;
begin
  mmpDo(aBoolean, trueProc, NIL, aObject);
end;

//===== TRFuncs which return a boolean result

function mmpDo(const aBoolean: boolean; const trueFunc: TRFunc; const falseFunc: TRFunc): boolean;
var doFunc: array[boolean] of TRFunc;
begin
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;
  case doFunc[aBoolean] = NIL of FALSE: result := doFunc[aBoolean](); end;
end;

function mmpDo(const aBoolean: boolean; const trueFunc: TRFunc): boolean;
begin
  result := mmpDo(aBoolean, trueFunc, NIL);
end;

//===== Event Notices with no result

function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): boolean;
begin
  T :=  procedure begin notifyApp(newNotice(trueEvent)); end;
  F :=  procedure begin notifyApp(newNotice(falseEvent)); end;
  mmpDo(aBoolean, T, F);
end;

function mmpDo(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): boolean;
begin
  for var i := low(trueNotices) to high(trueNotices) do begin
    var vNotice := trueNotices[i];
    T := procedure begin notifyApp(newNotice(vNotice)); end;
    mmpDo(aBoolean, T, NIL);
  end;
end;

//===== Event Notices with the INotice returned

function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice;
begin
  result := NIL;
  case aBoolean of TRUE: result := notifyApp(newNotice(trueEvent)); end;
end;

function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := NIL;
  case aBoolean of TRUE: result := notifyApp(newNotice(trueEvent, aInteger)); end;
end;

//===== Event Notices with the INotice returned (no boolean test)

function mmpDo(const aEvent: TNoticeEvent): INotice;
begin
  result := notifyApp(newNotice(aEvent));
end;

function mmpDo(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice;
begin
  result := notifyApp(newNotice(aEvent, aBoolean));
end;

function mmpDo(const aEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := notifyApp(newNotice(aEvent, aInteger));
end;

function mmpDo(const aEvent: TNoticeEvent; const aString: string): INotice;
begin
  result := notifyApp(newNotice(aEvent, aString));
end;

function mmpDo(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice;
begin
  result := notifyApp(newNotice(aEvent, aText, aMediaType));
end;

function mmpDo(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice;
begin
  result := notifyApp(newNotice(aEvent, aWndRec));
end;

function mmpDo(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice;
begin
  result := notifyApp(newNotice(aEvent, aComponent));
end;

function mmpDo(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice;
begin
  result := notifyApp(newNotice(aEvent, aMediaType));
end;

function mmpDo(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice;
begin
  result := notifyApp(newNotice(aEvent, aMsg));
end;

function mmpDo(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice;
begin
  result := notifyApp(newNotice(aEvent, aShiftState));
end;

//===== Misc

procedure mmpFree(const aBoolean: boolean; const aObject: TObject);
begin
  mmpDo(aBoolean and (aObject <> NIL), procedure begin aObject.free; end);
end;

procedure mmpDo(const aBoolean: boolean; const aProcVar: TProcVar);
begin
  aProcVar();
end;

end.
