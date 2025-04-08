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
unit mmpDoProcs;

interface

uses
  winApi.messages,
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpFuncProg;

//===== TProcs
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc;          const falseProc: TProc                            ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc                                                             ): boolean; overload;

//===== TOProcs with a TObject parameter
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;         const falseProc: TOProc;  const aObject: TObject  ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;                                   const aObject: TObject  ): boolean; overload;

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
function mmpDo(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueProc, falseProc);
end;

function mmpDo(const aBoolean: boolean; const trueProc: TProc): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueProc);
end;

//===== TOProcs with a TObject parameter
function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueProc, aObject);
end;

function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueProc, falseProc, aObject);
end;

//===== TRFuncs which return a boolean result
function mmpDo(const aBoolean: boolean; const trueFunc: TRFunc; const falseFunc: TRFunc): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueFunc, falseFunc);
end;

function mmpDo(const aBoolean: boolean; const trueFunc: TRFunc): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueFunc);
end;

//===== Event Notices with no result
function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueEvent, falseEvent);
end;

function mmpDo(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): boolean; overload;
begin
  result := mmp.cmd(aBoolean, trueNotices);
end;

//===== Event Notices with the INotice returned
function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice; overload;
begin
  result := mmp.cmd(aBoolean, trueEvent);
end;

function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice; overload;
begin
  result := mmp.cmd(aBoolean, trueEvent, aInteger);
end;

//===== Event Notices with the INotice returned (no boolean test)
function mmpDo(const aEvent: TNoticeEvent): INotice; overload;
begin
  result := mmp.cmd(aEvent);
end;

function mmpDo(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice; overload;
begin
  result := mmp.cmd(aEvent, aBoolean);
end;

function mmpDo(const aEvent: TNoticeEvent; const aInteger: integer): INotice; overload;
begin
  result := mmp.cmd(aEvent, aInteger);
end;

function mmpDo(const aEvent: TNoticeEvent; const aString: string): INotice; overload;
begin
  result := mmp.cmd(aEvent, aString);
end;

function mmpDo(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice; overload;
begin
  result := mmp.cmd(aEvent, aText, aMediaType);
end;

function mmpDo(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice; overload;
begin
  result := mmp.cmd(aEvent, aWndRec);
end;

function mmpDo(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice; overload;
begin
  result := mmp.cmd(aEvent, aComponent);
end;

function mmpDo(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice; overload;
begin
  result := mmp.cmd(aEvent, aMediaType);
end;

function mmpDo(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice; overload;
begin
  result := mmp.cmd(aEvent, aMsg);
end;

function mmpDo(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice; overload;
begin
  result := mmp.cmd(aEvent, aShiftState);
end;
//===== Misc

procedure mmpFree(const aBoolean: boolean; const aObject: TObject);
begin
  mmp.free(aBoolean, aObject);
end;

procedure mmpDo(const aBoolean: boolean; const aProcVar: TProcVar);
begin
  mmp.cmd(aBoolean, aProcVar);
end;

end.
