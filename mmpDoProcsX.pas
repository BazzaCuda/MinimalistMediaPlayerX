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
unit mmpDoProcsX;

interface

uses
  winApi.messages,
  system.classes, system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

//===== TProcs
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc;          const falseProc: TProc                            ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TProc                                                             ): boolean; overload;

//===== TOProcs with a TObject parameter
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;         const falseProc: TOProc;  const aObject: TObject  ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueProc:     TOProc;                                   const aObject: TObject  ): boolean; overload;

//===== TBFuncs which return a boolean result
function mmpDo(const aBoolean: boolean;     const trueFunc:     TBFunc;         const falseFunc: TBFunc                           ): boolean; overload;
function mmpDo(const aBoolean: boolean;     const trueFunc:     TBFunc                                                            ): boolean; overload;

//===== TSFuncs which return a string result
function mmpDo(const aBoolean: boolean;     const trueFunc: TSFunc;             const falseFunc: TSFunc                           ): string; overload;
function mmpDo(const aBoolean: boolean;     const trueFunc: TSFunc                                                                ): string; overload;

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
begin
  result := mmpDo(aBoolean, trueProc, falseProc);
end;

function mmpDo(const aBoolean: boolean; const trueProc: TProc): boolean;
begin
  result := mmpDo(aBoolean, trueProc);
end;

//===== TOProcs with a TObject parameter
function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): boolean;
begin
  result := mmpDo(aBoolean, trueProc, aObject);
end;

function mmpDo(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): boolean;
begin
  result := mmpDo(aBoolean, trueProc, falseProc, aObject);
end;

//===== TRFuncs which return a boolean result
function mmpDo(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc): boolean;
begin
  result := mmpDo(aBoolean, trueFunc, falseFunc);
end;

function mmpDo(const aBoolean: boolean; const trueFunc: TBFunc): boolean;
begin
  result := mmpDo(aBoolean, trueFunc);
end;

//===== TSFuncs which return a string result
function mmpDo(const aBoolean: boolean; const trueFunc: TSFunc; const falseFunc: TSFunc): string;
begin
  result := mmpDo(aBoolean, trueFunc, falseFunc);
end;

function mmpDo(const aBoolean: boolean; const trueFunc: TSFunc): string;
begin
  result := mmpDo(aBoolean, trueFunc);
end;

//===== Event Notices with no result
function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): boolean;
begin
  result := mmpDo(aBoolean, trueEvent, falseEvent);
end;

function mmpDo(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): boolean;
begin
  result := mmpDo(aBoolean, trueNotices);
end;

//===== Event Notices with the INotice returned
function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice;
begin
  result := mmpDo(aBoolean, trueEvent);
end;

function mmpDo(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := mmpDo(aBoolean, trueEvent, aInteger);
end;

//===== Event Notices with the INotice returned (no boolean test)
function mmpDo(const aEvent: TNoticeEvent): INotice;
begin
  result := mmpDo(aEvent);
end;

function mmpDo(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice;
begin
  result := mmpDo(aEvent, aBoolean);
end;

function mmpDo(const aEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := mmpDo(aEvent, aInteger);
end;

function mmpDo(const aEvent: TNoticeEvent; const aString: string): INotice;
begin
  result := mmpDo(aEvent, aString);
end;

function mmpDo(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice;
begin
  result := mmpDo(aEvent, aText, aMediaType);
end;

function mmpDo(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice;
begin
  result := mmpDo(aEvent, aWndRec);
end;

function mmpDo(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice;
begin
  result := mmpDo(aEvent, aComponent);
end;

function mmpDo(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice;
begin
  result := mmpDo(aEvent, aMediaType);
end;

function mmpDo(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice;
begin
  result := mmpDo(aEvent, aMsg);
end;

function mmpDo(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice;
begin
  result := mmpDo(aEvent, aShiftState);
end;
//===== Misc

procedure mmpFree(const aBoolean: boolean; const aObject: TObject);
begin
  mmp.free(aBoolean, aObject);
end;

procedure mmpDo(const aBoolean: boolean; const aProcVar: TProcVar);
begin
  mmpDo(aBoolean, aProcVar);
end;

end.
