{   bazLib / bazCmd
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/bazLib

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
unit bazCmd;

interface

uses
  winApi.messages,
  system.classes, system.sysUtils,
  bazAction, bazFuncDefs,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  TOProc      = reference to procedure(aObject: TObject);
  TBFunc      = reference to function: boolean;
  TSFunc      = reference to function: string;
  TSFuncS     = reference to function(const aString: string): string;
  TVoidSFunc  = reference to function(const aString: string): TVoid;
  TVoidFunc   = reference to function: TVoid;
  TProcVar    = reference to procedure;
  TRefProc    = procedure(const [ref] Obj: TObject);
  PObject     = ^TObject;

type
  mmp = record
    class function use<T>(const aBoolean: boolean; const aTrueValue: T; aFalseValue: T): T; overload; static;
    class function use<T>(const aBoolean: boolean; const aTrueFunc: TFunc<T>; aFalseValue: T): T; overload; static;
    class function use<T>(const aBoolean: boolean; const aTrueFunc: TFunc<T>; const aFalseFunc: TFunc<T>): T; overload; static;

    //===== TVoidFuncs which return a TVoid result
    class function cmd(const aBoolean: boolean; const trueFunc: TVoidFunc; const falseFunc: TVoidFunc): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TVoidFunc): TVoid; overload; static;

    //===== TProcs
    class function cmd(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueProc: TProc): TVoid; overload; static;

    //===== TOProcs with a TObject parameter
    class function cmd(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueProc: TRefProc; const aObject: PObject): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): TVoid; overload; static;

    //===== TBFuncs which return a boolean result
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc): boolean; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc): boolean; overload; static;

    //===== TVoidSFuncs which take a string and return a TVoid result
    class function cmd(const aBoolean: boolean; const trueFunc: TVoidSFunc; const aString: string): TVoid; overload; static;

    //===== TSFuncs which return a string result
    class function cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseFunc: TSFunc): string; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TSFunc): string; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseValue: string): string; overload; static;

    //===== TSFuncSs which take a string and return a string result
    class function cmd(const aBoolean: boolean; const trueFunc: TSFuncS; const aString: string): string; overload; static;

    //===== Event Notices with no result
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): TVoid; overload; static;

    //===== Event Notices with the INotice returned
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice; overload; static;
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice; overload; static;

    //===== Event Notices with the INotice returned (no boolean test)
    class function cmd(const aEvent: TNoticeEvent): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aInteger: integer): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aString: string): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice; overload; static;
    class function cmd(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice; overload; static;

    //===== Misc
    class procedure free(const aBoolean: boolean; const aObject: TObject); static;
   end;

   procedure doNowt;

var
  guardClause: boolean;

implementation

uses
  _debugWindow;

{ Global Null Methods }

procedure doNowt;
begin
end;

function nullBFunc: boolean;
begin
  result := FALSE;
end;

procedure nullOProc(aObject: TObject);
begin
end;

procedure nullProc;
begin
end;

function nullSFunc: string;
begin
  result := EMPTY;
end;

function nullSFuncS(const aString: string): string;
begin
  result := EMPTY;
end;

function nullVoidSFunc(const aString: string): TVoid;
begin
  result := default(TVoid);
end;

function nullVoidFunc: TVoid;
begin
  result := default(TVoid);
end;

{ global methods }

procedure freeObject(aObject: TObject);
begin
  aObject.free;
end;

{ mmp }

class function mmp.use<T>(const aBoolean: boolean; const aTrueValue: T; aFalseValue: T): T;
var resolution: array[boolean] of T;
begin
  resolution[TRUE]  := aTrueValue;
  resolution[FALSE] := aFalseValue;
  result            := resolution[aBoolean];
end;

class function mmp.use<T>(const aBoolean: boolean; const aTrueFunc: TFunc<T>; aFalseValue: T): T;
type
  TResolver = reference to function: T;
var
  resolution: array[boolean] of TResolver;
begin
  resolution[TRUE]  := function: T begin result := aTrueFunc(); end;
  resolution[FALSE] := function: T begin result := aFalseValue; end;

  result := resolution[aBoolean]();
end;

class function mmp.use<T>(const aBoolean: boolean; const aTrueFunc: TFunc<T>; const aFalseFunc: TFunc<T>): T;
type
  TResolver = reference to function: T;
var
  resolution: array[boolean] of TResolver;
begin
  resolution[TRUE]  := function: T begin result := aTrueFunc(); end;
  resolution[FALSE] := function: T begin result := aFalseFunc(); end;

  result := resolution[aBoolean]();
end;

//===== TVoidFuncs which return a TVoid result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TVoidFunc; const falseFunc: TVoidFunc): TVoid;
begin
  use<TVoidFunc>(aBoolean, trueFunc, falseFunc)();
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TVoidFunc): TVoid;
begin
  result := cmd(aBoolean, trueFunc, nullVoidFunc);
end;

//===== TProcs
class function mmp.cmd(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): TVoid;
begin
  use<TProc>(aBoolean, trueProc, falseProc)();
end;

class function mmp.cmd(const aBoolean: boolean; const trueProc: TProc): TVoid;
begin
  cmd(aBoolean, trueProc, nullProc);
end;

//===== TOProcs with a TObject parameter
class function mmp.cmd(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): TVoid;
begin
  cmd(aBoolean, trueProc, nullOProc, aObject);
end;

class function mmp.cmd(const aBoolean: boolean; const trueProc: TRefProc; const aObject: PObject): TVoid;
begin
  mmp.cmd(aBoolean, procedure begin trueProc(aObject^); end);
end;

class function mmp.cmd(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): TVoid;
begin
  use<TOProc>(aBoolean, trueProc, falseProc)(aObject);
end;

//===== TBFuncs which return a boolean result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc): boolean;
begin
  result := use<TBFunc>(aBoolean, trueFunc, falseFunc)();
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc): boolean;
begin
  result := cmd(aBoolean, trueFunc, nullBFunc);
end;

//===== TSVoidFuncs which take a string and return a TVoid result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TVoidSFunc; const aString: string): TVoid;
begin
  use<TVoidSFunc>(aBoolean, trueFunc, nullVoidSFunc)(aString);
  result := default(TVoid);
end;

//===== TSFuncs which return a string result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseFunc: TSFunc): string;
begin
  result := use<TSFunc>(aBoolean, trueFunc, falseFunc)();
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFunc): string;
begin
  result := cmd(aBoolean, trueFunc, nullSFunc);
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseValue: string): string;
begin
  result := cmd(aBoolean, trueFunc, function: string begin result := falseValue; end);
end;

//===== TSFuncSs which take a string and return a string result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFuncS; const aString: string): string;
begin
  result := use<TSFuncS>(aBoolean, trueFunc, nullSFuncS)(aString);
end;

function eventTrigger(const aNoticeEvent: TNoticeEvent): INotice;
begin
  result := notifyApp(newNotice(aNoticeEvent));
end;

function nullEventTrigger(const aNoticeEvent: TNoticeEvent): INotice;
begin
  result := NIL;
end;

function eventTriggerInteger(const aNoticeEvent: TNoticeEVent; const aInteger: integer): INotice;
begin
  result := notifyApp(newNotice(aNoticeEvent, aInteger));
end;

function nullEventTriggerInteger(const aNoticeEvent: TNoticeEVent; const aInteger: integer): INotice;
begin
  result := NIL;
end;

//===== Event Notices with no result
class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): TVoid;
begin
  eventTrigger(use<TNoticeEvent>(aBoolean, trueEvent, falseEvent));
end;

class function mmp.cmd(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): TVoid;
begin
  for var i := low(trueNotices) to high(trueNotices) do
    use<TEventTrigger>(aBoolean, eventTrigger, nullEventTrigger)(trueNotices[i]);
end;

//===== Event Notices with the INotice returned
class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice;
begin
  result := use<TEventTrigger>(aBoolean, eventTrigger, nullEventTrigger)(trueEvent);
end;

class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := use<TEventTriggerInteger>(aBoolean, eventTriggerInteger, nullEventTriggerInteger)(trueEvent, aInteger);
end;

//===== Event Notices with the INotice returned (no boolean test)
class function mmp.cmd(const aEvent: TNoticeEvent): INotice;
begin
  result := notifyApp(newNotice(aEvent));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice;
begin
  result := notifyApp(newNotice(aEvent, aBoolean));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := notifyApp(newNotice(aEvent, aInteger));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aString: string): INotice;
begin
  result := notifyApp(newNotice(aEvent, aString));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice;
begin
  result := notifyApp(newNotice(aEvent, aText, aMediaType));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice;
begin
  result := notifyApp(newNotice(aEvent, aWndRec));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice;
begin
  result := notifyApp(newNotice(aEvent, aComponent));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice;
begin
  result := notifyApp(newNotice(aEvent, aMediaType));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice;
begin
  result := notifyApp(newNotice(aEvent, aMsg));
end;

class function mmp.cmd(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice;
begin
  result := notifyApp(newNotice(aEvent, aShiftState));
end;

//===== Misc
class procedure mmp.free(const aBoolean: boolean; const aObject: TObject);
begin
  use<TOProc>(aBoolean and assigned(aObject), freeObject, nullOProc)(aObject);
end;

function testSyntax: boolean;
begin
  result := mmp.use<TMediaType>(TRUE, mtAudio, mtVideo) = mtVideo;
end;

end.
