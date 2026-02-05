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
  bazAction,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;


const
  doNowt = NIL;

var
  guardClause:  boolean;

type
  TOProc    = TProc<TObject>;
  TBFunc    = TFunc<boolean>;
  TSFunc    = TFunc<string>;
  TVoidFunc = function: TVoid of object;
  TProcVar  = procedure;

type
  mmp = record
    class function use<T>(const aBoolean: boolean; const aTrueValue: T; aFalseValue: T): T; overload; static;

    //===== TVoidFuncs which return a TVoid result
    class function cmd(const aBoolean: boolean; const trueFunc: TVoidFunc; const falseFunc: TVoidFunc): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TVoidFunc): TVoid; overload; static;

    //===== TProcs
    class function cmd(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueProc: TProc): TVoid; overload; static;

    //===== TOProcs with a TObject parameter
    class function cmd(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): TVoid; overload; static;

    //===== TBFuncs which return a boolean result
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc): boolean; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc): boolean; overload; static;

    //===== TBFuncs which return a boolean result while maintaning a current state
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc; const falseBoolean: boolean): boolean; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseBoolean: boolean): boolean; overload; static;

    //===== TSFuncs which return a string result
    class function cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseFunc: TSFunc): string; overload; static;
    class function cmd(const aBoolean: boolean; const trueFunc: TSFunc): string; overload; static;

    //===== Event Notices with no result
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): TVoid; overload; static;
    class function cmd(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): TVoid; overload; static;

    //===== Event Notices with the INotice returned
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice; overload; static;
    class function cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice; overload; static;

    //===== Event Notices with the INotice returned (no boolean test)
//    class function cmd<T>(const aEvent: TNoticeEvent; const aParam: T): INotice; overload; static;
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
    class procedure cmd(const aBoolean: boolean; const aProcVar: TProcVar); overload; static;
   end;

implementation

class function mmp.use<T>(const aBoolean: boolean; const aTrueValue: T; aFalseValue: T): T;
var setTypeVal: array[boolean] of T;
begin
  setTypeVal[TRUE]  := aTrueValue;
  setTypeVal[FALSE] := aFalseValue;
  result            := setTypeVal[aBoolean];
end;

{ mmp }

//===== TVoidFuncs which return a TVoid result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TVoidFunc; const falseFunc: TVoidFunc): TVoid;
var
  doFunc: array[boolean] of TVoidFunc;
begin
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;
  case assigned(doFunc[aBoolean]) of TRUE: doFunc[aBoolean](); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TVoidFunc): TVoid;
begin
  result := cmd(aBoolean, trueFunc, NIL);
end;

//===== TProcs
class function mmp.cmd(const aBoolean: boolean; const trueProc: TProc; const falseProc: TProc): TVoid;
var doProc: array[boolean] of TProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case assigned(doProc[aBoolean]) of TRUE: doProc[aBoolean](); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueProc: TProc): TVoid;
begin
  cmd(aBoolean, trueProc, NIL);
end;

//===== TOProcs with a TObject parameter
class function mmp.cmd(const aBoolean: boolean; const trueProc: TOProc; const aObject: TObject): TVoid;
begin
  cmd(aBoolean, trueProc, NIL, aObject);
end;

class function mmp.cmd(const aBoolean: boolean; const trueProc: TOProc; const falseProc: TOProc; const aObject: TObject): TVoid;
var doProc: array[boolean] of TOProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case assigned(doProc[aBoolean]) of TRUE: doProc[aBoolean](aObject); end;
end;

//===== TBFuncs which return a boolean result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc): boolean;
var doFunc: array[boolean] of TBFunc;
begin
  result        := FALSE;
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;
  case assigned(doFunc[aBoolean]) of TRUE: result := doFunc[aBoolean](); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc): boolean;
begin
  result := cmd(aBoolean, trueFunc, NIL);
end;

//===== TBFuncs which return a boolean result while maintaning a current state
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseFunc: TBFunc; const falseBoolean: boolean): boolean;
var
  doFunc: array[boolean] of TBFunc;
begin
  result        := falseBoolean; // in case falseFunc can be NIL
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;

  case assigned(doFunc[aBoolean]) of TRUE: result := doFunc[aBoolean](); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TBFunc; const falseBoolean: boolean): boolean;
begin
  result := cmd(aBoolean, trueFunc, NIL, falseBoolean);
end;

//===== TSFuncs which return a string result
class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFunc; const falseFunc: TSFunc): string;
var doFunc: array[boolean] of TSFunc;
begin
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;
  case assigned(doFunc[aBoolean]) of TRUE: result := doFunc[aBoolean](); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueFunc: TSFunc): string;
begin
  result := cmd(aBoolean, trueFunc, NIL);
end;

//===== Event Notices with no result
class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const falseEvent: TNoticeEvent): TVoid;
var
  T: TProc;
  F: TProc;
begin
  T := procedure begin notifyApp(newNotice(trueEvent)); end;
  F := procedure begin notifyApp(newNotice(falseEvent)); end;
  cmd(aBoolean, T, F);
end;

class function mmp.cmd(const aBoolean: boolean; const trueNotices: array of TNoticeEvent): TVoid;
begin
  for var i := low(trueNotices) to high(trueNotices) do begin
    var vNotice := trueNotices[i];
    var T:TProc := procedure begin notifyApp(newNotice(vNotice)); end;
    cmd(aBoolean, T, NIL);
  end;
end;

//===== Event Notices with the INotice returned
class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent): INotice;
begin
  result := NIL;
  case aBoolean of TRUE: result := notifyApp(newNotice(trueEvent)); end;
end;

class function mmp.cmd(const aBoolean: boolean; const trueEvent: TNoticeEvent; const aInteger: integer): INotice;
begin
  result := NIL;
  case aBoolean of TRUE: result := notifyApp(newNotice(trueEvent, aInteger)); end;
end;

//===== Event Notices with the INotice returned (no boolean test)
//class function mmp.cmd<T>(const aEvent: TNoticeEvent; const aParam: T): INotice;
//begin
//  result := notifyApp(newNotice(aEvent, aParam));
//end;

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
  cmd(aBoolean and (aObject <> NIL), procedure begin aObject.free; end);
end;

class procedure mmp.cmd(const aBoolean: boolean; const aProcVar: TProcVar);
begin
  case aBoolean of TRUE: aProcVar(); end;
end;

function testSyntax: boolean;
begin
  result := mmp.use<TMediaType>(true, mtAudio, mtVideo) = mtVideo;
end;

end.
