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
  system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

const
  doNowt = NIL;

type
  void    = boolean;
  TOProc  = TProc<TObject>;
  TRFunc  = TFunc<boolean>;

var
  T:      TProc;
  F:      TProc;

procedure mmpDo(aBoolean: boolean; trueProc:    TProc; falseProc: TProc); overload;
procedure mmpDo(aBoolean: boolean; trueProc:    TProc); overload;
procedure mmpDo(aBoolean: boolean; trueEvent:   TNoticeEvent; falseEvent: TNoticeEvent); overload;
procedure mmpDo(aBoolean: boolean; trueEvent:   TNoticeEvent); overload;
procedure mmpDo(aBoolean: boolean; trueNotices: array of TNoticeEvent); overload;
procedure mmpDo(aBoolean: boolean; trueEvent:   TNoticeEvent; aInteger: integer); overload;

procedure mmpDo(aBoolean: boolean; trueProc:    TOProc; aObject: TObject); overload;
procedure mmpDo(aBoolean: boolean; trueProc:    TOProc; falseProc: TOProc; aObject: TObject); overload;

function  mmpDo(aBoolean: boolean; trueFunc: TRFunc; falseFunc: TRFunc): boolean; overload;
function  mmpDo(aBoolean: boolean; trueFunc:    TRFunc): boolean; overload;

procedure mmpFree(aBoolean: boolean; aObject: TObject);

implementation

procedure mmpDo(aBoolean: boolean; trueProc: TProc; falseProc: TProc);
var doProc: array[boolean] of TProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case doProc[aBoolean] = NIL of FALSE: doProc[aBoolean](); end;
end;

procedure mmpDo(aBoolean: boolean; trueProc: TProc); overload;
begin
  mmpDo(aBoolean, trueProc, NIL);
end;

procedure mmpDo(aBoolean: boolean; trueEvent: TNoticeEvent; falseEvent: TNoticeEvent);
begin
  T :=  procedure begin notifyApp(newNotice(trueEvent)); end;
  F :=  procedure begin notifyApp(newNotice(falseEvent)); end;
  mmpDo(aBoolean, T, F);
end;

procedure mmpDo(aBoolean: boolean; trueEvent: TNoticeEvent);
begin
  T :=  procedure begin notifyApp(newNotice(trueEvent)); end;
  mmpDo(aBoolean, T, NIL);
end;

procedure mmpDo(aBoolean: boolean; trueNotices: array of TNoticeEvent);
begin
  for var i := low(trueNotices) to high(trueNotices) do begin
    var vNotice := trueNotices[i];
    T := procedure begin notifyApp(newNotice(vNotice)); end;
    mmpDo(aBoolean, T, NIL);
  end;
end;

procedure mmpDo(aBoolean: boolean; trueEvent: TNoticeEvent; aInteger: integer);
begin
  T :=  procedure begin notifyApp(newNotice(trueEvent, aInteger)); end;
  mmpDo(aBoolean, T);
end;

procedure mmpFree(aBoolean: boolean; aObject: TObject);
begin
  mmpDo(aBoolean and (aObject <> NIL), procedure begin aObject.free; end);
end;

procedure mmpDo(aBoolean: boolean; trueProc: TOProc; falseProc: TOProc; aObject: TObject);
var doProc: array[boolean] of TOProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  case doProc[aBoolean] = NIL of FALSE: doProc[aBoolean](aObject); end;
end;

procedure mmpDo(aBoolean: boolean; trueProc: TOProc; aObject: TObject);
begin
  mmpDo(aBoolean, trueProc, NIL, aObject);
end;

//=====

function mmpDo(aBoolean: boolean; trueFunc: TRFunc; falseFunc: TRFunc): boolean;
var doFunc: array[boolean] of TRFunc;
begin
  doFunc[TRUE]  := trueFunc;
  doFunc[FALSE] := falseFunc;
  case doFunc[aBoolean] = NIL of FALSE: result := doFunc[aBoolean](); end;
end;

function mmpDo(aBoolean: boolean; trueFunc: TRFunc): boolean;
begin
  result := mmpDo(aBoolean, trueFunc, NIL);
end;

end.
