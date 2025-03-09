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

var
  T:      TProc;
  F:      TProc;

procedure mmpDo(aBoolean: boolean; trueProc: TProc; falseProc: TProc); overload;
procedure mmpDo(aBoolean: boolean; trueProc: TProc); overload;
procedure mmpDo(aBoolean: boolean; aTrueEvent: TNoticeEvent; aFalseEvent: TNoticeEvent); overload;
procedure mmpDo(aBoolean: boolean; aTrueEvent: TNoticeEvent); overload;

implementation

procedure mmpDo(aBoolean: boolean; trueProc: TProc; falseProc: TProc);
var doProc: array[boolean] of TProc;
begin
  doProc[TRUE]  := trueProc;
  doProc[FALSE] := falseProc;
  try
    doProc[aBoolean]();
  except
  end;
end;

procedure mmpDo(aBoolean: boolean; trueProc: TProc); overload;
begin
  mmpDo(aBoolean, trueProc, NIL);
end;

procedure mmpDo(aBoolean: boolean; aTrueEvent: TNoticeEvent; aFalseEvent: TNoticeEvent);
begin
  T :=  procedure begin notifyApp(newNotice(aTrueEvent)); end;
  F :=  procedure begin notifyApp(newNotice(aFalseEvent)); end;
  mmpDo(aBoolean, T, F);
end;

procedure mmpDo(aBoolean: boolean; aTrueEvent: TNoticeEvent);
begin
  T :=  procedure begin notifyApp(newNotice(aTrueEvent)); end;
  mmpDo(aBoolean, T, NIL);
end;

end.
