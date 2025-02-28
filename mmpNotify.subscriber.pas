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
unit mmpNotify.subscriber;

interface

uses mmpNotify.notices;

function newSubscriber: ISubscriber; overload;
function newSubscriber(const aNotifyMethod: TNotifyMethod): ISubscriber; overload;

implementation

type
  TSubscriber = class(TInterfacedObject, ISubscriber)
  private
    FNotifyMethod: TNotifyMethod;
  public
    function  notifySubscriber(const aNotice: INotice): INotice;
    procedure setNotifyMethod(const aNotifyMethod: TNotifyMethod);
  end;

function newSubscriber: ISubscriber;
begin
  result := TSubscriber.create;
end;

function newSubscriber(const aNotifyMethod: TNotifyMethod): ISubscriber;
begin
  result              := newSubscriber;
  result.notifyMethod := aNotifyMethod;
end;
{ TSubscriber }

procedure TSubscriber.setNotifyMethod(const aNotifyMethod: TNotifyMethod);
begin
  FNotifyMethod := aNotifyMethod;
end;

function TSubscriber.notifySubscriber(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case assigned(FNotifyMethod) of TRUE: FNotifyMethod(aNotice); end;
end;

end.
