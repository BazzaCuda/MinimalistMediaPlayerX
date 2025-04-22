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
unit mmpNotify.notifier;

interface

uses
  winApi.windows,
  system.classes, system.generics.collections,
  mmpNotify.notices;

function appEvents: ISubscribable;
function newNotifier: INotifier;

function notifyApp(const aNotice: INotice): INotice;
function notifySubscribers(const aNotifier: INotifier; const aNotice: INotice): INotice; overload;
implementation

uses
  _debugWindow;

type
 TNotifier = class(TInterfacedObject, INotifier)
  private
    FSubscribers: TList<ISubscriber>;
  public
    function    subscribe(const aSubscriber: ISubscriber): ISubscriber;
    procedure   unsubscribe(const aSubscriber: ISubscriber);
    procedure   notifySubscribers(const aNotice: INotice);
    procedure   unsubscribeAll;

    constructor create;
    destructor  Destroy; override;
  end;

function newNotifier: INotifier;
begin
  result := TNotifier.create;
end;

function appEvents: ISubscribable;
{$J+} const gAppEvents: ISubscribable = NIL; {$J-}
begin
  case gAppEvents = NIL of TRUE: gAppEvents := newNotifier; end;
  result := gAppEvents;
end;

function notifyApp(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice  = NIL of TRUE: EXIT; end;
  (appEvents as INotifier).notifySubscribers(aNotice);
end;

function notifySubscribers(const aNotifier: INotifier; const aNotice: INotice): INotice;
begin
  result := aNotice;
  case (aNotifier = NIL) or (aNotice = NIL) of TRUE: EXIT; end;
  aNotifier.notifySubscribers(aNotice);
end;

{ TNotifier }

constructor TNotifier.create;
begin
  inherited;
  FSubscribers := TList<ISubscriber>.Create;
end;

destructor TNotifier.Destroy;
begin
  for var vSubscriber in FSubscribers do unsubscribe(vSubscriber);
  FSubscribers.free;
  inherited;
end;

procedure TNotifier.notifySubscribers(const aNotice: INotice);
begin
  case aNotice  = NIL of TRUE: EXIT; end;
  for var vSubscriber in FSubscribers do vSubscriber.notifySubscriber(aNotice);
end;

function TNotifier.subscribe(const aSubscriber: ISubscriber): ISubscriber;
begin
  result := aSubscriber;
  case aSubscriber = NIL of TRUE: EXIT; end;
  FSubscribers.add(aSubscriber);
end;

procedure TNotifier.unsubscribe(const aSubscriber: ISubscriber);
begin
  FSubscribers.remove(aSubscriber);
end;

procedure TNotifier.unsubscribeAll;
begin
  for var vSubscriber in FSubscribers do unsubscribe(vSubscriber);
end;

end.
