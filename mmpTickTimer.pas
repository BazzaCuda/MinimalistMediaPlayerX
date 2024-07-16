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
unit mmpTickTimer;

interface

uses
  system.classes,
  vcl.extCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  ITickTimer = interface
    ['{520972A9-0FDF-4557-A9A2-BE8B3D5B143A}']
    function getNotifier: INotifier;
    property notifier:    INotifier  read getNotifier;
  end;

function TT: ITickTimer;

implementation

uses
  winApi.windows,
  mmpConsts;

type
  TTickTimer = class(TInterfacedObject, ITickTimer)
  strict private
    FNotifier:  INotifier;
    FTimer:     TTimer;
  private
    procedure   timerEvent(aSender: TObject);
    function    getNotifier: INotifier;
  public
    constructor create;
    destructor  Destroy; override;
    property    notifier: INotifier  read getNotifier;
  end;

var gTT: ITickTimer = NIL;
function TT: ITickTimer;
begin
  case gTT = NIL of TRUE: gTT := TTickTimer.create; end;
  result := gTT;
end;

{ TTickTimer }

constructor TTickTimer.create;
begin
  inherited;
  FTimer          := TTimer.create(NIL);
  FTimer.interval := 999;
  FTimer.onTimer  := timerEvent;
end;

destructor TTickTimer.Destroy;
begin
  case FTimer = NIL of FALSE: FTimer.free; end;
  inherited;
end;

function TTickTimer.getNotifier: INotifier;
begin
  case FNotifier = NIL of TRUE: FNotifier := newNotifier; end;
  result := FNotifier;
end;

procedure TTickTimer.timerEvent(aSender: TObject);
begin
  FNotifier.notifySubscribers(newNotice(evTickTimer));
end;

initialization
  gTT := NIL;

finalization
  gTT := NIL;

end.
