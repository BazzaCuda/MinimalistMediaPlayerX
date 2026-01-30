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
unit mmpMenu;

interface

uses
  vcl.menus,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  IMMPMenu = interface
    function popup(x, y: integer): IMMPMenu;
  end;

function newMMPMenu: IMMPMenu;

implementation

uses
  bazAction, bazCmd,
  model.mmpConfigFile,
  _debugWindow;

type
  TMMPMenu = class(TInterfacedObject, IMMPMenu)
  strict private
    FMenu:      TPopupMenu;
  private
    function    buildMenu(const aMenu: TPopupMenu): TVoid;
  protected
    procedure   onClick(sender: TObject);
  public
    constructor Create;
    destructor  Destroy; override;
    function    popup(x, y: integer): IMMPMenu;
  end;

function newMMPMenu: IMMPMenu;
begin
  result := TMMPMenu.create;
end;

{ TMMPMenu }

constructor TMMPMenu.Create;
begin
  inherited;
end;

destructor TMMPMenu.Destroy;
begin
  case FMenu = NIL of FALSE: FMenu.free; end;
  inherited;
end;

procedure TMMPMenu.onClick(sender: TObject);
begin
  mmp.cmd(evAppClose);
end;

function TMMPMenu.buildMenu(const aMenu: TPopupMenu): TVoid;
begin
  var vMenuItem     := TMenuItem.create(FMenu);
  vMenuItem.caption := 'Exit';
  vMenuItem.onClick := onClick;
  FMenu.items.add(vMenuItem);
end;

function TMMPMenu.popup(x, y: integer): IMMPMenu;
begin
  FMenu := TPopupMenu.create(NIL);
  buildMenu(FMenu);
  FMenu.popup(x, y);
  result := SELF;
end;

end.
