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
unit mmpAction;

interface

uses
  bazAction;

type
  TVoid = bazAction.TVoid;

  IAction<TResult> = interface(bazAction.IAction<TResult>)
  end;

  TAction<TResult> = class(bazAction.TAction<TResult>, bazAction.IAction<TResult>)
  end;

  IAction = interface(bazAction.IAction)
  end;

  TAction = class(bazAction.TAction, bazAction.IAction)
  end;

implementation

end.
