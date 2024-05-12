{   Minimalist Media Player
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
{   Minimalist Media Player
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
unit TGlobalVarsClass;

interface

uses  winAPI.windows, system.classes, vcl.forms, vcl.extCtrls;

type
  TGlobalVars = class(TObject)
  strict private
    FAppWnd: HWND;
    FAltKeyDown: boolean;
    FCloseApp: boolean;
    FShowingHelp: boolean;
    FShowingPlaylist: boolean;
    FShowingTimeline: boolean;
    FUserInput: boolean;
  public
    constructor create;
    destructor  destroy;
    property altKeyDown:      boolean read FAltKeyDown      write FAltKeyDown;
    property appWnd:          HWND    read FAppWnd          write FAppWnd;
    property closeApp:        boolean read FCloseApp        write FCloseApp;
    property showingHelp:     boolean read FShowingHelp     write FShowingHelp;
    property showingPlaylist: boolean read FShowingPlaylist write FShowingPlaylist;
    property showingTimeline: boolean read FShowingTimeline write FShowingTimeline;
    property userInput:       boolean read FUserInput       write FUserInput;
  end;

function GV: TGlobalVars;

implementation

uses
  vcl.controls;

var
  gGV: TGlobalVars;

function GV: TGlobalVars;
begin
  case gGV = NIL of TRUE: gGV := TGlobalVars.create; end;
  result := gGV;
end;

{ TGlobalVars }

constructor TGlobalVars.create;
begin
  inherited;
end;

destructor TGlobalVars.destroy;
begin
  inherited;
end;

initialization
  gGV := NIL;

finalization
  case gGV <> NIL of TRUE: gGV.free; end;

end.
