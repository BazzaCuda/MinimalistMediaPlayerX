{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit playlist;

interface

uses system.generics.collections;

type
  TPlaylist = class(TObject)
  strict private
    FPlayIx: integer;
    FPlaylist: TList<string>;
  private
    constructor create;
    destructor  Destroy; override;
  public
    function add(anItem: string): boolean;
    function clear: boolean;
    function count: integer;
    function currentItem: string;
    function currentIx: integer;
    function delete: boolean;
    function displayItem: string;
    function find(anItem: string): boolean;
    function first: boolean;
    function formattedItem: string;
    function hasItems: boolean;
    function isFirst: boolean;
    function isLast: boolean;
    function last: boolean;
    function next: boolean;
    function prev: boolean;
    function sort: boolean;
  end;

function PL: TPlaylist;

var
  gPL: TPlaylist;

implementation

uses
  system.sysUtils, sysCommands, globalVars;

function PL: TPlaylist;
begin
  case gPL = NIL of TRUE: gPL := TPlaylist.create; end;
  result := gPL;
end;

{ TPlaylist }

function TPlaylist.add(anItem: string): boolean;
begin
  FPlayList.add(anItem);
  FPlayIx := FPlayList.count - 1;
end;

function TPlaylist.clear: boolean;
begin
  FPlaylist.clear;
  FPlayIx := -1;
end;

function TPlaylist.count: integer;
begin
  result := FPlaylist.count;
end;

constructor TPlaylist.create;
begin
  inherited;
  FPlaylist := TList<string>.create;
  FPLaylist.sort;
end;

function TPlaylist.currentItem: string;
begin
  result := '';
  case FPlayIx = -1 of TRUE: EXIT; end;
  result := FPlaylist[FPlayIx];
end;

function TPlaylist.currentIx: integer;
begin
  result := FPlayIx;
end;

function TPlaylist.delete: boolean;
begin
  FPlayList.delete(FPlayIx);
  dec(FPlayIx);
end;

destructor TPlaylist.Destroy;
begin
  case FPlaylist <> NIL of TRUE: FPlaylist.free; end;
  inherited;
end;

function TPlaylist.displayItem: string;
begin
  result := format('[%d/%d] %s', [FPlayIx, count, extractFileName(currentItem)]);
end;

function TPlaylist.find(anItem: string): boolean;
begin
  FPlayIx := FPlaylist.indexOf(anItem);
  result  := FPlayIx <> -1;
end;

function TPlaylist.first: boolean;
begin
  result := FALSE;
  case hasItems of TRUE: FPlayIx := 0; end;
  result := TRUE;
end;

function TPlaylist.formattedItem: string;
begin
  result := format('[%d/%d] %s', [FPlayIx + 1, FPlaylist.count, ExtractFileName(currentItem)]);
end;

function TPlaylist.hasItems: boolean;
begin
  result := FPlaylist.count > 0;
end;

function TPlaylist.isFirst: boolean;
begin
  result := FPlayIx = 0;
end;

function TPlaylist.isLast: boolean;
begin
  result := FPlayIx = FPlaylist.count - 1;
end;

function TPlaylist.last: boolean;
begin
  result := FALSE;
  case hasItems of TRUE: FPlayIx := FPlaylist.count - 1; end;
  result := TRUE;
end;

function TPlaylist.next: boolean;
begin
  result := FALSE;
  case isLast of TRUE: EXIT; end;
  inc(FPlayIx);
  result := TRUE;
end;

function TPlaylist.prev: boolean;
begin
  result := FALSE;
  case isFirst of TRUE: EXIT; end;
  dec(FPlayIx);
  result := TRUE;
end;

function TPlaylist.sort: boolean;
begin
  FPlaylist.sort;
end;

initialization
  gPL := NIL;

finalization
  case gPL <> NIL of TRUE: gPL.free; end;

end.
