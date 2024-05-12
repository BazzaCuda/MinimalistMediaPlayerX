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
unit TBookmarkClass;

interface

type
  TBookmark = class(TObject)
  strict private
    function release: boolean;
  protected
  private
  public
    function asInteger(const aURL: string): integer;
    function delete(const aURL: string): string;
    function save(const aURL: string; const aPosition: integer): string;
  end;

function BM: TBookmark;

implementation

uses
  system.sysUtils,
  TConfigFileClass;

var
  gBM: TBookmark;

function BM: TBookmark;
begin
  case gBM = NIL of TRUE: gBM := TBookmark.Create; end;
  result := gBM;
end;

{ TBookmark }

function TBookmark.asInteger(const aURL: string): integer;
begin
  result := CF.asInteger[aURL];
end;

function TBookmark.delete(const aURL: string): string;
begin
  CF.deleteName(aURL);
  result := 'Bookmark deleted';
end;

function TBookmark.release: boolean;
begin
  freeAndNIL(gBM);
end;

function TBookmark.save(const aURL: string; const aPosition: integer): string;
begin
  CF.value[aURL] := intToStr(aPosition);
  result := 'Bookmarked';
  release;
end;

initialization
  gBM := NIL;

finalization
  case gBM <> NIL of TRUE: gBM.free; end;


end.
