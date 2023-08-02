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
unit bookmark;

interface

type
  TBookmark = class(TObject)
  strict private
    function release: boolean;
  protected
  private
  public
    function asInteger: integer;
    function delete:    boolean;
    function save:      boolean;
  end;

function BM: TBookmark;

implementation

uses
  system.sysUtils, playlist, configFile, mediaPlayer, formSubtitles;

var
  gBM: TBookmark;

function BM: TBookmark;
begin
  case gBM = NIL of TRUE: gBM := TBookmark.Create; end;
  result := gBM;
end;

{ TBookmark }

function TBookmark.asInteger: integer;
begin
  result := CF.asInteger[PL.currentItem];
  ST.opInfo := 'From bookmark';
end;

function TBookmark.delete: boolean;
begin
  result := CF.deleteName(PL.currentItem);
  ST.opInfo := 'Bookmark deleted';
end;

function TBookmark.release: boolean;
begin
  freeAndNIL(gBM);
end;

function TBookmark.save: boolean;
begin
  CF.value[PL.currentItem] := intToStr(MP.position);
  ST.opInfo := 'Bookmarked';
  release;
end;

initialization
  gBM := NIL;

finalization
  case gBM <> NIL of TRUE: gBM.free; end;


end.
