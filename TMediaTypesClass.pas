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
unit TMediaTypesClass;

interface

uses
  consts;

type

  TMediaTypes = class(TObject)
  strict private
    FMediaExts: string;
    function getMediaExts: string;
  protected
  private
  public
    constructor Create;
    destructor  Destroy;  override;
    function mediaType(const aExt: string): TMediaType;
    property mediaExts: string read FMediaExts;
  end;

function MT: TMediaTypes;

implementation

uses
  system.sysUtils;

var
  gMT: TMediaTypes;

function MT: TMediaTypes;
begin
  case gMT = NIL of TRUE: gMT := TMediaTypes.Create; end;
  result := gMT;
end;

{ TMediaType }

constructor TMediaTypes.Create;
begin
  inherited;
  getMediaExts;
end;

destructor TMediaTypes.Destroy;
begin
  inherited;
end;

function TMediaTypes.getMediaExts: string;
begin
  result := FMediaExts;
  case result <> '' of TRUE: EXIT; end;
  for var i := 0 to high(mediaTypes) do
    FMediaExts := FMediaExts + mediaTypes[i].fileExts;
  result := FMediaExts;
end;

function TMediaTypes.mediaType(const aExt: string): TMediaType;
begin
  result := mtUnk;
  for var i := 0 to high(mediaTypes) do
    case mediaTypes[i].fileExts.contains(aExt) of TRUE: begin
                                                          result := mediaTypes[i].mediaType;
                                                          EXIT; end;end;
end;

initialization
  gMT := NIL;

finalization
  case gMT <> NIL of TRUE: gMT.free; end;
end.
