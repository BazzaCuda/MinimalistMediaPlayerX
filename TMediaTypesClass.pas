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
  mmpConsts;

type

  TMediaTypes = class(TObject)
  strict private
    FMediaExts: string;
    function getMediaExts: string;
  protected
  private
  public
    constructor create;
    destructor  Destroy;  override;
    function mediaType(const aExt: string): TMediaType;
    property mediaExts: string read FMediaExts;
  end;

implementation

uses
  system.sysUtils;

{ TMediaType }

constructor TMediaTypes.create;
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

  var vExt: string := aExt;

  case vExt = '.avi' of TRUE: vExt := '.avii'; end; // fudge because of the ambiguity that .avi and .avif now cause. should have been .av1f !!

  for var mediaType: TMediaTypeRec in mediaTypes do
    case mediaType.fileExts.contains(vExt) of TRUE: begin
                                                      result := mediaType.mediaType;
                                                      EXIT; end;end;
end;

end.
