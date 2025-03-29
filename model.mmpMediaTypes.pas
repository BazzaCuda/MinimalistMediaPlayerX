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
unit model.mmpMediaTypes;

interface

uses
  mmpConsts;

type
  IMediaTypes = interface
    ['{DE35A42E-3D19-4DBF-9491-8E6D8A175CE1}']
    function mediaType(const aExt: string): TMediaType;
  end;

function MT: IMediaTypes;

implementation

uses
  system.sysUtils;

type
  TMediaTypes = class(TInterfacedObject, IMediaTypes)
  strict private
    FMediaExts: string;
  private
    function    getMediaExts: string;
  protected
  public
    constructor create;
    destructor  Destroy;  override;
    function    mediaType(const aExt: string): TMediaType;
  end;

var gMT: IMediaTypes = NIL;
function MT: IMediaTypes;
begin
  case gMT = NIL of TRUE: gMT := TMediaTypes.create; end;
  result := gMT;
end;

{ TMediaTypes }

constructor TMediaTypes.create;
begin
  inherited;
//  getMediaExts; // not currently used
end;

destructor TMediaTypes.Destroy;
begin
  inherited;
end;

function TMediaTypes.getMediaExts: string;
begin
  result := FMediaExts;
  case result <> '' of TRUE: EXIT; end;
  for var i := low(mediaTypes) to high(mediaTypes) do
    FMediaExts := FMediaExts + mediaTypes[i].fileExts;
  result := FMediaExts;
end;

function TMediaTypes.mediaType(const aExt: string): TMediaType;
begin
  result := mtUnk;

  var vExt: string := lowerCase(extractFileExt(aExt)) + ' ';

  case trim(vExt) = '' of TRUE: EXIT; end;

  for var mediaType: TMediaTypeRec in mediaTypes do
    case mediaType.fileExts.contains(vExt) of TRUE: begin
                                                      result := mediaType.mediaType;
                                                      EXIT; end;end;
end;

initialization

finalization
  gMT := NIL;

end.
