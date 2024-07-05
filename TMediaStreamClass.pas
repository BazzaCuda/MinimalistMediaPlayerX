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
unit TMediaStreamClass;

interface

uses
  system.generics.collections,
  vcl.stdCtrls;

type
  TMediaStream = class(TObject)
  strict private
  private
    FID:          string;
    FBitRate:     string;
    FDuration:    string;
    FFormat:      string;
    FLanguage:    string;
    FStreamType:  string;
    FTitle:       string;
    FVideoWidth:  string;
    FVideoHeight: string;

    FIconIx:      integer;
    FInfo:        string;
    FSelected:    boolean;
  public
    constructor create(const aID: string; const aStreamType: string; const aDuration: string; const aFormat: string; const aBitRate: string; const aTitle: string; const aLanguage: string; const aInfo: string; const aIconIx: integer);
    property ID:          string  read FID          write FID;
    property streamType:  string  read FStreamType  write FStreamType;
    property duration:    string  read FDuration    write FDuration;
    property bitRate:     string  read FBitRate     write FBitRate;
    property format:      string  read FFormat      write FFormat;
    property title:       string  read FTitle       write FTitle;
    property language:    string  read FLanguage    write FLanguage;
    property videoHeight: string  read FVideoHeight write FVideoHeight;
    property videoWidth:  string  read FVideoWidth  write FVideoWidth;

    property iconIx:      integer read FIconIx      write FIconIx;
    property info:        string  read FInfo        write FInfo;
    property selected:    boolean read FSelected    write FSelected;
  end;

implementation

{ TMediaStream }

constructor TMediaStream.create(const aID, aStreamType, aDuration, aFormat, aBitRate, aTitle, aLanguage, aInfo: string; const aIconIx: integer);
begin
  FID         := aID;
  FStreamType := aStreamType;
  FDuration   := aDuration;
  FFormat     := aFormat;
  FBitRate    := aBitRate;
  FTitle      := aTitle;
  FLanguage   := aLanguage;
  FInfo       := aInfo;

  FIconIx     := aIconIx;
  FSelected   := TRUE;
end;

end.
