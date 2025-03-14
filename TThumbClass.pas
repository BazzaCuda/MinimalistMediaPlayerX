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
unit TThumbClass;

interface

uses
  vcl.extCtrls;

type
  TThumb = class(TImage)
  strict private
  public
    constructor create(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
  end;

implementation

uses
  mmpThumbUtils;

{ TThumb }

constructor TThumb.create(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
begin
  inherited Create(NIL);

  width   := aDesiredWidth;
  height  := aDesiredHeight;
  stretch := TRUE;

  mmpExtractThumb(picture.bitmap, aFilePath, aDesiredWidth, aDesiredHeight);
end;

end.
