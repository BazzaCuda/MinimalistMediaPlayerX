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
unit mmpImageUtils;

interface

uses
  vcl.extCtrls,
  bazAction;

function mmpCopyPNGImage(const sourceImage: TImage; const destImage: TImage): TVoid;

implementation

function mmpCopyPNGImage(const sourceImage: TImage; const destImage: TImage): TVoid;
begin
  // Check if the source image has a picture to copy
  case assigned(sourceImage.picture) and assigned(sourceImage.picture.graphic) of FALSE: EXIT; end;

  // Clear the destination image
  destImage.picture := nil;

  // Assign the graphic content from the source to the destination
  destImage.picture.assign(sourceImage.picture.graphic);
end;


end.
