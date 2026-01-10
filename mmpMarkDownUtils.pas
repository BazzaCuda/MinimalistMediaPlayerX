{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit mmpMarkDownUtils;

interface

uses
  winApi.windows,
  system.classes,
  vcl.stdCtrls,
  htmlView, markDownUtils, MarkDownViewerComponents,
  mmpConsts;

function mmpInitMarkDownViewer(const aMD: TMarkDownViewer): boolean;
function mmpLoadMarkDownFromResource(const aMD: TMarkDownViewer; const aResourceName: string): boolean;

implementation

function mmpLoadMarkDownFromResource(const aMD: TMarkDownViewer; const aResourceName: string): boolean;
begin
  var vRS := TResourceStream.create(hInstance, aResourceName, RT_RCDATA);
  var vSS := TStringStream.create;
  try
    vSS.copyFrom(vRS);
    aMD.loadFromStream(vSS);
  finally
    vSS.free;
    vRS.free;
  end;
end;

function mmpInitMarkDownViewer(const aMD: TMarkDownViewer): boolean;
begin
  aMD.DefBackground    := DARK_MODE_DARK;
  aMD.defFontColor     := DARK_MODE_SILVER;
  aMD.defHotSpotColor  := DARK_MODE_SILVER;
  aMD.defOverLinkColor := DARK_MODE_SILVER;
  aMD.borderStyle      := htNone;
  aMD.defFontName      := FONT_TAHOMA;
  aMD.defFontSize      := 11;
  aMD.scrollBars       := ssVertical;
  aMD.htOptions        := [htOverLinksActive];
  aMD.processorDialect := mdCommonMark; // the only option if the markdown includes tables
end;

end.
