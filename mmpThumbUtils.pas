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
unit mmpThumbUtils;

interface

uses
  system.sysUtils, system.win.comObj, vcl.graphics, winApi.activeX, winApi.shlObj, winApi.windows;

procedure ExtractThumb(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);


implementation

const
  IEIFLAG_OFFLINE = $0008;      // if the extractor shouldn't hit the net to get any content neede for the rendering
  IEIFLAG_SCREEN  = $0020;      // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )

type
  IExtractImage = interface
    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
    function getLocation(pszPathBuffer: PWideChar; cch: DWORD; var pdwPriority: DWORD; var prgSize: TSize; dwRecClrDepth: DWORD; var pdwFlags: DWORD): HResult; stdcall;
    function extract(var phBmpThumbnail: HBITMAP): HResult; stdcall;
  end;

procedure ExtractThumb(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
var
  malloc:         IMalloc;
  desktopFolder:  IShellFolder;
  sourceFolder:   IShellFolder;
  eaten:          cardinal;
  flags:          cardinal;
  prio:           cardinal;
  id:             PItemIDList;
  ex:             IExtractImage;
  s:              TSize;
  h:              HBITMAP;
  w:              WideString;
begin
  coInitialize(NIL); //move this to app startup code

try
  try
    oleCheck(SHGetMalloc(Malloc));
    oleCheck(SHGetDesktopFolder(DesktopFolder));

    flags := 0;
    w     := extractFilePath(aFilePath);
    oleCheck(desktopFolder.parseDisplayName(0, nil, PWideChar(w), eaten, id, flags));
    try
      oleCheck(desktopFolder.bindToObject(id, nil, IShellFolder, sourceFolder));
    finally
      malloc.free(id);
    end;

    w := extractFileName(aFilePath);
    oleCheck(sourceFolder.parseDisplayName(0, nil, PWideChar(w), eaten, id, flags));
    try
      oleCheck(sourceFolder.getUIObjectOf(0, 1, id, IExtractImage, nil, ex));
    finally
      malloc.free(id);
    end;

    s.cx  := aDesiredWidth;
    s.cy  := aDesiredHeight;
    flags := IEIFLAG_SCREEN or IEIFLAG_OFFLINE;
    prio  := 0;
    setLength(w, MAX_PATH);
    oleCheck(ex.getLocation(PWideChar(w), length(w) * 2, prio, s, 32, flags));
    oleCheck(ex.extract(h));

    aBitmap.handle := h;
  finally
    desktopFolder := NIL;
    sourceFolder  := NIL;
    malloc        := NIL;
  end;
except end;
end;

end.
