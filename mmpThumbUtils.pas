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
unit mmpThumbUtils;

interface

uses
  system.classes,
  vcl.graphics;
//  winApi.activeX, winApi.shlObj, winApi.windows,
//  system.sysUtils, system.win.comObj,
//  vcl.graphics;

procedure mmpExtractThumb(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);

implementation

uses
  system.math,
  mmpConsts,
  winapi.wincodec,
  winapi.windows,
  winapi.activex,
  system.types;

procedure mmpExtractThumb(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
begin
  case aFilePath = EMPTY of TRUE: EXIT; end;

  var vFactory: IWICImagingFactory;
  case succeeded(coCreateInstance(CLSID_WICImagingFactory, nil, CLSCTX_INPROC_SERVER, IID_IWICImagingFactory, vFactory)) of FALSE: EXIT; end;

  var vDecoder: IWICBitmapDecoder;
  case succeeded(vFactory.createDecoderFromFilename(pWideChar(wideString(aFilePath)), GUID_NULL, GENERIC_READ, WICDecodeMetadataCacheOnDemand, vDecoder)) of FALSE: EXIT; end;

  var vFrame: IWICBitmapFrameDecode;
  case succeeded(vDecoder.getFrame(0, vFrame)) of FALSE: EXIT; end;

  var vWidth: cardinal;
  var vHeight: cardinal;
  vFrame.getSize(vWidth, vHeight);

  var vScaleX := aDesiredWidth / vWidth;
  var vScaleY := aDesiredHeight / vHeight;
  var vScale  := min(vScaleX, vScaleY);

  var vNewWidth   := system.round(vWidth * vScale);
  var vNewHeight  := system.round(vHeight * vScale);

  var vScaler: IWICBitmapScaler;
  case succeeded(vFactory.createBitmapScaler(vScaler)) of FALSE: EXIT; end;

  case succeeded(vScaler.initialize(vFrame, vNewWidth, vNewHeight, WICBitmapInterpolationModeFant)) of FALSE: EXIT; end;

  var vConverter: IWICFormatConverter;
  case succeeded(vFactory.createFormatConverter(vConverter)) of FALSE: EXIT; end;

  case succeeded(vConverter.initialize(vScaler, GUID_WICPixelFormat32bppPBGRA, WICBitmapDitherTypeNone, NIL, 0.0, WICBitmapPaletteTypeCustom)) of FALSE: EXIT; end;

  var vBitmapInfo: tagBITMAPINFO;
  fillChar(vBitmapInfo, sizeOf(vBitmapInfo), 0);
  vBitmapInfo.bmiHeader.biSize := sizeOf(tagBITMAPINFOHEADER);
  vBitmapInfo.bmiHeader.biWidth := vNewWidth;
  vBitmapInfo.bmiHeader.biHeight := -vNewHeight;
  vBitmapInfo.bmiHeader.biPlanes := 1;
  vBitmapInfo.bmiHeader.biBitCount := 32;
  vBitmapInfo.bmiHeader.biCompression := BI_RGB;

  var vPixels: pointer;
  var vDeviceContext := getDC(0);
  var vHandleBitmap := createDIBSection(vDeviceContext, vBitmapInfo, DIB_RGB_COLORS, vPixels, 0, 0);
  releaseDC(0, vDeviceContext);

  case vHandleBitmap = 0 of TRUE: EXIT; end;

  var vStride := vNewWidth * 4;
  var vBufferSize := vNewHeight * vStride;

  case succeeded(vConverter.copyPixels(nil, vStride, vBufferSize, vPixels)) of
    TRUE: aBitmap.handle := vHandleBitmap;
    FALSE: deleteObject(vHandleBitmap);
  end;
end;

procedure mmpExtractThumb_wic2(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
// TWICImage doesn't natively support .jxl
begin
  case aFilePath = EMPTY of TRUE: EXIT; end;

  var vWICImage := TWicImage.create;
  try
    vWICImage.loadFromFile(aFilePath);

    var vScaleX := aDesiredWidth / vWICImage.width;
    var vScaleY := aDesiredHeight / vWICImage.height;
    var vScale  := min(vScaleX, vScaleY);

    var vNewWidth   := system.round(vWICImage.width * vScale);
    var vNewHeight  := system.round(vWICImage.height * vScale);

    var vBitmap     := vcl.graphics.TBitmap.create;
    try
      vBitmap.width   := vNewWidth;
      vBitmap.height  := vNewHeight;

      var vDestRect   := system.types.rect(0, 0, vNewWidth, vNewHeight);
      vBitmap.canvas.stretchDraw(vDestRect, vWICImage);

      aBitmap.assign(vBitmap);
    finally
      vBitmap.free;
    end;
  finally
    vWICImage.free;
  end;
end;

//const
//  IEIFLAG_OFFLINE = $0008;      // whether the extractor shouldn't hit the net to get any content needed for the rendering
//  IEIFLAG_SCREEN  = $0020;      // render as if for the screen  (this is exlusive with IEIFLAG_ASPECT )
//
//type
//  IExtractImage = interface
//    ['{BB2E617C-0920-11d1-9A0B-00C04FC2D6C1}']
//    function getLocation(pszPathBuffer: pWideChar; cch: DWORD; var pdwPriority: DWORD; var prgSize: TSize; dwRecClrDepth: DWORD; var pdwFlags: DWORD): HRESULT; stdcall;
//    function extract(var phBmpThumbnail: HBITMAP): HRESULT; stdcall;
//  end;

//procedure mmpExtractThumb_v2(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
//var
//  shellItem: IShellItemImageFactory;
//  size: TSize;
//  h: HBITMAP;
//begin
//  case aFilePath = EMPTY of TRUE: EXIT; end;
//
//  // convert path to Shell Item Factory
//  // IShellItemImageFactory is a specialised interface that provides the getImage method for high-performance thumbnail retrieval
//  case succeeded(SHCreateItemFromParsingName(pWideChar(WideString(aFilePath)), nil, IShellItemImageFactory, shellItem)) of
//    TRUE: begin
//            size.cx := aDesiredWidth;
//            size.cy := aDesiredHeight;
//
//            // Extract the bitmap handle
//            // SIIGBF_RESIZETOFIT instructs Windows to perform the high-quality scaling internally before returning the bitmap handle
//            case succeeded(shellItem.getImage(size, SIIGBF_RESIZETOFIT, h)) of TRUE: aBitmap.handle := h; end;end;end;
//end;
//
//procedure mmpExtractThumb_v1(const aBitmap: vcl.graphics.TBitmap; const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
//var
//  malloc:         IMalloc;
//  desktopFolder:  IShellFolder;
//  sourceFolder:   IShellFolder;
//  eaten:          cardinal;
//  flags:          cardinal;
//  prio:           cardinal;
//  id:             pItemIDList;
//  ex:             IExtractImage;
//  s:              TSize;
//  h:              HBITMAP;
//  w:              WideString;
//begin
////  coInitialize(NIL); // moved to the initialization section
//
//try
//  try
//    oleCheck(SHGetMalloc(Malloc));
//    oleCheck(SHGetDesktopFolder(DesktopFolder));
//
//    flags := 0;
//    w     := extractFilePath(aFilePath);
//    oleCheck(desktopFolder.parseDisplayName(0, nil, pWideChar(w), eaten, id, flags));
//    try
//      oleCheck(desktopFolder.bindToObject(id, nil, IShellFolder, sourceFolder));
//    finally
//      malloc.free(id);
//    end;
//
//    w := extractFileName(aFilePath);
//    oleCheck(sourceFolder.parseDisplayName(0, nil, pWideChar(w), eaten, id, flags));
//    try
//      oleCheck(sourceFolder.getUIObjectOf(0, 1, id, IExtractImage, nil, ex));
//    finally
//      malloc.free(id);
//    end;
//
//    s.cx  := aDesiredWidth;
//    s.cy  := aDesiredHeight;
//    flags := IEIFLAG_SCREEN or IEIFLAG_OFFLINE;
//    prio  := 0;
//    setLength(w, MAX_PATH);
//    oleCheck(ex.getLocation(pWideChar(w), length(w) * 2, prio, s, 32, flags));
//    oleCheck(ex.extract(h));
//
//    aBitmap.handle := h;
//  finally
//    desktopFolder := NIL;
//    sourceFolder  := NIL;
//    malloc        := NIL;
//  end;
//except end;
//end;
//
//initialization
//  coInitialize(NIL); // EXPERIMENTAL - moved from mmpThumbUtils.mmpExtractThumb
//
//finalization
//  coUninitialize;

end.
