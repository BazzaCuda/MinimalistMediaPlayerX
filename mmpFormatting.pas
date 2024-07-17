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
unit mmpFormatting;

interface

function mmpFormatFileNumber    (const aFileNumber: integer; const aFileCount: integer): string;
function mmpFormatFileSize      (const aSize: int64): string;
function mmpFormatThumbFileSize (const aSize: int64): string;
function mmpFormatPageNumber    (const aPageNumber: integer; const aPageCount: integer; const aA: string): string;
function mmpFormatSeconds       (const aSeconds: integer): string;
function mmpFormatTickCount     (const aTickCount: double): string;
function mmpFormatTime          (const aSeconds: integer): string;
function mmpFormatWidthHeight   (const aWidth: integer; const aHeight: integer): string;

implementation

uses
  system.sysUtils;

function mmpFormatFileNumber(const aFileNumber: integer; const aFileCount: integer): string;
begin
  result := format(' %d / %d ', [aFileNumber, aFileCount]);
end;

function mmpFormatPageNumber(const aPageNumber: integer; const aPageCount: integer; const aA: string): string;
begin
  result := format('page %d%s / %d', [aPageNumber, aA, aPageCount]);
end;

function mmpFormatFileSize(const aSize: int64): string;
begin
 case aSize >= 1052266987 of  TRUE: try result := format('FS:  %.3f GB', [aSize / 1024 / 1024 / 1024]); except end;  // >= 0.98 of 1GB
                             FALSE: case aSize < 1024 * 1024 of  TRUE: try result := format('FS:  %d KB', [trunc(aSize / 1024)]); except end;
                                                                FALSE: try result := format('FS:  %.2f MB', [aSize / 1024 / 1024]); except end;end;end;
end;

function mmpFormatThumbFileSize(const aSize: int64): string;
begin
 case aSize >= 1052266987 of  TRUE: try result := format('%.3f GB', [aSize / 1024 / 1024 / 1024]); except end;  // >= 0.98 of 1GB
                             FALSE: case aSize < 1024 * 1024 of  TRUE: try result := format('%d KB', [trunc(aSize / 1024)]); except end;
                                                                FALSE: try result := format('%.2f MB', [aSize / 1024 / 1024]); except end;end;end;
end;

function mmpFormatSeconds(const aSeconds: integer): string;
begin
  case aSeconds < 100 of   TRUE: result := format('%ds', [aSeconds]);
                          FALSE: result := format('%dm%.2ds', [aSeconds div 60, aSeconds mod 60]);
  end;
end;

function mmpFormatTickCount(const aTickCount: double): string;
begin
  result := format('%.4fms', [aTickCount]);
end;

function mmpFormatWidthHeight(const aWidth: integer; const aHeight: integer): string;
begin
  result := format('%d x %d', [aWidth, aHeight]);
end;

function mmpFormatTime(const aSeconds: integer): string;
begin
  case aSeconds < 60 of  TRUE: result := format('%.2d:%.2d', [0, aSeconds]);
                        FALSE: case aSeconds < 3600 of   TRUE: result := format('%.2d:%.2d', [aSeconds div 60, aSeconds mod 60]);
                                                        FALSE: result := format('%.2d:%.2d:%.2d', [aSeconds div 3600, (aSeconds mod 3600) div 60, aSeconds mod 3600 mod 60]); end;end;
end;

end.
