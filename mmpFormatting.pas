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
unit mmpFormatting;

interface

function mmpFormatFileNumber    (const aFileNumber: integer; const aFileCount: integer): string;
function mmpFormatFileSize      (const aSize: int64): string;
function mmpFormatPageNumber    (const aPageNumber: integer; const aPageCount: integer; const aA: string): string;
function mmpFormatSeconds       (const aSeconds: integer): string;
function mmpFormatTickCount     (const aTickCount: double): string;
function mmpFormatTime          (const aSeconds: integer): string;
function mmpFormatWidthHeight   (const aWidth: integer; const aHeight: integer): string;

implementation

uses
  system.sysUtils,
  bazCmd, mmpConsts;

function mmpFormatFileNumber(const aFileNumber: integer; const aFileCount: integer): string;
begin
  result := format(' %d / %d ', [aFileNumber, aFileCount]);
end;

function mmpFormatPageNumber(const aPageNumber: integer; const aPageCount: integer; const aA: string): string;
begin
  result := format('page %d%s / %d', [aPageNumber, aA, aPageCount]);
end;

function mmpFormatFileSize(const aSize: int64): string;
var
  vFormatString:  string;
  vSize:          double;
begin
  try
    vFormatString := '%.0f KB';
    vFormatString := mmp.use(aSize >= _1MB, '%.2f MB', vFormatString);
    vFormatString := mmp.use(aSize >= _xGB, '%.3f GB', vFormatString); // >= 0.98 of 1GB

    vSize         := trunc(aSize / _1KB);
    vSize         := mmp.use<double>(aSize >= _1MB, aSize / _1MB, vSize);
    vSize         := mmp.use<double>(aSize >= _xGB, aSize / _1GB, vSize); // >= 0.98 of 1GB

    result := format(vFormatString, [vSize]);
  except end;
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
  case aSeconds < 60 of   TRUE: result := format('%.2d:%.2d', [0, aSeconds]); // 0:ss
  {mm:ss}                FALSE: case aSeconds < SECS_PER_HOUR of   TRUE: result := format('%.2d:%.2d',      [aSeconds div 60, aSeconds mod 60]);
  {hh:mm:ss}                                                      FALSE: result := format('%.2d:%.2d:%.2d', [aSeconds div SECS_PER_HOUR, (aSeconds mod SECS_PER_HOUR) div 60, aSeconds mod SECS_PER_HOUR mod 60]); end;end;
end;

end.
