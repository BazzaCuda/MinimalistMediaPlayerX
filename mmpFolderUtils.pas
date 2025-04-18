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
unit mmpFolderUtils;

interface

function mmpITBS(aFolderPath: string): string;
function mmpRTBS(aFolderPath: string): string;

implementation

uses
  system.sysUtils,
  mmpConsts, mmpFuncProg;

function mmpITBS(aFolderPath: string): string;
begin
  result := aFolderPath;
  guardClause := (length(result) > 0) and (result[high(result)] <> BACKSLASH);
  result := mmp.use(guardClause, result + BACKSLASH, result);
end;

function mmpRTBS(aFolderPath: string): string;
begin
  result := aFolderPath;
  guardClause := (length(result) > 1) and charInSet(result[length(result)], ['\','/']) and ((length(result) <> 3) or (result[2] <> ':'));
  setLength(result, mmp.use(guardClause, length(result) - 1, length(result)));
end;


end.
