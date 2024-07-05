{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit TListHelperClass;

interface

uses
  system.generics.collections, system.generics.defaults,
  mmpUtils;

type
  TListHelper = class helper for TList<string>
  public
    function naturalSort: boolean;
  end;

implementation

uses
  system.sysUtils,
  mmpFileUtils;

{ TListHelper }

function TListHelper.naturalSort: boolean;
begin
  result := FALSE;
  SELF.sort(
            TComparer<string>.construct(
                                        function(const a, b: string): integer
                                        begin
                                          result := mmpCompareStr(lowerCase(mmpFileNameWithoutExtension(extractFileName(a))), lowerCase(mmpFileNameWithoutExtension(extractFileName(b))));
                                        end
                                       )
           );
  result := TRUE;
end;

end.
