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
unit TListHelperClass;

interface

uses
  system.generics.collections, system.generics.defaults,
  mmpAction, mmpUtils,
  _debugWindow;

type
  TListHelper = class helper for TList<string>
  public
    function naturalSort: TVoid;
  end;

implementation

uses
  system.sysUtils,
  mmpFileUtils;

{ TListHelper }

function TListHelper.naturalSort: TVoid;
begin
  SELF.sort(
            TComparer<string>.construct(
                                        function(const a, b: string): integer
                                        begin
                                          var aa := lowerCase(mmpFileNameWithoutExtension(extractFileName(a)));
                                          var bb := lowerCase(mmpFileNameWithoutExtension(extractFileName(b)));
                                          case aa[1] = '!' of TRUE: aa[1] := #01; end;
                                          case bb[1] = '!' of TRUE: bb[1] := #01; end;
                                          result := mmpCompareStr(aa, bb);
                                        end
                                       )
           );
end;

end.
