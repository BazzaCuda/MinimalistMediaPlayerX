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
unit TMPVHostClass;

interface

uses
  vcl.extCtrls;

type
  TOpenFileEvent = procedure(const aURL: string) of object;

  TMPVHost = class(TPanel)
  strict private
    FOnOpenFile: TOpenFileEvent;
  public
    function openFile(const aURL: string): string;
    property OnOpenFile: TOpenFileEvent read FOnOpenFile write FOnOpenFile;
  end;

implementation

{ TMPVHost }

function TMPVHost.openFile(const aURL: string): string;
begin
  case assigned(FOnOpenFile) of TRUE: FOnOpenFile(aURL); end;
end;

end.
