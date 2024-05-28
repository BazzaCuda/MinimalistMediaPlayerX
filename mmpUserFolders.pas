{   Minimalist Media Player
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
unit mmpUserFolders;

interface

function mmpUserBaseFolder(const aFolder: string): string;
function mmpUserOverride(const aFolder: string): string;

implementation

uses
  system.sysUtils,
  mmpFolderUtils,
  TConfigFileClass;

function mmpUserBaseFolder(const aFolder: string): string;
begin
  result := '';

  case pos(':', mmpUserOverride(aFolder)) > 0 of TRUE: EXIT; end; // don't use the base folder if an override is an absolute path, e.g. "C:\....."

  result := CF.value['baseFolder'];
  case result <> '' of TRUE: result := ITBS(result); end;

end;

function mmpUserOverride(const aFolder: string): string;
begin
  result := '';
  var vValue := CF.value[aFolder];
  case vValue <> '' of  TRUE: result := vValue;
                       FALSE: result := aFolder; end;
  case result <> '' of TRUE: result := ITBS(result); end;
end;

end.
