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
unit mmpUserFolders;

interface

function mmpFolderFromFKey (const aKey: WORD):      string;
function mmpUserBaseFolder (const aFolder: string): string;
function mmpUserDstFolder  (const aFolder: string): string;
function mmpUserOverride   (const aFolder: string): string;

implementation

uses
  system.sysUtils,
  mmpConsts, mmpFolderUtils, mmpFileUtils,
  model.mmpConfigFile;

function mmpFolderFromFKey(const aKey: WORD): string;
begin
  result := 'folder' + intToStr(aKey - 111); // F1-F12 = 112-123
end;

function mmpUserBaseFolder(const aFolder: string): string;
begin
  result := EMPTY;

  case pos(':', mmpUserOverride(aFolder)) > 0 of TRUE: EXIT; end; // don't use the base folder if an override is an absolute path, e.g. "C:\....."

  result := CF[CONF_BASE_FOLDER];
  case result <> EMPTY of TRUE: result := mmpITBS(result); end;
end;

function mmpUserDstFolder(const aFolder: string): string;
begin
  result := mmpUserBaseFolder(aFolder) + mmpUserOverride(aFolder);
end;

function mmpUserOverride(const aFolder: string): string;
// destination folders can be absolute paths or relative to the base folder
begin
  result := EMPTY;
  var vValue := trim(CF[aFolder]); // trim() - allow the config window to set to a space to blank the folder name, but without CF then deleting the value
  case vValue <> EMPTY of  TRUE: result := vValue;
                          FALSE: result := aFolder; end;
  case result <> EMPTY of  TRUE: result := mmpITBS(result); end;
end;

end.
