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
unit TCleanupClass;

interface

type
  ICleanup = interface
  ['{EEC91FA7-48B3-419B-B5EC-63210CBA7DDF}']
    function cleanup(const aFolderPath: string): boolean;
  end;

function newCleanup: ICleanup;

implementation

uses
  system.classes, system.strUtils, system.sysUtils,
  mmpFileUtils;

type
  TCleanup = class(TInterfacedObject, ICleanup)
  public
    function cleanup(const aFolderPath: string): boolean;
  end;

function newCleanup: ICleanup;
begin
  result := TCleanup.create;
end;

{ TCleanup }

function TCleanup.cleanup(const aFolderPath: string): boolean;
const filesOnly = faAnyFile AND NOT faDirectory AND NOT faHidden and NOT faSysFile;
var SR: TSearchRec;

  function processSegFile: boolean;
  var vFN: string;
  begin
    result := FALSE;

    var vSL := TStringList.create;
    vSL.loadFromFile(aFolderPath + SR.name);

    for var i := 0 to vSL.count - 1 do begin
      vFN := copy(vSL[i], 7);
      delete(vFN, length(vFN), 1); // delete the trailing quote
      replaceStr(vFN, '\\', '\');
      case fileExists(vFN) of TRUE: mmpDeleteThisFile(vFN, [], TRUE, FALSE); end;
    end;
    vSL.free;
    result := TRUE;
  end;

  function extOK: boolean;
  begin
    var vExt := lowerCase(extractFileExt(SR.name)) + '.';
    case vExt = '.seg.' of TRUE: processSegFile; end;
    result := '.log.mmp.seg.key.'.contains(vExt);
  end;

begin
  result := FALSE;
  case findFirst(aFolderPath + '*.*', filesOnly, SR) = 0 of TRUE:
    repeat
      case extOK of TRUE: mmpDeleteThisFile(aFolderPath + SR.name, [], TRUE, FALSE); end;
    until findNext(SR) <> 0;
  end;
  findClose(SR);
  mmpRunTasks;
  result := TRUE;end;
end.
