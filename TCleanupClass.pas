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

uses
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpAction;

type
  ICleanup = interface
    function cleanup(const aFolderPath: string; const aCurrentItem: string): TVoid;
  end;

function newCleanup: ICleanup;

implementation

uses
  system.classes, system.strUtils, system.sysUtils,
  bazCmd,
  mmpFileUtils, mmpUtils,
  _debugWindow;

type
  TCleanup = class(TInterfacedObject, ICleanup)
  public
    function cleanup(const aFolderPath: string; const aCurrentItem: string): TVoid;
  end;

function newCleanup: ICleanup;
begin
  result := TCleanup.create;
end;

{ TCleanup }

function TCleanup.cleanup(const aFolderPath: string; const aCurrentItem: string): TVoid;
const
  {$WARN SYMBOL_PLATFORM OFF}
  filesOnly = faAnyFile AND NOT faDirectory AND NOT faHidden and NOT faSysFile;
  {$WARN SYMBOL_PLATFORM OFF}
var SR: TSearchRec;

  function processSegFile: TVoid;
  var vFN: string;
  begin
    var vSL := TStringList.create;
    vSL.loadFromFile(aFolderPath + SR.name);
                                                                    // file 'B:\\Movies\\Kelly'\''s Heroes (1970).seg02.mp4'
    for var i := 0 to vSL.count - 1 do begin
      vFN := copy(vSL[i], 7);
      delete(vFN, length(vFN), 1); // delete the trailing quote
      vFN := stringReplace(vFN, '''\''''', '''', [rfReplaceAll]); // EXPERIMENTAL - collapse each '\'' back to a single quote
      vFN := replaceStr(vFN, '\\', '\');                          // un-double the backslashes

      var vFound := mmp.cmd(evPLFind, vFN).tf;

      case fileExists(vFN) and vFound of TRUE: mmp.cmd(evPLDeleteIx, -1); end;
      case fileExists(vFN) of TRUE: mmpDeleteThisFile(vFN, [], TRUE, FALSE, sameText(vFN, aCurrentItem)); end; // only mpvStop if it's the current file
    end;
    vSL.free;
  end;

  function extOK: boolean;
  begin
    var vExt := lowerCase(extractFileExt(SR.name)) + '.';
    case vExt = '.seg.' of TRUE: processSegFile; end;
    result := '.log.mmp.seg.key.chp.'.contains(vExt);
  end;

begin
  case findFirst(aFolderPath + '*.*', filesOnly, SR) = 0 of TRUE:
    repeat
      case extOK of TRUE: mmpDeleteThisFile(aFolderPath + SR.name, [], TRUE, FALSE, FALSE); end; // no need to mpvStop for these files
    until (findNext(SR) <> 0);
  end;
  findClose(SR);
  mmpRunTasks;
end;

end.
