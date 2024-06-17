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
unit mmpFolderNavigation;

interface

type
  TNextFolderDirection = (nfForwards, nfBackwards);

function mmpNextFolder(const aFolderPath: string; const aDirection: TNextFolderDirection; const aPrevFolderPath: string = ''): string;

implementation

uses
  system.classes, system.sysUtils,
  mmpFolderUtils;

procedure findSubFolders(const aFolderList: TStringList; const aFolderPath: string);
var
  rc: integer;
  sr: TSearchRec;
begin
  aFolderList.clear;
  aFolderList.sorted := FALSE;
  rc := findFirst(aFolderPath + '*.*', faDirectory, sr);
  while rc = 0 do begin
    case (sr.attr and faDirectory) <> 0 of TRUE:
      case (sr.name <> '.') and (sr.name <> '..') of TRUE: aFolderList.add(aFolderPath + sr.name + '\'); end;end;
    rc := findNext(sr);
  end;
  findClose(sr);
  aFolderList.sorted := TRUE;
end;

function mmpNextFolder(const aFolderPath: string; const aDirection: TNextFolderDirection; const aPrevFolderPath: string = ''): string;
var
  folderList: TStringList;
  ix: integer;

  function deepestSubFolder(aFolderPath: string): string;
  var
    folderList: TStringList;
  begin
    folderList := TStringList.Create;
    try
      findSubFolders(folderList, aFolderPath);
      case folderList.Count > 0 of
        TRUE: result := deepestSubFolder(folderList[folderList.Count - 1]);
       FALSE: result := aFolderPath; end;
    finally
      folderList.Free;
    end;
  end;
begin
  case (aDirection = nfForwards) and (aFolderPath = aPrevFolderPath) of TRUE: EXIT; end; //  reached the end of the folders on this drive

  folderList := TStringList.Create;
  try

    case aDirection = nfForwards of
      TRUE: begin

        findSubFolders(folderList, aFolderPath);      //  does this folder have subfolders ?
        case folderList.count <> 0 of                 //  if it does.....
          TRUE: begin
          case aPrevFolderPath = '' of                 //  ... and we haven't used any so far....
            TRUE: result := folderList[0];            //  ... just return the first in the list
           FALSE: begin
                    ix := folderList.indexOf(aPrevFolderPath); //  ... otherwise find the previous used in the list of subfolders...
                    case ix + 1 <= folderList.Count - 1 of    //  ... and return the next one if there is one...
                      TRUE: result := folderList[ix + 1];
                     FALSE: result := mmpNextFolder(extractFilePath(RTBS(aFolderPath)), aDirection, aFolderPath); end;  //  ...if this is the end of the subfolders go back up a level
                  end;
          end;
                end;
         FALSE: result := mmpNextFolder(extractFilePath(RTBS(aFolderPath)), aDirection, aFolderPath); end; // no subfolders so go back up a level
            end;

     FALSE:

      case aDirection = nfBackwards of TRUE: begin
        findSubFolders(folderList, extractFilePath(RTBS(aFolderPath)));  //  get this subfolder's siblings
        ix := folderList.indexOf(aFolderPath);                           //  find this subfolder's position in the list
        case ix - 1 >= 0 of                                              //  if there is a previous sibling, get its deepest subfolder
          TRUE: result := deepestSubFolder(FolderList[ix - 1]);
         FALSE: result := extractFilePath(RTBS(AFolderPath)); end;       //  otherwise just go up one level
      end;end;
    end;


  finally
    folderList.Free;
  end;
end;


end.
