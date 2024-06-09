{   Minimalist Media Player
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
unit mmpPlaylistUtils;

interface

function mmpDeleteCurrentItem: boolean;
function mmpReloadPlaylist(const aFolder: string): string;

implementation

uses
  winApi.windows,
  system.classes, system.sysUtils,
  mmpDialogs, mmpFileUtils, mmpSysCommands, mmpUtils,
  formMediaCaption, formPlaylist,
  TGlobalVarsClass, TMediaPlayerClass, TPlaylistClass;

function mmpDeleteCurrentItem: boolean;
begin
  case PL.hasItems of FALSE: EXIT; end;
  MP.pause;

  var vShiftState := mmpShiftState;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(PL.currentItem);
  case ssCtrl in mmpShiftState of  TRUE: vMsg := vMsg + '*.*';
                                  FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(PL.currentItem); end;

  case mmpShowOkCancelMsgDlg(vMsg) = IDOK of TRUE:  begin
                                                      var vIx := PL.currentIx;
                                                      MP.dontPlayNext := TRUE;  // because...
                                                      MP.stop;                  // this would have automatically done MP.playNext
                                                      case mmpDeleteThisFile(PL.currentItem, vShiftState) of FALSE: EXIT; end;
                                                      PL.delete(PL.currentIx);  // this decrements PL's FPlayIx...
                                                      case (ssCtrl in vShiftState) or (NOT PL.hasItems) of
                                                                                                      TRUE: mmpSendSysCommandClose(GV.appWnd);
                                                                                                     FALSE: begin
                                                                                                              loadPlaylistWindow(TRUE);
                                                                                                              case vIx = 0 of  TRUE: MP.playCurrent;
                                                                                                                              FALSE: MP.playnext; end;end;end;end;end; // ...hence, playNext
end;

function mmpReloadPlaylist(const aFolder: string): string;
begin
  var vCurrentItem     := PL.currentItem;
  var vCurrentPosition := MP.position;

  PL.fillPlaylist(aFolder);
  case PL.find(vCurrentItem) of  TRUE: MP.position := vCurrentPosition;
                                FALSE: begin
                                         PL.first;
                                         MP.play(PL.currentItem); end;end;
  MC.caption := PL.formattedItem;
  result := 'Playlist reloaded';
end;

end.
