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

uses
  TMediaPlayerClass, TPlaylistClass;

function mmpCanDeleteCurrentItem(const aFilePath: string): boolean;
function mmpDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
function mmpDoDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
function mmpPlaySomething(const aIx: integer; const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
function mmpReloadPlaylist(const aFolder: string): string;

implementation

uses
  winApi.windows,
  system.classes, system.sysUtils,
  mmpConsts, mmpDialogs, mmpFileUtils, mmpSysCommands, mmpUtils,
  formMediaCaption, formPlaylist,
  TConfigFileClass, TGlobalVarsClass,
  _debugWindow;

function mmpCanDeleteCurrentItem(const aFilePath: string): boolean;
begin
  result := FALSE;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(aFilePath);
  case ssCtrl in mmpShiftState of  TRUE: vMsg := vMsg + '*.*';
                                  FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(aFilePath); end;

  result := mmpShowOkCancelMsgDlg(vMsg) = IDOK;
end;

function mmpDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
begin
 aMP.pause;
 case mmpCanDeleteCurrentItem(aPL.currentItem) of TRUE: begin
                                                          aMP.dontPlayNext := TRUE; // dontPlayNext because MP.stop would have done MP.playNext
                                                          aMP.stop;
                                                          case mmpDoDeleteCurrentItem(aPL, aMP) of TRUE: loadPlaylistWindow(TRUE); end;end;end;
end;

function mmpDoDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
begin
  result := FALSE;
  case aPL.hasItems of FALSE: EXIT; end;

  var vIx := aPL.currentIx;
  case mmpDeleteThisFile(aPL.currentItem, mmpShiftState) of FALSE: EXIT; end;
  aPL.delete(aPL.currentIx);  // this decrements PL's FPlayIx...
  case (ssCtrl in mmpShiftState) or (NOT aPL.hasItems) of
     TRUE:  case CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] AND aMP.playNextFolder of FALSE: mmpSendSysCommandClose(GV.appWnd); end; // shortcut logic!
    FALSE:  mmpPlaySomething(vIx, aPL, aMP); end;

  result := TRUE;
end;

function mmpPlaySomething(const aIx: integer; const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
begin
  case (aIx = 0) or aPL.isLast of  TRUE: aMP.playCurrent;   // aIx = 0 is not the same as .isFirst
                                  FALSE: aMP.playnext; end; // ...hence, playNext
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
