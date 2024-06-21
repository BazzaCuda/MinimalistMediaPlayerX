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
unit mmpPlaylistUtils;

interface

uses
  system.classes,
  TMediaPlayerClass, TPlaylistClass;

function mmpCanDeleteCurrentItem(const aFilePath: string; const aShiftState: TShiftState): boolean;
function mmpCheckPlaylistItemExists(const aPL: TPlaylist; const aMP: TMediaPlayer; const bNextFolderOnEmpty: boolean): boolean;
function mmpDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer; const aShiftState: TShiftState; const bNextFolderOnEmpty: boolean): boolean;
function mmpDoDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer; const aShiftState: TShiftState; const bNextFolderOnEmpty: boolean): boolean;
function mmpPlaySomething(const aIx: integer; const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
function mmpReloadPlaylist(const aPL: TPlaylist; const aMP: TMediaPlayer): string;
function mmpValidatePlaylist(const aPL: TPlaylist): boolean;

implementation

uses
  winApi.windows,
  system.sysUtils,
  mmpConsts, mmpDialogs, mmpFileUtils, mmpSysCommands, mmpUtils,
  formMediaCaption, formPlaylist,
  _debugWindow;

function mmpCanDeleteCurrentItem(const aFilePath: string; const aShiftState: TShiftState): boolean;
begin
  result := FALSE;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(aFilePath);
  case ssCtrl in aShiftState of  TRUE: vMsg := 'DELETE folder contents '#13#10#13#10'Folder: ' + extractFilePath(aFilePath) + '*.*';
                                  FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(aFilePath); end;

  case ssCtrl in aShiftState of TRUE: vMsg := vMsg + #13#10#13#10'(doesn''t affect the contents of subfolders)'; end;

  result := mmpShowOkCancelMsgDlg(vMsg) = IDOK;
end;

function mmpCheckPlaylistItemExists(const aPL: TPlaylist; const aMP: TMediaPlayer; const bNextFolderOnEmpty: boolean): boolean;
begin
  var vIx := aPL.currentIx;
  var vCI := aPL.currentItem;

  case mmpValidatePlaylist(aPL) of FALSE: case bNextFolderOnEmpty of   TRUE: case aMP.playNextFolder of  TRUE: EXIT;
                                                                                                        FALSE: begin mmpSendSysCommandClose; EXIT; end;end;
                                                                      FALSE: begin mmpSendSysCommandClose; EXIT; end;end;end;

  case fileExists(vCI) of  TRUE:  begin
                                    aPL.find(vCI);
                                    aMP.play(aPL.currentItem); end;
                          FALSE:  case aPL.validIx(vIx) of   TRUE:  begin // play first remaining item that was after the original item's position in the playlist
                                                                      aPL.setIx(vIx);
                                                                      aMP.play(aPL.currentItem); end;  // don't call any of the timed events like MP.playCurrent
                                                            FALSE:  case aPL.validIx(vIx - 1) of   TRUE:  begin
                                                                                                            aPL.setIx(vIx - 1);
                                                                                                            aMP.play(aPL.currentItem); end;
                                                                                                  FALSE:  aMP.play(aPL.currentItem); end;end;end;

end;

function mmpDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer; const aShiftState: TShiftState; const bNextFolderOnEmpty: boolean): boolean;
begin
 aMP.pause;
 case mmpCanDeleteCurrentItem(aPL.currentItem, aShiftState) of TRUE:  begin
                                                                        aMP.dontPlayNext := TRUE; // dontPlayNext because MP.stop would have done MP.playNext
                                                                        aMP.stop;
                                                                        case mmpDoDeleteCurrentItem(aPL, aMP, aShiftState, bNextFolderOnEmpty) of TRUE: loadPlaylistWindow(TRUE); end;end;end;
end;

function mmpDoDeleteCurrentItem(const aPL: TPlaylist; const aMP: TMediaPlayer; const aShiftState: TShiftState; const bNextFolderOnEmpty: boolean): boolean;
begin
  result := FALSE;
  case aPL.hasItems of FALSE: EXIT; end;

  var vIx := aPL.currentIx;
  case mmpDeleteThisFile(aPL.currentItem, aShiftState) of FALSE: EXIT; end;
  aPL.delete(aPL.currentIx);  // this decrements PL's FPlayIx...
  case (ssCtrl in aShiftState) or (NOT aPL.hasItems) of
     TRUE:  case bNextFolderOnEmpty AND aMP.playNextFolder of FALSE: mmpSendSysCommandClose; end; // shortcut logic!
    FALSE:  mmpPlaySomething(vIx, aPL, aMP); end;

  result := TRUE;
end;

function mmpPlaySomething(const aIx: integer; const aPL: TPlaylist; const aMP: TMediaPlayer): boolean;
begin
  case (aIx = 0) or aPL.isLast of  TRUE: aMP.playCurrent;   // aIx = 0 is not the same as .isFirst
                                  FALSE: aMP.playnext; end; // ...hence, playNext
end;

function mmpReloadPlaylist(const aPL: TPlaylist; const aMP: TMediaPlayer): string;
begin
  var vCurrentItem     := aPL.currentItem;
  var vCurrentPosition := aMP.position;

  aPL.fillPlaylist(extractFilePath(vCurrentItem));
  case aPL.find(vCurrentItem) of   TRUE: aMP.position := vCurrentPosition;
                                  FALSE: begin
                                          aPL.first;
                                          aMP.play(aPL.currentItem); end;end;
  result := 'Playlist reloaded';
end;

function mmpValidatePlaylist(const aPL: TPlaylist): boolean;
begin
  result := FALSE;

  for var i := aPL.count - 1 downto 0 do case fileExists(aPL.thisItem(i)) of FALSE: aPL.delete(i); end;

  aPL.first;

  loadPlaylistWindow(TRUE);

  result := aPL.hasItems;
end;

end.
