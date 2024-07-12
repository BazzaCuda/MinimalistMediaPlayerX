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
unit model.mmpPlaylistUtils;

interface

uses
  system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpGlobalState,
  model.mmpMediaPlayer, model.mmpPlaylist;

function mmpCheckPlaylistItemExists(const aPL: IPlaylist; const aMP: IMediaPlayer; const bNextFolderOnEmpty: boolean): boolean;
function mmpPlayCurrent:      boolean;
function mmpPlayFirst:        boolean;
function mmpPlayLast:         boolean;
function mmpPlayNext:         boolean;
function mmpPlayPrev:         boolean;
function mmpScreenshotFolder: string;
function mmpValidatePlaylist(const aPL: IPlaylist): boolean;

implementation

function mmpCheckPlaylistItemExists(const aPL: IPlaylist; const aMP: IMediaPlayer; const bNextFolderOnEmpty: boolean): boolean;
begin
  var vIx := aPL.currentIx;
  var vCI := aPL.currentItem;

  case mmpValidatePlaylist(aPL) of FALSE: case bNextFolderOnEmpty of   TRUE: case notifyApp(newNotice(evVMPlayNextFolder)).tf of
                                                                                                         TRUE: EXIT;
                                                                                                        FALSE: begin notifyApp(newNotice(evAppClose)); EXIT; end;end;
                                                                      FALSE: begin notifyApp(newNotice(evAppClose)); EXIT; end;end;end;

  case fileExists(vCI) of  TRUE:  begin
                                    aPL.find(vCI);
                                    notifyApp(newNotice(evVMMPPlayCurrent)); end;
                          FALSE:  case aPL.validIx(vIx) of   TRUE:  begin // play first remaining item that was after the original item's position in the playlist
                                                                      aPL.setIx(vIx);
                                                                      notifyApp(newNotice(evVMMPPlayCurrent)); end;
                                                            FALSE:  case aPL.validIx(vIx - 1) of   TRUE:  begin // otherwise play the nearest before its position
                                                                                                            aPL.setIx(vIx - 1);
                                                                                                            notifyApp(newNotice(evVMMPPlayCurrent)); end;
                                                                                                  FALSE:  notifyApp(newNotice(evVMMPPlayCurrent)); end;end;end;

end;

function mmpPlayCurrent: boolean;
begin
  notifyApp(newNotice(evVMShutTimeline));
  notifyApp(newNotice(evMPOpenUrl, notifyApp(newNotice(evPLReqCurrentItem)).text));
end;

function mmpPlayFirst: boolean;
begin
  case notifyApp(newNotice(evPLFirst)).tf of  TRUE: mmpPlayCurrent; end;
end;

function mmpPlayLast: boolean;
begin
  case notifyApp(newNotice(evPLLast)).tf of   TRUE: mmpPlayCurrent; end;
end;

function mmpPlayNext: boolean;
begin
  result := notifyApp(newNotice(evPLNext)).tf;

  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpPlayPrev: boolean;
begin
  result := notifyApp(newNotice(evPLPrev)).tf;
  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpScreenshotFolder: string;
begin
  case GS.MPVScreenshotDirectory = '' of   TRUE: result := notifyApp(newNotice(evPLReqCurrentFolder)).text;
                                          FALSE: result := GS.MPVScreenshotDirectory; end;
end;

function mmpValidatePlaylist(const aPL: IPlaylist): boolean;
begin
  result := FALSE;

  for var i := aPL.count - 1 downto 0 do case fileExists(aPL.thisItem(i)) of FALSE: aPL.deleteIx(i); end;

  aPL.first;

  notifyApp(newNotice(evPLFormLoadBox));

  result := aPL.hasItems;
end;

end.
