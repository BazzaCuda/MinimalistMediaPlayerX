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
unit model.mmpPlaylistUtils;

interface

uses
  system.sysUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpGlobalState,
  model.mmpConfigFile, model.mmpPlaylist;

function mmpCheckPlaylistItemExists(const aPL: IPlaylist; const bNextFolderOnEmpty: boolean): boolean;
function mmpPlayCurrent:                            boolean;
function mmpPlayFirst:                              boolean;
function mmpPlayLast:                               boolean;
function mmpPlayNext(const aMediaType: TMediaType): boolean;
function mmpPlayPrev(const aMediaType: TMediaType): boolean;
function mmpScreenshotFolder:                       string;
function mmpValidatePlaylist(const aPL: IPlaylist): boolean;

implementation

uses
  system.classes,
  vcl.forms,
  mmpFileUtils, mmpFuncProg, mmpUtils,
  _debugWindow;

function mmpCheckPlaylistItemExists(const aPL: IPlaylist; const bNextFolderOnEmpty: boolean): boolean;
begin
  var vIx := aPL.currentIx;
  var vCI := aPL.currentItem;

  case mmpValidatePlaylist(aPL) of FALSE: case bNextFolderOnEmpty of   TRUE: case mmp.cmd(evVMPlayNextFolder).tf of
                                                                                                         TRUE: EXIT;
                                                                                                        FALSE: begin mmp.cmd(evAppClose); EXIT; end;end;
                                                                      FALSE: begin mmp.cmd(evAppClose); EXIT; end;end;end;

  case fileExists(vCI) of  TRUE:  begin
                                    aPL.find(vCI);
                                    mmp.cmd(evVMMPPlayCurrent); end;
                          FALSE:  case aPL.validIx(vIx) of   TRUE:  begin // play first remaining item that was after the original item's position in the playlist
                                                                      aPL.setIx(vIx);
                                                                      mmp.cmd(evVMMPPlayCurrent); end;
                                                            FALSE:  case aPL.validIx(vIx - 1) of   TRUE:  begin // otherwise play the nearest before its position
                                                                                                            aPL.setIx(vIx - 1);
                                                                                                            mmp.cmd(evVMMPPlayCurrent); end;
                                                                                                  FALSE:  mmp.cmd(evVMMPPlayCurrent); end;end;end;
end;

function mmpPlayCurrent: boolean;
begin
  GS.notify(newNotice(evGSOpeningURL, TRUE));             // for TVM.reInitTimeline - gets reset in model.mmpMediaPlayer.openURL when all the new info is available
  var vURL := mmp.cmd(evPLReqCurrentItem).text;

  mmp.cmd(evMPOpenUrl, vURL); // for TVM.reInitTimeline - will reset evGSOpeningURL when it's finished
  while GS.openingURL do mmpProcessMessages;              // for TVM.reInitTimeline - wait for MP to finish opening the URL

  case GS.mediaType in [mtAudio, mtVideo] of FALSE: begin mmp.cmd(evVMShutTimeline); EXIT; end;end; // close the timeline if we're now displaying an image

  case GS.showingTimeline of TRUE: begin  var  vMPDuration := GS.duration;
                                          case vMPDuration > 0 of  TRUE: mmp.cmd(evVMReInitTimeline, vMPDuration); // reInit the timeline with the new media file details
                                                                  FALSE: mmp.cmd(evVMShutTimeline); end;end;end;
end;

function mmpPlayFirst: boolean;
begin
  result := mmp.cmd(evPLFirst).tf;
  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpPlayLast: boolean;
begin
  result := mmp.cmd(evPLLast).tf;
  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpPlayNext(const aMediaType: TMediaType): boolean;
begin
  result := mmp.cmd(evPLNext, aMediaType).tf; // bump the playlist to the next item if there is one
  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpPlayPrev(const aMediaType: TMediaType): boolean;
begin
  result := mmp.cmd(evPLPrev, aMediaType).tf;
  case result of TRUE: mmpPlayCurrent; end;
end;

function mmpScreenshotFolder: string;
begin
  case GS.MPVScreenshotDirectory = '' of   TRUE: result := mmp.cmd(evPLReqCurrentFolder).text;
                                          FALSE: result := GS.MPVScreenshotDirectory; end;
end;

function mmpValidatePlaylist(const aPL: IPlaylist): boolean;
begin
  result := FALSE;

  for var i := aPL.count - 1 downto 0 do case fileExists(aPL.thisItem(i)) of FALSE: aPL.deleteIx(i); end;

  aPL.first;

  mmp.cmd(evPLFormLoadBox);

  result := aPL.hasItems;
end;

end.
