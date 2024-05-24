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
unit TThumbsClass;

interface

uses
  generics.collections,
  vcl.extCtrls, vcl.forms,
  mmpConsts,
  TMPVHostClass, TPlaylistClass, TThumbClass;

type
  TThumbs = class(TObject)
  strict private
    FCurrentFolder:   string;
    FMPVHost:         TMPVHost;
    FThumbsHost:      TPanel;
    FThumbs:          TObjectList<TThumb>;
    FThumbSize: integer;
  private
    function fillPlaylist(const aPlaylist: TPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
    function generateThumbs(const aItemIx: integer): boolean;
  public
    FPlaylist: TPlaylist;
    constructor create;
    destructor destroy; override;
    function initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel): boolean;
    function playPrevThumbsPage: boolean;
    function playThumbs(const aFilePath: string = ''): boolean;
    function thumbsPerPage: integer;
    property thumbSize: integer read FThumbSize write FThumbSize;
  end;

implementation

uses
  system.sysUtils,
  vcl.controls, vcl.graphics,
  _debugWindow;

{ TThumbs }

constructor TThumbs.create;
begin
  inherited;
  FPlaylist := TPlaylist.create;
  FThumbs := TObjectList<TThumb>.create;
  FThumbs.ownsObjects := TRUE;
  FThumbSize := THUMB_DEFAULT_SIZE;
end;

destructor TThumbs.destroy;
begin
  case FPlaylist  = NIL of FALSE: FPlaylist.free; end;
  case FThumbs    = NIL of FALSE: FThumbs.free;   end;
  inherited;
end;

function TThumbs.fillPlaylist(const aPlaylist: TPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
begin
  case aPlaylist.hasItems AND (aPlaylist.currentFolder <> aCurrentFolder) of TRUE: aPlaylist.clear; end;
  case aPlaylist.hasItems of FALSE: aPlaylist.fillPlaylist(aCurrentFolder, [mtImage]); end;
  case aPlaylist.hasItems of TRUE:  aPlaylist.find(aFilePath); end;
  case aPlaylist.hasItems AND (aPlaylist.currentIx = -1) of TRUE: aPlaylist.first; end;
end;

function TThumbs.generateThumbs(const aItemIx: integer): boolean;
var
  vThumbTop:  integer;
  vThumbLeft: integer;
  vIx:        integer;
  vDone:      boolean;

  function adjustCurrentItem: boolean;  // guarantee a full page of thumbnails on the last page
  begin
    case (FPlaylist.count - FPlaylist.currentIx) < thumbsPerPage of TRUE: FPlaylist.setIx(FPlaylist.count - thumbsPerPage); end;
  end;

  function calcNextThumbPosition: integer;
  begin
    vThumbLeft := vThumbLeft + FThumbSize + THUMB_MARGIN;
    case (vThumbLeft + FThumbSize) > FThumbsHost.width of
      TRUE: begin
              vThumbLeft  := THUMB_MARGIN;
              vThumbTop   := vThumbTop + FThumbSize + THUMB_MARGIN; end;end;
  end;

begin
  case FPlaylist.validIx(aItemIx) of FALSE: EXIT; end;

  FThumbs.clear;

  adjustCurrentItem;

  vThumbTop  := THUMB_MARGIN;
  vThumbLeft := THUMB_MARGIN;

  repeat
    FThumbs.add(TThumb.create(FThumbsHost, FPlayList.currentItem));
    vIx := FThumbs.count - 1;

    FThumbs[vIx].top  := vThumbTop;
    FThumbs[vIx].left := vThumbLeft;

    calcNextThumbPosition;

    vDone := NOT FPlaylist.next;
  until (vThumbTop + FThumbSize > FThumbsHost.height) OR vDone;
end;

function TThumbs.initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel): boolean;
begin
  FMPVHost    := aMPVHost;
  FThumbsHost := aThumbsHost;
end;

function TThumbs.playPrevThumbsPage: boolean;
begin
  case FPlaylist.isFirst of FALSE:  begin
                                      FPlaylist.setIx(FPlaylist.currentIx - (thumbsPerPage * 2));
                                      playThumbs;
  end;end;
end;

function TThumbs.playThumbs(const aFilePath: string = ''): boolean;
begin
  case aFilePath <> '' of TRUE: begin
                                  FCurrentFolder := extractFilePath(aFilePath);
                                  fillPlaylist(FPlaylist, aFilePath, FCurrentFolder); end;end;
  generateThumbs(FPlaylist.currentIx);
end;

function TThumbs.thumbsPerPage: integer;
begin
  result := (FThumbsHost.width div (FThumbSize + THUMB_MARGIN)) * ((FThumbsHost.height) div (FThumbSize + THUMB_MARGIN));
end;

end.
