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
  private
    function fillPlaylist(const aPlaylist: TPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
    function generateThumbs(const aItemIx: integer): boolean;
  public
    FPlaylist:        TPlaylist;
    constructor create;
    destructor destroy; override;
    function initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel): boolean;
    function playThumbs(const aFilePath: string): boolean;
  end;

implementation

uses
  system.sysUtils,
  vcl.controls, vcl.graphics;

{ TThumbs }

constructor TThumbs.create;
begin
  inherited;
  FPlaylist := TPlaylist.create;
  FThumbs := TObjectList<TThumb>.create;
  FThumbs.ownsObjects := TRUE;
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
  case aPlaylist.hasItems of FALSE: aPlaylist.fillPlaylist(extractFilePath(aFilePath), [mtImage]); end;
  case aPlaylist.hasItems AND (aPlaylist.currentFolder =  aCurrentFolder) of TRUE: aPlaylist.find(aFilePath); end; // same folder so find the horse we rode in on
  case aPlaylist.hasItems AND (aPlaylist.currentIx = -1)                  of TRUE: aPlaylist.first; end;
end;

function TThumbs.generateThumbs(const aItemIx: integer): boolean;
var
  vThumbTop:  integer;
  vThumbLeft: integer;
  vIx:        integer;
  vDone:      boolean;
  vThumbSize: integer;

  procedure calcNextThumbPosition;
  begin
    vThumbLeft := vThumbLeft + vThumbSize + THUMB_MARGIN;
    case (vThumbLeft + vThumbSize) > FThumbsHost.width of
      TRUE: begin
              vThumbLeft  := THUMB_MARGIN;
              vThumbTop   := vThumbTop + vThumbSize + THUMB_MARGIN; end;end;
  end;

begin
  case FPlaylist.validIx(aItemIx) of FALSE: EXIT; end;

  vThumbTop  := THUMB_MARGIN;
  vThumbLeft := THUMB_MARGIN;

  vThumbSize := THUMB_DEFAULT_SIZE; // placeholder for when we allow the user to increase/decrease the size

  repeat
    FThumbs.add(TThumb.create(FThumbsHost, FPlayList.currentItem));
    vIx := FThumbs.count - 1;

    FThumbs[vIx].top  := vThumbTop;
    FThumbs[vIx].left := vThumbLeft;

    vDone := NOT FPlaylist.next;

    calcNextThumbPosition;

  until (vThumbTop + THUMB_DEFAULT_SIZE > FThumbsHost.height) OR vDone;

end;

function TThumbs.initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel): boolean;
begin
  FMPVHost    := aMPVHost;
  FThumbsHost := aThumbsHost;
end;

function TThumbs.playThumbs(const aFilePath: string): boolean;
begin
  FCurrentFolder := extractFilePath(aFilePath);
  fillPlaylist(FPlaylist, aFilePath, FCurrentFolder);
  generateThumbs(FPlaylist.currentIx);









//  FMPVHost.openFile(FPlaylist.currentItem);
//  FMPVHost.openFile(aFilePath);
//  FMPVHost.openFile('B:\Images\Asterix the Gaul\16 Asterix in Switzerland\Asterix -07- Asterix in Switzerland - 12.jpg');
end;

end.
