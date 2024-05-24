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
    FPlaylist:        TPlaylist;
    FThumbsHost:      TPanel;
    FThumbs:          TObjectList<TThumb>;
  private
    function fillPlaylist(const aPlaylist: TPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
  public
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
  case aPlaylist.hasItems AND (aPlaylist.currentFolder =  aCurrentFolder) of TRUE: aPlaylist.find(aFilePath); end;
  case aPlaylist.hasItems AND (aPlaylist.currentFolder <> aCurrentFolder) of TRUE: aPlaylist.clear; end;
  case aPlaylist.hasItems of FALSE: aPlaylist.fillPlaylist(extractFilePath(aFilePath), [mtImage]); end;
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
  FMPVHost.openFile(aFilePath);
end;

end.
