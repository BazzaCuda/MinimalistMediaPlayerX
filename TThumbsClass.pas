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
  vcl.comCtrls, vcl.extCtrls, vcl.forms,
  mmpConsts,
  TMPVHostClass, TPlaylistClass, TThumbClass;

type
  TPlayType = (ptGenerateThumbs, ptPlaylistOnly);

  TThumbs = class(TObject)
  strict private
    FCurrentFolder:   string;
    FMPVHost:         TMPVHost;
    FPlaylist:        TPlaylist;
    FStatusBar:       TStatusBar;
    FThumbsHost:      TPanel;
    FThumbs:          TObjectList<TThumb>;
    FThumbSize:       integer;
  private
    function fillPlaylist(const aPlaylist: TPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
    function generateThumbs(const aItemIx: integer): integer;
    procedure thumbClick(sender: TObject);
    function getCurrentIx: integer;
  public
    constructor create;
    destructor destroy; override;
    function initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel; const aStatusBar: TStatusBar): boolean;
    function playCurrentItem: boolean;
    function playNext: boolean;
    function playPrev: boolean;
    function playPrevThumbsPage: boolean;
    function playThumbs(const aFilePath: string = ''; const aPlayType: TPlayType = ptGenerateThumbs): integer;
    function setPanelText(const aURL: string; aTickCount: double = 0): boolean;
    function thumbsPerPage: integer;
    property currentFolder: string read FCurrentFolder;
    property currentIx: integer read getCurrentIx;
    property playlist: TPlaylist read FPlaylist;
    property thumbSize: integer read FThumbSize write FThumbSize;
    property statusBar: TStatusBar write FStatusBar;
  end;

implementation

uses
  system.sysUtils,
  vcl.controls, vcl.graphics,
  mmpMPVFormatting,
  mmpFileUtils, mmpPanelCtrls, mmpUtils,
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

function TThumbs.generateThumbs(const aItemIx: integer): integer;
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

  function setPanelPageNo: boolean;
  var
    tpp: integer;
    extra: integer;
  begin
    tpp   := thumbsPerPage;
    extra := 0;
    case FPlaylist.count div tpp * tpp < FPlaylist.count of TRUE: extra := 1; end; // is there a remainder after fileCount div thumbsPerPage? If so, there's an extra page
    mmpSetPanelText(FStatusBar, pnVers, mmpFormatPageNumber(((FPlaylist.currentIx + tpp - 1) div tpp) + 1, FPlaylist.count div tpp + extra));
  end;

begin
  FThumbs.clear;

  case FPlaylist.validIx(aItemIx) of FALSE: EXIT; end;

  adjustCurrentItem;

  vThumbTop  := THUMB_MARGIN;
  vThumbLeft := THUMB_MARGIN;

  setPanelPageNo;

  repeat
    FThumbs.add(TThumb.create(FThumbsHost, FPlayList.currentItem, FThumbSize, FThumbSize));
    vIx := FThumbs.count - 1;

    FThumbs[vIx].top      := vThumbTop;
    FThumbs[vIx].left     := vThumbLeft;
    FThumbs[vIx].tag      := FPlaylist.currentIx;
    FThumbs[vIx].OnClick  := thumbClick;
    FThumbs[vIx].hint     := '|$' + FPlaylist.currentItem;

    setPanelText(FPlaylist.currentItem);

    calcNextThumbPosition;

    vDone := NOT FPlaylist.next;
  until (vThumbTop + FThumbSize > FThumbsHost.height) OR vDone;

  result := FPlaylist.currentIx;
end;

function TThumbs.getCurrentIx: integer;
begin
  result := FPlaylist.currentIx;
end;

function TThumbs.initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TPanel; const aStatusBar: TStatusBar): boolean;
begin
  FMPVHost    := aMPVHost;
  FThumbsHost := aThumbsHost;
  FStatusBar  := aStatusBar;
end;

function TThumbs.playCurrentItem: boolean;
begin
  case FPlaylist.hasItems of TRUE: FMPVHost.openFile(FPlaylist.currentItem); end;
end;

function TThumbs.playNext: boolean;
begin
  case FPlaylist.next of TRUE: FMPVHost.openFile(FPlaylist.currentItem); end;
end;

function TThumbs.playPrevThumbsPage: boolean;
begin
  case FPlaylist.isFirst of FALSE:  begin
                                      FPlaylist.setIx(FPlaylist.currentIx - (thumbsPerPage * 2));
                                      playThumbs;
  end;end;
end;

function TThumbs.playPrev: boolean;
begin
  case FPlaylist.prev of TRUE: FMPVHost.openFile(FPlaylist.currentItem); end;
end;

function TThumbs.playThumbs(const aFilePath: string = ''; const aPlayType: TPlayType = ptGenerateThumbs): integer;
begin
  result := -1;
  case aFilePath <> '' of TRUE: begin
                                  FCurrentFolder := extractFilePath(aFilePath);                // need to keep track of current folder in case it contains no images
                                  mmpSetPanelText(FStatusBar, pnSave, FCurrentFolder);
                                  fillPlaylist(FPlaylist, aFilePath, FCurrentFolder); end;end; // in which case, the playlist's currentFolder will be void

  case aPlayType of ptGenerateThumbs: result := generateThumbs(FPlaylist.currentIx); end;
  mmpProcessMessages; // force statusBar page number to display if the left or right arrow is held down (also displays file name and number)
end;

function TThumbs.setPanelText(const aURL: string; aTickCount: double = 0): boolean;
begin
  case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnName, extractFileName(aURL));
                             FALSE: mmpSetPanelText(FStatusBar, pnName, THUMB_NO_IMAGES); end;


  case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnNumb, mmpFormatFileNumber(FPlaylist.indexOf(aURL) + 1, FPlaylist.count));
                             FALSE: mmpSetPanelText(FStatusBar, pnNumb, mmpFormatFileNumber(0, 0)); end;

  case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnSize, mmpFormatThumbFileSize(mmpFileSize(aURL)));
                             FALSE: mmpSetPanelText(FStatusBar, pnSize, mmpFormatThumbFileSize(0)); end;


  mmpSetPanelText(FStatusBar, pnHint, mmpFormatTickCount(aTickCount));
  mmpSetPanelText(FStatusBar, pnSave, FPlaylist.currentFolder);
end;


procedure TThumbs.thumbClick(sender: TObject);
begin
  FPlaylist.setIx(TControl(sender).tag);
  FMPVHost.openFile(FPlayList.currentItem);
end;

function TThumbs.thumbsPerPage: integer;
begin
  result := ((FThumbsHost.width - THUMB_MARGIN) div (FThumbSize + THUMB_MARGIN)) * ((FThumbsHost.height - THUMB_MARGIN) div (FThumbSize + THUMB_MARGIN));
end;

end.
