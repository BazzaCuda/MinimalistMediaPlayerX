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
unit TThumbsClass;

interface

uses
  system.classes, system.generics.collections,
  vcl.comCtrls, vcl.controls, vcl.extCtrls, vcl.forms,
  mmpConsts,
  model.mmpPlaylist,
  TMPVHostClass, TThumbClass;

type
  TPlayType = (ptGenerateThumbs, ptPlaylistOnly);

  IThumbs = interface
    ['{BAA8AD99-5696-442E-B97B-2DBDD2FAE55E}']
    function    cancel:             boolean;
    function    initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TWinControl; const aStatusBar: TStatusBar): boolean;
    function    playCurrentItem:    boolean;
    function    playPrevThumbsPage: boolean;
    function    playThumbs(const aFilePath: string = ''; const aPlayType: TPlayType = ptGenerateThumbs): integer;
    function    setPanelText(const aURL: string; aTickCount: double = -1; const aGetMediaInfo: boolean = FALSE): boolean;
    function    showDisplayDimensions(const aHost: THostType): boolean;
    function    thumbColCount:      integer;
    function    thumbsPerPage:      integer;
    function    thumbRowCount:      integer;

    function    getCurrentFolder: string;
    function    getCurrentIx: integer;
    function    getWhichHost: THostType;
    procedure   setFoldPanelReserved(const Value: boolean);
    function    getOnThumbClick: TNotifyEvent;
    procedure   setOnThumbClick(const Value: TNotifyEvent);
    function    getPlaylist: IPlaylist;
    procedure   setStatusBar(const Value: TStatusBar);
    function    getThumbSize: integer;
    procedure   setThumbSize(const Value: integer);

    property    currentFolder:      string        read getCurrentFolder;
    property    currentIx:          integer       read getCurrentIx;
    property    foldPanelReserved:  boolean                             write setFoldPanelReserved;
    property    onThumbClick:       TNotifyEvent  read getOnThumbClick  write setOnThumbClick;
    property    playlist:           IPlaylist     read getPlaylist;
    property    thumbSize:          integer       read getThumbSize     write setThumbSize;
    property    statusBar:          TStatusBar                          write setStatusBar;
    property    whichHost:          THostType     read getWhichHost;
  end;

function newThumbs: IThumbs;

implementation

uses
  winApi.windows,
  system.sysUtils,
  vcl.graphics,
  mmpFileUtils, mmpFormatting, mmpPanelCtrls, mmpUtils,
  model.mmpMediaInfo,
  _debugWindow;

type
  TThumbs = class(TInterfacedObject, IThumbs)
  strict private
    FCancel: boolean;
    FCurrentFolder:     string;
    FMPVHost:           TMPVHost;
    FOnThumbClick:      TNotifyEvent;
    FPlaylist:          IPlaylist;
    FFoldPanelReserved: boolean;
    FStatusBar:         TStatusBar;
    FThumbsHost:        TWinControl;
    FThumbs:            TList<IThumb>;
    FThumbSize:         integer;
  private
    function    fillPlaylist(const aPlaylist: IPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
    function    generateThumbs(const aItemIx: integer): integer;
    function    getCurrentIx: integer;
    function    getWhichHost: THostType;
    function    getCurrentFolder: string;
    procedure   setFoldPanelReserved(const Value: boolean);
    function    getOnThumbClick: TNotifyEvent;
    procedure   setOnThumbClick(const Value: TNotifyEvent);
    function    getPlaylist: IPlaylist;
    function    getThumbSize: integer;
    procedure   setThumbSize(const Value: integer);
    procedure   setStatusBar(const Value: TStatusBar);
  public
    constructor create;
    destructor  Destroy; override;
    function    cancel:             boolean;
    function    initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TWinControl; const aStatusBar: TStatusBar): boolean;
    function    playCurrentItem:    boolean;
    function    playPrevThumbsPage: boolean;
    function    playThumbs(const aFilePath: string = ''; const aPlayType: TPlayType = ptGenerateThumbs): integer;
    function    setPanelText(const aURL: string; aTickCount: double = -1; const aGetMediaInfo: boolean = FALSE): boolean;
    function    showDisplayDimensions(const aHost: THostType): boolean;
    function    thumbColCount:      integer;
    function    thumbsPerPage:      integer;
    function    thumbRowCount:      integer;

    property    currentFolder:      string        read getCurrentFolder;
    property    currentIx:          integer       read getCurrentIx;
    property    foldPanelReserved:  boolean                             write setFoldPanelReserved;
    property    onThumbClick:       TNotifyEvent  read getOnThumbClick  write setOnThumbClick;
    property    playlist:           IPlaylist     read getPlaylist;
    property    thumbSize:          integer       read getThumbSize     write setThumbSize;
    property    statusBar:          TStatusBar                          write setStatusBar;
    property    whichHost:          THostType     read getWhichHost;
  end;

var gThumbs: TThumbs = NIL;
function newThumbs: IThumbs;
begin
  case gThumbs = NIL of TRUE: gThumbs := TThumbs.create; end;
  result := gThumbs;
end;

{ TThumbs }

function TThumbs.cancel: boolean;
begin
  FCancel := TRUE;
end;

constructor TThumbs.create;
begin
  inherited;
  FPlaylist   := newPlaylist;
  FThumbs     := TList<IThumb>.create;
  FThumbSize  := THUMB_DEFAULT_SIZE;
end;

destructor TThumbs.Destroy;
begin
  FPlaylist := NIL;
  case FThumbs = NIL of FALSE: for var i := 0 to FThumbs.count - 1 do FThumbs[i] := NIL; end;
  case FThumbs = NIL of FALSE: FThumbs.free; end;
  inherited;
end;

function TThumbs.fillPlaylist(const aPlaylist: IPlaylist; const aFilePath: string; const aCurrentFolder: string): boolean;
begin
  case aPlaylist.hasItems AND (aPlaylist.currentFolder <> aCurrentFolder) of TRUE: aPlaylist.clear; end;
  case aPlaylist.hasItems of  FALSE: aPlaylist.fillPlaylist(aCurrentFolder, [mtImage]); end;
  case aPlaylist.hasItems of   TRUE: aPlaylist.find(aFilePath); end;
  case aPlaylist.hasItems AND (aPlaylist.currentIx = -1) of TRUE: aPlaylist.first; end;
end;

function TThumbs.generateThumbs(const aItemIx: integer): integer;
var
  vThumbTop:  integer;
  vThumbLeft: integer;
  vIx:        integer;

  function adjustCurrentItem: boolean;  // guarantee a full page of thumbnails on the last page
  begin
    var vEndIx := FPlaylist.count - 1;
    case (vEndIx - FPlaylist.currentIx) < thumbsPerPage of TRUE: FPlaylist.setIx(vEndIx - (thumbsPerPage - 1)); end;
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
    case FPlaylist.count mod tpp > 0 of  TRUE: extra := 1; // is there a remainder after fileCount div thumbsPerPage? If so, there's an extra page
                                        FALSE: extra := 0; end;
    var vPageNo := (((FPlaylist.currentIx - 1) + tpp) div tpp) + 1;   // was tpp + 1
    case FPlaylist.isLast of TRUE: vPageNo := vPageNo + extra; end;

    var vA := '';
    case (FPlaylist.currentIx <> ((FPlaylist.count - 1) - (thumbsPerPage - 1))) and ((FPlaylist.currentIx mod thumbsPerPage) <> 0) of TRUE: vA := 'a'; end;

    mmpSetPanelText(FStatusBar, pnHelp, mmpFormatPageNumber(vPageNo, (FPlaylist.count div tpp) + extra, vA));
  end;

begin
  FThumbs.clear;

  case FPlaylist.validIx(aItemIx) of FALSE: EXIT; end;

  adjustCurrentItem;

  setPanelPageNo;

  vThumbTop  := THUMB_MARGIN;
  vThumbLeft := THUMB_MARGIN;

  repeat
    FThumbs.add(newThumb(FPlayList.currentItem, FThumbSize, FThumbSize));
    vIx := FThumbs.count - 1;

    FThumbs[vIx].thumbTop      := vThumbTop;
    FThumbs[vIx].thumbLeft     := vThumbLeft;
    FThumbs[vIx].thumbTag      := FPlaylist.currentIx;
    FThumbs[vIx].onThumbClick  := FOnThumbClick;
    FThumbs[vIx].thumbHint     := '|$' + FPlaylist.currentItem;

    FThumbs[vIx].thumbParent   := FThumbsHost;  // delay to prevent flicker of top left thumbnail

    setPanelText(FPlaylist.currentItem);

    mmpProcessMessages; // show the thumbnails as they're drawn

    calcNextThumbPosition;

  until (NOT FPlaylist.next) OR ((vThumbTop + FThumbSize) > FThumbsHost.height) OR FCancel;

  result := FPlaylist.currentIx;
end;

function TThumbs.getCurrentFolder: string;
begin
  result := FCurrentFolder;
end;

function TThumbs.getCurrentIx: integer;
begin
  result := FPlaylist.currentIx;
end;

function TThumbs.getOnThumbClick: TNotifyEvent;
begin
  result := FOnThumbClick;
end;

function TThumbs.getPlaylist: IPlaylist;
begin
  result := FPlaylist;
end;

function TThumbs.getThumbSize: integer;
begin
  result := FThumbSize;
end;

function TThumbs.getWhichHost: THostType;
begin
  case FThumbsHost.visible of  TRUE: result := htThumbsHost;  end;
  case FMPVHost.visible    of  TRUE: result := htMPVHost;     end;
end;

function TThumbs.initThumbs(const aMPVHost: TMPVHost; const aThumbsHost: TWinControl; const aStatusBar: TStatusBar): boolean;
begin
  FMPVHost    := aMPVHost;
  FThumbsHost := aThumbsHost;
  FStatusBar  := aStatusBar;
end;

function TThumbs.playCurrentItem: boolean;
begin
  case FPlaylist.hasItems of TRUE: FMPVHost.openFile(FPlaylist.currentItem); end;
end;

function TThumbs.playPrevThumbsPage: boolean;
begin
  case FPlaylist.isFirst of FALSE:  begin
                                      FPlaylist.setIx(FPlaylist.currentIx - (thumbsPerPage * 2));
                                      playThumbs;
  end;end;
end;

function TThumbs.playThumbs(const aFilePath: string = ''; const aPlayType: TPlayType = ptGenerateThumbs): integer;
begin
  result := -1;
  case aFilePath <> '' of TRUE: begin
                                  FCurrentFolder := extractFilePath(aFilePath);                // need to keep track of current folder in case it contains no images
                                  mmpInitStatusBar(FStatusBar);
                                  mmpSetPanelText(FStatusBar, pnFold, FCurrentFolder);
                                  fillPlaylist(FPlaylist, aFilePath, FCurrentFolder); end;end; // in which case, the playlist's currentFolder will be void

  FCancel := FALSE;
  case aPlayType of ptGenerateThumbs: result := generateThumbs(FPlaylist.currentIx); end;
  mmpProcessMessages; // force statusBar page number to display if the left or right arrow is held down (also displays file name and number)
end;

procedure TThumbs.setFoldPanelReserved(const Value: boolean);
begin
 FFoldPanelReserved := value;
end;

procedure TThumbs.setOnThumbClick(const Value: TNotifyEvent);
begin
  FOnThumbClick := value;
end;

function TThumbs.setPanelText(const aURL: string; aTickCount: double = -1; const aGetMediaInfo: boolean = FALSE): boolean;
begin
  case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnName, extractFileName(aURL));
                             FALSE: mmpSetPanelText(FStatusBar, pnName, THUMB_NO_IMAGES); end;

  case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnNumb, mmpFormatFileNumber(FPlaylist.indexOf(aURL) + 1, FPlaylist.count));
                             FALSE: mmpSetPanelText(FStatusBar, pnNumb, mmpFormatFileNumber(0, 0)); end;

  case aGetMediaInfo      of  TRUE: case FPlaylist.hasItems of  TRUE: mmpSetPanelText(FStatusBar, pnSize, mmpFormatFileSize(mmpFileSize(aURL)));
                                                               FALSE: mmpSetPanelText(FStatusBar, pnSize, mmpFormatFileSize(0)); end;
                             FALSE: mmpSetPanelText(FStatusBar, pnSize, ''); end;


  case aGetMediaInfo      of  TRUE: case FPlaylist.hasItems of  TRUE: begin
                                                                        MI.getMediaInfo(aURL, mtImage);
                                                                        mmpSetPanelText(FStatusBar, pnXXYY, format('%d x %d', [MI.imageWidth, MI.imageHeight]));
                                                                      end;
                                                               FALSE: mmpSetPanelText(FStatusBar, pnXXYY, ''); end;
                             FALSE: mmpSetPanelText(FStatusBar, pnXXYY, ''); end;

  case whichHost of htMPVHost:    showDisplayDimensions(htMPVHost);
                    htThumbsHost: showDisplayDimensions(htThumbsHost); end;

  case aTickCount <> -1 of TRUE: mmpSetPanelText(FStatusBar, pnTick, mmpFormatTickCount(aTickCount)); end;

  case FFoldPanelReserved of  TRUE: FFoldPanelReserved := FALSE;
                             FALSE: mmpSetPanelText(FStatusBar, pnFold, FPlaylist.currentFolder); end;

  FStatusBar.repaint;
  mmpProcessMessages;
end;

procedure TThumbs.setStatusBar(const Value: TStatusBar);
begin
  FStatusBar := value;
end;

procedure TThumbs.setThumbSize(const Value: integer);
begin
  FThumbSize := value;
end;

function TThumbs.showDisplayDimensions(const aHost: THostType): boolean;
begin
  case FPlaylist.hasItems of  TRUE: case aHost of htMPVHost:    mmpSetPanelText(FStatusBar, pnDDXY, format('D: %d x %d', [FMPVHost.width, FMPVHost.height]));
                                                  htThumbsHost: mmpSetPanelText(FStatusBar, pnDDXY, format('D: %d x %d', [FThumbsHost.width, FThumbsHost.height])); end;
                             FALSE: mmpSetPanelText(FStatusBar, pnDDXY, ''); end;
end;

function TThumbs.thumbColCount: integer;
var
  vWorkingWidth: integer;
  vRemainingWidth: integer;
begin
  result := 1;
  vWorkingWidth := FThumbsHost.width - THUMB_MARGIN;                           // ignore the left-hand margin
  case vWorkingWidth < (FThumbSize + THUMB_MARGIN) of TRUE: EXIT; end;         // prevent division by zero
  result := vWorkingWidth div (FThumbSize + THUMB_MARGIN);                     // how many thumbs with right margins can fit?
  vRemainingWidth := vWorkingWidth - (result * (FThumbSize + THUMB_MARGIN));   // how much space is left?
  case vRemainingWidth >= FThumbSize of TRUE: inc(result); end;                // we can fit another thumbnail without a right margin
end;

function TThumbs.thumbRowCount: integer;
var
  vWorkingHeight: integer;
  vRemainingHeight: integer;
begin
  result := 1;
  vWorkingHeight := FThumbsHost.height - THUMB_MARGIN;                         // ignore the top margin
  case vWorkingHeight < (FThumbSize + THUMB_MARGIN) of TRUE: EXIT; end;        // prevent division by zero
  result := vWorkingHeight div (FThumbSize + THUMB_MARGIN);                    // how many thumbs with right margins can fit?
  vRemainingHeight := vWorkingHeight - (result * (FThumbSize + THUMB_MARGIN)); // how much space is left?
  case vRemainingHeight >= FThumbsize of TRUE: inc(result); end;               // we can fit another thumbnail without a bottom margin
end;

function TThumbs.thumbsPerPage: integer;
begin
  result := thumbColCount * thumbRowCount;
end;

end.
