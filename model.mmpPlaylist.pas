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
unit model.mmpPlaylist;

interface

uses
  system.classes, system.generics.collections,
  vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  TSetOfMediaType = set of TMediaType;

  IPlaylist = interface
    ['{EAA8F4FC-4F65-4080-B25F-5F5CA08309D9}']
    function    clear:                                      boolean;
    function    copyToClipboard:                            string;
    function    count:                                      integer;
    function    currentFolder:                              string;
    function    currentItem:                                string;
    function    currentIx:                                  integer;
    function    deleteIx(const ix: integer = -1):           boolean;
    function    fillPlaylist(const aFolder: string; const aSetOfMediaType: TSetOfMediaType = [mtAudio, mtVideo, mtImage]): boolean;
    function    find(const anItem: string):                 boolean;
    function    first:                                      boolean;
    function    getNotifier:                                INotifier;
    function    hasItems:                                   boolean;
    function    indexOf(const anItem: string):              integer;
    function    insert(const anItem: string):               boolean;
    function    isFirst:                                    boolean;
    function    isLast:                                     boolean;
    function    last:                                       boolean;
    function    next(const aMediaType: TMediaType = mtUnk): boolean;
    function    notify(const aNotice: INotice):             INotice;
    function    notifyEx(const aNotice: INotice; const aSetOfMediaType: TSetOfMediaType = [mtAudio, mtVideo, mtImage]): INotice;
    function    prev:                                       boolean;
    function    replaceCurrentItem(const aNewItem: string): boolean;
    function    setIx(const ix: integer):                   integer;
    function    thisItem(const ix: integer):                string;
    function    validIx(const ix: integer):                 boolean;

    property    notifier:                                   INotifier     read getNotifier;
  end;

function newPlaylist: IPlaylist;

implementation

uses
  winApi.windows,
  system.regularExpressions, system.sysUtils,
  vcl.clipbrd,
  mmpFileUtils, mmpUtils,
  model.mmpMediaTypes,
  TListHelperClass,
  _debugWindow;

type
  TPlaylist = class(TInterfacedObject, IPlaylist)
  strict private
    FCurrentFolder: string;
    FNotifier:      INotifier;
    FPlayIx:        integer;
    FPlaylist:      TList<string>;
    FSubscriber:    ISubscriber;
  private
    function    add(const anItem: string):          boolean;
    function    displayItem:                        string;
    function    extractNumericPart(const aString: string): integer;
    function    formattedItem:                      string;
    function    getPlaylist(aListBox: TListBox):    boolean;
    function    isSpecialImage:                     boolean;
    function    onNotify(const aNotice: INotice):   INotice;
    function    sort:                               boolean;
  protected
    procedure   setCurrentFolder(const aValue: string);
  public
    constructor create;
    destructor  Destroy; override;
    function    clear:                                      boolean;
    function    copyToClipboard:                            string;
    function    count:                                      integer;
    function    currentFolder:                              string;
    function    currentItem:                                string;
    function    currentIx:                                  integer;
    function    deleteIx(const ix: integer = -1):           boolean;
    function    fillPlaylist(const aFolder: string; const aSetOfMediaType: TSetOfMediaType = [mtAudio, mtVideo, mtImage]): boolean;
    function    find(const anItem: string):                 boolean;
    function    first:                                      boolean;
    function    getNotifier:                                INotifier;
    function    hasItems:                                   boolean;
    function    indexOf(const anItem: string):              integer;
    function    insert(const anItem: string):               boolean;
    function    isFirst:                                    boolean;
    function    isLast:                                     boolean;
    function    last:                                       boolean;
    function    next(const aMediaType: TMediaType = mtUnk): boolean;
    function    notify(const aNotice: INotice):             INotice;
    function    notifyEx(const aNotice: INotice; const aSetOfMediaType: TSetOfMediaType = [mtAudio, mtVideo, mtImage]): INotice;
    function    prev:                                       boolean;
    function    replaceCurrentItem(const aNewItem: string): boolean;
    function    setIx(const ix: integer):                   integer;
    function    thisItem(const ix: integer):                string;
    function    validIx(const ix: integer):                 boolean;
  end;

function newPlaylist: IPlaylist;
begin
  result := TPlaylist.create;
end;

{ TPlaylist }

function TPlaylist.add(const anItem: string): boolean;
begin
  result := FPlayList.add(anItem) <> 0;
end;

function TPlaylist.clear: boolean;
begin
  FPlaylist.clear;
  FPlayIx := -1;
  result  := FPlaylist.count = 0;
end;

function TPlaylist.copyToClipboard: string;
begin
  result := '';
  clipboard.AsText := mmpFileNameWithoutExtension(currentItem);
  result := 'Copied to clipboard';
end;

function TPlaylist.count: integer;
begin
  result := FPlaylist.count;
end;

constructor TPlaylist.create;
begin
  inherited;
  FPlaylist   := TList<string>.create;
  FPLaylist.sort;
  FSubscriber := appNotifier.subscribe(newSubscriber(onNotify));
end;

function TPlaylist.currentFolder: string;
begin
  result := FCurrentFolder;
end;

function TPlaylist.currentItem: string;
begin
  result := '';
  case FPlayIx = -1 of TRUE: EXIT; end;
  result := FPlaylist[FPlayIx];
end;

function TPlaylist.currentIx: integer;
begin
  result := FPlayIx;
end;

function TPlaylist.deleteIx(const ix: integer = -1): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case ix = -1 of  TRUE:  begin
                            FPlaylist.delete(FPlayIx);
                            dec(FPlayIx); end;
                  FALSE:  begin
                            case (ix < 0) or (ix > FPlaylist.count - 1) of TRUE: EXIT; end;
                            FPlaylist.delete(ix);
                            dec(FPlayIx); end;end;

  case (FPlayIx < 0) and (FPlaylist.count > 0) of TRUE: FPlayIx := 0; end; // the item at index 0 was deleted so point to the new item[0]
  result := TRUE;
end;

destructor TPlaylist.Destroy;
begin
  appNotifier.unsubscribe(FSubscriber);
  case FPlaylist <> NIL of TRUE: FPlaylist.free; end;
  inherited;
end;

function TPlaylist.displayItem: string;
begin
  result := format('[%d/%d] %s', [FPlayIx, count, extractFileName(currentItem)]);
end;

function TPlaylist.extractNumericPart(const aString: string): Integer;
var
  vMatch: TMatch;
begin
  // Use a regular expression to extract the numeric part from the string
  vMatch := TRegEx.match(aString, '\d+');
  if vMatch.success then
    result := strToIntDef(vMatch.value, 0)
  else
    result := 0;
end;

function TPlaylist.fillPlaylist(const aFolder: string; const aSetOfMediaType: TSetOfMediaType = [mtAudio, mtVideo, mtImage]): boolean;
const
  faFilesOnly = faAnyFile AND NOT faDirectory {AND NOT faHidden} AND NOT faSysFile;
var
  vSR: TSearchRec;

  function fileExtOK: boolean;
  begin
    var vMT := MT.mediaType(vSR.name);
    result  := (vMT <> mtUnk) and ((mtUnk in aSetOfMediaType) or (vMT in aSetOfMediaType));
  end;

begin
  result := FALSE;
  clear;
  case aFolder = '' of TRUE: EXIT; end;
  case directoryExists(aFolder) of FALSE: EXIT; end;
  FCurrentFolder := aFolder;

  case findFirst(aFolder + '*.*', faFilesOnly, vSR) = 0 of  TRUE:
    repeat
      case fileExtOK of TRUE: add(aFolder + vSR.Name); end;
    until findNext(vSR) <> 0;
  end;

  system.sysUtils.findClose(vSR);
  sort;

  case hasItems of  TRUE: FPlayIx := 0;
                   FALSE: FPlayIx := -1; end;

  result := hasItems;
  notifyApp(newNotice(evPLNewPlaylist));
end;

function TPlaylist.find(const anItem: string): boolean;
begin
  FPlayIx := FPlaylist.indexOf(anItem);
  result  := FPlayIx <> -1;
end;

function TPlaylist.first: boolean;
begin
  result := FALSE;
  case hasItems of  TRUE: FPlayIx := 0;
                   FALSE: FPlayIx := -1; end;
  result := FPlayIx = 0;
end;

function TPlaylist.formattedItem: string;
begin
  case hasItems of FALSE: EXIT; end;
  result := format('[%d/%d] %s', [FPlayIx + 1, FPlaylist.count, extractFileName(currentItem)]);
end;

function TPlaylist.getNotifier: INotifier;
begin
  case FNotifier = NIL of TRUE: FNotifier := newNotifier; end;
  result := FNotifier;
end;

function TPlaylist.getPlaylist(aListBox: TListBox): boolean;
var i: integer;
begin
  result := FALSE;

  aListBox.items.beginUpdate; // prevent flicker when moving the window
  try
    aListBox.clear;

    for i := 0 to FPlaylist.count - 1 do
      aListBox.items.add(extractFileName(FPlaylist[i]));
  finally
    aListBox.items.endUpdate;
  end;

  result := aListBox.count > 0;
end;

function TPlaylist.hasItems: boolean;
begin
  result := FPlaylist.count > 0;
end;

function TPlaylist.indexOf(const anItem: string): integer;
begin
  result := FPlaylist.indexOf(anItem);
end;

function TPlaylist.insert(const anItem: string): boolean;
// insert at FPlayIx + 1, after the current item
begin
  result := FALSE;
  case isLast of   TRUE: FPlaylist.add(anItem);
                  FALSE: FPlaylist.insert(FPlayIx, anItem); end;
  result := FPlaylist.count > 0;
end;

function TPlaylist.isFirst: boolean;
begin
  result := FPlayIx = 0;
end;

function TPlaylist.isLast: boolean;
begin
  result := FPlayIx = FPlaylist.count - 1;
end;

function TPlaylist.isSpecialImage: boolean;
begin
  result := FALSE;
  var vExt := lowerCase(extractFileExt(currentItem)) + ' ';
  result := '.avif .webp .png '.contains(vExt);
end;

function TPlaylist.last: boolean;
begin
  result := FALSE;
  case hasItems of   TRUE: FPlayIx := FPlaylist.count - 1;
                    FALSE: FPlayIx := -1; end;
  result := FPlayIx <> -1;
end;

function TPlaylist.next(const aMediaType: TMediaType = mtUnk): boolean;

  function findNext: boolean;
  var vMediaType: TMediaType;
  begin
    result := FALSE;
    repeat
      inc(FPlayIx);
      vMediaType := MT.mediaType(currentItem);
      case isLast and NOT (aMediaType in [mtUnk, vMediaType]) of TRUE: EXIT; end;
    until (aMediaType in [mtUnk, vMediaType]) or isLast; // order of shortcut logic is important here
    result := TRUE;
  end;

begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case isLast   of TRUE:  EXIT; end;
  case findNext of FALSE: EXIT; end;
  result := TRUE;
end;

function TPlaylist.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TPlaylist.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evPLCopyToClipboard:    aNotice.text  := copyToClipboard;
    evPLDeleteIx:           aNotice.tf    := deleteIx(aNotice.integer);
    evPLFillPlaylist:       fillPlaylist(aNotice.text, [aNotice.mediaType]);
    evPLFillListbox:        getPlaylist(aNotice.component as TListBox);
    evPLFind:               aNotice.tf    := find(aNotice.text);
    evPLFirst:              aNotice.tf    := first;
    evPLLast:               aNotice.tf    := last;
    evPLNext:               aNotice.tf    := next(aNotice.mediaType);
    evPLPrev:               aNotice.tf    := prev;
    evPLReplaceCurrentItem: replaceCurrentItem(aNotice.text);

    evPLReqCurrentFolder:   aNotice.text    := currentFolder;
    evPLReqCurrentIx:       aNotice.integer := FPlayIx;
    evPLReqHasItems:        aNotice.tf      := hasItems;
    evPLReqIsLast:          aNotice.tf      := isLast;
    evPLReqIsSpecialImage:  aNotice.tf      := isSpecialImage;
    evPLReqThisItem:        aNotice.text    := thisItem(aNotice.integer);
    evPLReqCurrentItem:     aNotice.text    := currentItem;
    evPLReqFormattedItem:   aNotice.text    := formattedItem; // return to sender
  end;
  result := aNotice;
end;

function TPlaylist.notifyEx(const aNotice: INotice; const aSetOfMediaType: TSetOfMediaType): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evPLFillPlaylist: fillPlaylist(aNotice.text, aSetOfMediaType);
  end;
end;

function TPlaylist.prev: boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case isFirst of TRUE: EXIT; end;
  dec(FPlayIx);
  result := TRUE;
end;

function TPlaylist.replaceCurrentItem(const aNewItem: string): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  FPlaylist[FPlayIx] := aNewItem;
  result := TRUE;
end;

procedure TPlaylist.setCurrentFolder(const aValue: string);
begin
  FCurrentFolder := aValue;
//  FNotifier.notifySubscribers(newNotice(evPLCurrentFolder, aValue)); // CHECK THIS
//  notifyApp(newNotice(evPLCurrentFolder, aValue));
end;

function TPlaylist.setIx(const ix: integer): integer;
begin
  result := -1;
  case validIx(ix) of  TRUE: FPlayIx := ix;
                      FALSE: first; end;
  result := FPlayIx;
end;

function TPlaylist.sort: boolean;
begin
  result := FALSE;

  FPlaylist.naturalSort;

  result := TRUE;
end;

function TPlaylist.thisItem(const ix: integer): string;
begin
  result := '';
  case hasItems of FALSE: EXIT; end;
  case (ix < 0) or (ix > FPlaylist.count - 1) of TRUE: EXIT; end;
  result := FPlaylist[ix];
end;

function TPlaylist.validIx(const ix: integer): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case (ix < 0) or (ix > FPlaylist.count - 1) of TRUE: EXIT; end;
  result := TRUE;
end;

end.
