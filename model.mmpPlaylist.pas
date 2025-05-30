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
  IPlaylist = interface
    ['{EAA8F4FC-4F65-4080-B25F-5F5CA08309D9}']
    function    add(const anItem: string):                  boolean;
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
    function    hasItems:                                   boolean;
    function    indexOf(const anItem: string):              integer;
    function    insert(const anItem: string):               boolean;
    function    isFirst:                                    boolean;
    function    isLast:                                     boolean;
    function    last:                                       boolean;
    function    next(const aSetOfMediaType: TSetOfMediaType = [mtUnk]; const bShuffle: boolean = FALSE): boolean;
    function    nextIx:                                     boolean;
    function    notify(const aNotice: INotice):             INotice;
    function    prev(const aSetOfMediaType: TSetOfMediaType = [mtUnk]): boolean;
    function    sort:                                       boolean;
    function    replaceCurrentItem(const aNewItem: string): boolean;
    function    setIx(const ix: integer):                   integer;
    function    thisItem(const ix: integer):                string;
    function    validIx(const ix: integer):                 boolean;
  end;

function newPlaylist: IPlaylist;

implementation

uses
  winApi.windows,
  system.regularExpressions, system.sysUtils,
  vcl.clipbrd,
  mmpFileUtils, mmpGlobalState, mmpFuncProg, mmpUtils,
  model.mmpMediaTypes,
  TListHelperClass,
  _debugWindow;

type
  TPlaylist = class(TInterfacedObject, IPlaylist)
  strict private
    FCurrentFolder: string;
    FPlayIx:        integer;
    FPlaylist:      TList<string>;
    FSubscriber:    ISubscriber;
  private
    function    add(const anItem: string):          boolean;
    function    displayItem:                        string;
    function    expandSetOfMediaType(const aSetOfMediaType: TSetOfMediaType): TSetOfMediaType;
    function    extractNumericPart(const aString: string): integer;
    function    formattedItem:                      string;
    function    getPlaylist(aListBox: TListBox):    boolean;
    function    isSpecialImage:                     boolean;
    function    onNotify(const aNotice: INotice):   INotice;
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
    function    hasItems:                                   boolean;
    function    indexOf(const anItem: string):              integer;
    function    insert(const anItem: string):               boolean;
    function    isFirst:                                    boolean;
    function    isLast:                                     boolean;
    function    last:                                       boolean;
    function    next(const aSetOfMediaType: TSetOfMediaType = [mtUnk]; const bShuffle: boolean = FALSE): boolean;
    function    nextIx:                                     boolean;
    function    notify(const aNotice: INotice):             INotice;
    function    prev(const aSetOfMediaType: TSetOfMediaType = [mtUnk]): boolean;
    function    replaceCurrentItem(const aNewItem: string): boolean;
    function    setIx(const ix: integer):                   integer;
    function    sort:                                       boolean;
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
  result := FALSE;
  case FPlaylist.indexOf(anItem) = -1 of FALSE: EXIT; end;
  case fileExists(anItem) of FALSE: EXIT; end;
  FCurrentFolder := extractFilePath(anItem);
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
  FSubscriber := appEvents.subscribe(newSubscriber(onNotify));
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
  appEvents.unsubscribe(FSubscriber);
  FSubscriber := NIL;
  case FPlaylist <> NIL of TRUE: FPlaylist.free; end;
  inherited;
end;

function TPlaylist.displayItem: string;
begin
  result := format('[%d/%d] %s', [FPlayIx, count, extractFileName(currentItem)]);
end;

function TPlaylist.expandSetOfMediaType(const aSetOfMediaType: TSetOfMediaType): TSetOfMediaType;
begin
  result := aSetOfMediaType;
  case mtAudioVideo in result of TRUE: result := result - [mtAudioVideo] + [mtAudio, mtVideo]; end;
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
// in the context of the playlist, mtUnk is a synonym for mtAny except when it's being used to identify unsupported file types
const
  faFilesOnly = faAnyFile AND NOT faDirectory {AND NOT faHidden} AND NOT faSysFile;
var
  vSR: TSearchRec;
  vSetOfMediaType: TSetOfMediaType;

  function fileExtOK: boolean;
  begin
    var vMT := MT.mediaType(vSR.name);
    result  := (vMT <> mtUnk) and ((mtUnk in vSetOfMediaType) or (vMT in vSetOfMediaType));
  end;

begin
  result := FALSE;

  vSetOfMediaType := expandSetOfMediaType(aSetOfMediaType);

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
  mmp.cmd(evPLNewPlaylist);
end;

function TPlaylist.find(const anItem: string): boolean;
begin
  FPlayIx := FPlaylist.indexOf(anItem);
  result  := FPlayIx <> -1;
end;

function TPlaylist.first: boolean;
begin
  result := FALSE;
  FPlayIx := -ord(NOT hasItems); // TRUE = 0, FALSE = -1, a reversal of their ordinal values
  result := FPlayIx = 0;
end;

function TPlaylist.formattedItem: string;
begin
  case hasItems of FALSE: EXIT; end;
  result := format('[%d/%d] %s', [FPlayIx + 1, FPlaylist.count, extractFileName(currentItem)]);
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

function TPlaylist.isSpecialImage: boolean; // I don't remember what this was all about - it's not used
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

function TPlaylist.next(const aSetOfMediaType: TSetOfMediaType = [mtUnk]; const bShuffle: boolean = FALSE): boolean;
// in the context of the playlist, mtUnk is a synonym for mtAny except when it's being used to identify unsupported file types
var vSetOfMediaType: TSetOfMediaType;

  function findNext: boolean;
  var vMediaType: TMediaType;
  begin
    result := FALSE;
    repeat
      inc(FPlayIx);
      vMediaType := MT.mediaType(currentItem);
      case isLast and NOT (mtUnk in vSetOfMediaType) and NOT (vMediaType in vSetOfMediaType) of TRUE: EXIT; end; // bypass result := TRUE if the final iteration fails to find a match
    until (mtUnk in vSetOfMediaType) or (vMediaType in vSetOfMediaType);
    result := TRUE;
  end;

  function findRandom: boolean;
  begin
    result := FALSE;

    // compile a list of FPlaylist indexes that match the required mediaType
    var vIxArray: TArray<integer>;
    setLength(vIxArray, 0);
    for var i := 0 to FPlaylist.count - 1 do
      case (mtUnk in vSetOfMediaType) or (MT.mediaType(FPlaylist[i]) in vSetOfMediaType) of TRUE: begin
                                                                                                    setLength(vIxArray, length(vIxArray) + 1);
                                                                                                    vIxArray[high(vIxArray)] := i; end;end;
    case length(vIxArray) = 0 of TRUE: EXIT; end;

    var vIx := random(length(vIxArray)); // pick a random ix OF vIxArray
    FPlayIx := vIxArray[vIx];            // use the FPlaylist.itemIndex stored in it
    result := TRUE;
  end;

begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;

  vSetOfMediaType := expandSetOfMediaType(aSetOfMediaType);

//  for var vMediaType: TMediaType := low(TMediaType) to high(TMediaType) do
//    case vMediaType in vSetOfMediaType of TRUE: TDebug.debugEnum<TMediaType>('media type', vMediaType); end;

  case bShuffle of   TRUE:  case findRandom of FALSE: EXIT; end;
                    FALSE:  begin
                              case isLast   of TRUE:  EXIT; end;
                              case findNext of FALSE: EXIT; end;end;end;
  result := TRUE;
end;

function TPlaylist.nextIx: boolean;
begin
  case FPlayIx + 1 > FPlaylist.count - 1 of TRUE: EXIT; end;
  inc(FPlayIx);
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
    evPLAddItem:            add(aNotice.text);
    evPLCopyToClipboard:    aNotice.text  := copyToClipboard;
    evPLDeleteIx:           aNotice.tf    := deleteIx(aNotice.integer);
    evPLFillPlaylist:       fillPlaylist(aNotice.text, [aNotice.mediaType]);
    evPLFillListbox:        getPlaylist(aNotice.component as TListBox);
    evPLFind:               aNotice.tf    := find(aNotice.text);
    evPLFirst:              aNotice.tf    := first;
    evPLLast:               aNotice.tf    := last;
    evPLNext:               aNotice.tf    := next([aNotice.mediaType], GS.shuffle);
    evPLPrev:               aNotice.tf    := prev([aNotice.mediaType]);
    evPLReplaceCurrentItem: replaceCurrentItem(aNotice.text);
    evPLSetNoItem:          FPlayIx       := -1;

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

function TPlaylist.prev(const aSetOfMediaType: TSetOfMediaType = [mtUnk]): boolean;
// in the context of the playlist, mtUnk is a synonym for mtAny except when it's being used to identify unsupported file types
var vSetOfMediaType: TSetOfMediaType;

  function findPrev: boolean;
  var vMediaType: TMediaType;
  begin
    result := FALSE;
    repeat
      dec(FPlayIx);
      vMediaType := MT.mediaType(currentItem);
      case isFirst and NOT (mtUnk in vSetOfMediaType) and NOT (vMediaType in vSetOfMediaType) of TRUE: EXIT; end; // bypass result := TRUE if the final iteration fails to find a match
    until (mtUnk in vSetOfMediaType) or (vMediaType in vSetOfMediaType);
    result := TRUE;
  end;

begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case isFirst  of TRUE:  EXIT; end;

  vSetOfMediaType := expandSetOfMediaType(aSetOfMediaType);

  case findPrev of FALSE: EXIT; end;
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

initialization
  randomize;

end.
