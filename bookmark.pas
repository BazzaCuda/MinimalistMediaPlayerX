unit bookmark;

interface

type
  TBookmark = class(TObject)
  strict private
    function release: boolean;
  protected
  private
  public
    function asInteger: integer;
    function delete:    boolean;
    function save:      boolean;
  end;

function BM: TBookmark;

implementation

uses
  system.sysUtils, playlist, configFile, mediaPlayer, formSubtitles;

var
  gBM: TBookmark;

function BM: TBookmark;
begin
  case gBM = NIL of TRUE: gBM := TBookmark.Create; end;
  result := gBM;
end;

{ TBookmark }

function TBookmark.asInteger: integer;
begin
  result := CF.asInteger[PL.currentItem];
  ST.opInfo := 'From bookmark';
end;

function TBookmark.delete: boolean;
begin
  result := CF.deleteName(PL.currentItem);
  ST.opInfo := 'Bookmark deleted';
end;

function TBookmark.release: boolean;
begin
  freeAndNIL(gBM);
end;

function TBookmark.save: boolean;
begin
  CF.value[PL.currentItem] := intToStr(MP.position);
  ST.opInfo := 'Bookmarked';
  release;
end;

initialization
  gBM := NIL;

finalization
  case gBM <> NIL of TRUE: gBM.free; end;


end.
