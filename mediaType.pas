unit mediaType;

interface

uses
  consts;

type

  TMediaTypes = class(TObject)
  strict private
    FMediaExts: string;
    function getMediaExts: string;
  protected
  private
  public
    constructor Create;
    destructor  Destroy;  override;
    function mediaType(const aExt: string): TMediaType;
    property mediaExts: string read FMediaExts;
  end;

function MT: TMediaTypes;

implementation

uses
  system.sysUtils;

var
  gMT: TMediaTypes;

function MT: TMediaTypes;
begin
  case gMT = NIL of TRUE: gMT := TMediaTypes.Create; end;
  result := gMT;
end;

{ TMediaType }

constructor TMediaTypes.Create;
begin
  inherited;
  getMediaExts;
end;

destructor TMediaTypes.Destroy;
begin
  inherited;
end;

function TMediaTypes.getMediaExts: string;
begin
  result := FMediaExts;
  case result <> '' of TRUE: EXIT; end;
  for var i := 0 to high(mediaTypes) do
    FMediaExts := FMediaExts + mediaTypes[i].fileExts;
  result := FMediaExts;
end;

function TMediaTypes.mediaType(const aExt: string): TMediaType;
begin
  result := mtUnk;
  for var i := 0 to high(mediaTypes) do
    case mediaTypes[i].fileExts.contains(aExt) of TRUE: begin
                                                          result := mediaTypes[i].mediaType;
                                                          EXIT; end;end;
end;

initialization
  gMT := NIL;

finalization
  case gMT <> NIL of TRUE: gMT.free; end;
end.
