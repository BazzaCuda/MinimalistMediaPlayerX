{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit TMediaInfoClass;

interface

uses
  vcl.stdCtrls, generics.collections;

type
  TMediaStream = class(TObject)
  strict private
  private
    FID:          string;
    FBitRate:     string;
    FDuration:    string;
    FFormat:      string;
    FLanguage:    string;
    FStreamType:  string;
    FTitle:       string;
    FVideoWidth:  string;
    FVideoHeight: string;

    FIconIx: integer;
    FInfo:        string;
    FSelected: boolean;
  protected
    constructor create(const aID: string; const aStreamType: string; const aDuration: string; const aFormat: string; const aBitRate: string; const aTitle: string; const aLanguage: string; const aInfo: string; const aIconIx: integer);
  public
    property ID:          string read FID write FID;
    property streamType:  string read FStreamType write FStreamType;
    property duration:    string read FDuration write FDuration;
    property bitRate:     string read FBitRate write FBitRate;
    property format:      string read FFormat write FFormat;
    property title:       string read FTitle write FTitle;
    property language:    string read FLanguage write FLanguage;
    property videoHeight: string read FVideoHeight write FVideoHeight;
    property videoWidth:  string read FVideoWidth write FVideoWidth;

    property iconIx:      integer read FIconIx write FIconIx;
    property info:        string read FInfo write FInfo;
    property selected:    boolean read FSelected write FSelected;
  end;

  TMediaChapter = class(TObject)
  private
    FChapterTitle:   string;
    FChapterStartSS: integer;
    FChapterEndSS: integer;
  public
    property chapterTitle:   string  read FChapterTitle   write FChapterTitle;
    property chapterStartSS: integer read FChapterStartSS write FChapterStartSS;
    property chapterEndSS:   integer read FChapterEndSS   write FChapterEndSS;
  end;

  TMediaInfo = class(TObject)
  private
    FAudioBitRate: integer;
    FAudioCount: integer;
    FFileSize: int64;
    FGeneralCount: integer;
    FHasCoverArt: string;
    FHeight: integer;
    FImageCount: integer;
    FImageHeight: integer;
    FImageWidth: integer;
    FOtherCount: integer;
    FOverallBitRate: integer;
    FOverallFrameRate: string;
    FStereoMono: string;
    FTextCount: integer;
    FVideoBitRate: integer;
    FVideoCount: integer;
    FWidth: integer;

    FChapterCount: integer;
    FDuration: integer;
    FLowestID: integer;
    FMediaChapters: TObjectList<TMediaChapter>;
    FMediaStreams:  TObjectList<TMediaStream>;
    FNeedInfo: boolean;
    FURL: string;

    function getAudioBitRate: string;
    function getDuration: integer;
    function getFileSize: string;
    function getHasCoverArt: boolean;
    function getSelectedCount: integer;
    function getStereoMono: string;
    function getStreamCount: integer;
    function getOverallFrameRate: string;
    function getOverallBitRate: string;
    function getVideoBitRate: string;
    function getXY: string;
    procedure setURL(const aValue: string);
  protected
    constructor create;
    destructor destroy; override;
  public
    function getData(const aMemo: TMemo): boolean;
    function initMediaInfo(const aURL: string = ''): boolean;
    property audioBitRate:      string  read getAudioBitRate;
    property audioCount:        integer read FAudioCount;
    property chapterCount:      integer read FChapterCount;
    property duration:          integer read getDuration;
    property fileSize:          string  read getFileSize;
    property generalCount:      integer read FGeneralCount;
    property hasCoverArt:       boolean read getHasCoverArt;
    property imageCount:        integer read FImageCount;
    property imageHeight:       integer read FImageHeight;
    property imageWidth:        integer read FImageWidth;
    property mediaChapters:     TObjectList<TMediaChapter> read FMediaChapters;
    property mediaStreams:      TObjectList<TMediaStream>  read FMediaStreams;
    property otherCount:        integer read FOtherCount;
    property overallBitRate:    string  read getOverallBitRate;
    property overallFrameRate:  string  read getOverallFrameRate;
    property stereoMono:        string  read getStereoMono;
    property streamCount:       integer read getStreamCount;
    property textCount:         integer read FTextCount;
    property URL:               string  read FURL         write setURL;
    property videoBitRate:      string  read getVideoBitRate;
    property videoCount:        integer read FVideoCount;
    property X:                 integer read FWidth;
    property Y:                 integer read FHeight;
    property XY:                string  read getXY;

    property lowestID:          integer read FLowestID write FLowestID;
    property selectedCount:     integer read getSelectedCount;
  end;

function MI: TMediaInfo;

implementation

uses
  mediaInfoDLL, system.sysUtils, commonUtils, system.timeSpan, _debugWindow;

var
  gMI: TMediaInfo;

function MI: TMediaInfo;
begin
  case gMI = NIL of TRUE: gMI := TMediaInfo.create; end;
  result := gMI;
end;

{ TMediaInfo }

constructor TMediaInfo.create;
begin
  inherited;
  FMediaStreams := TObjectList<TMediaStream>.create;
  FMediaStreams.ownsObjects := TRUE;
  FMediaChapters := TObjectList<TMediaChapter>.create;
  FMediaChapters.ownsObjects := TRUE;
end;

destructor TMediaInfo.destroy;
begin
  FMediaStreams.free;
  FMediaChapters.free;
  inherited;
end;

function TMediaInfo.getAudioBitRate: string;
begin
  result := format('AR:  %d Kb/s', [round(FAudioBitRate / 1000)]);
end;

function TMediaInfo.getHasCoverArt: boolean;
begin
  result := FHasCoverArt = 'Yes';
end;

function TMediaInfo.getData(const aMemo: TMemo): boolean;
begin
  case FNeedInfo of TRUE: initMediaInfo(FURL); end;
  aMemo.clear;
  aMemo.lines.add('');
  aMemo.lines.add(XY);
  aMemo.lines.add(overallFrameRate);
  aMemo.lines.add(overallBitRate);
  aMemo.lines.add(audioBitRate);
  aMemo.lines.add(videoBitRate);
  aMemo.lines.add(stereoMono);
  aMemo.lines.add(fileSize);
end;

function TMediaInfo.getDuration: integer;
begin
  result := FDuration div 1000;
end;

function TMediaInfo.getFileSize: string;
begin
  result := CU.formatFileSize(CU.getFileSize(FURL));
end;

function TMediaInfo.getOverallBitRate: string;
begin
  result := format('BR:  %d Kb/s', [round(FOverallBitRate / 1000)]);
end;

function TMediaInfo.getOverallFrameRate: string;
begin
  case FOverallFrameRate = '' of  TRUE: result := 'FR:';
                                 FALSE: result := format('FR:  %s fps', [FOverallFrameRate]); end;
end;

function TMediaInfo.getSelectedCount: integer;
begin
  result := 0;
  for var vMediaStream in FMediaStreams do case vMediaStream.selected of TRUE: inc(result); end;
end;

function TMediaInfo.getStereoMono: string;
begin
  result := 'SM:  ' + FStereoMono;
end;

function TMediaInfo.getStreamCount: integer;
begin
  result := FGeneralCount + FVideoCount + FAudioCount + FTextCount + FOtherCount + FImageCount;
end;

function TMediaInfo.getVideoBitRate: string;
begin
  result := format('VR:  %d Kb/s', [round(FVideoBitRate / 1000)]);
end;

function TMediaInfo.getXY: string;
begin
  result := format('XY:  %d x %d', [X, Y]);
end;

function TMediaInfo.initMediaInfo(const aURL: string = ''): boolean;
var
  handle: THandle;
  vBitRate:     string;
  vDuration:    string;
  vFormat:      string;
  vLanguage:    string;
  vTitle:       string;

  vID:          string;
  vInfo:        string;
  vStreamType:  string;

  function createVideoStream(aStreamIx: integer): boolean;
  begin
//    debug(mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Format_Settings_RefFrames',         Info_Text, Info_Name));

    vBitRate    := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'BitRate/String',         Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Duration/String5',       Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Format',                 Info_Text, Info_Name);
    vID         := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'ID',                     Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Language/String',        Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Title',                  Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'StreamKind',             Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Width',                  Info_Text, Info_Name) + 'x' + mediaInfo_Get(handle, Stream_Video, aStreamIx, 'Height', Info_Text, Info_Name) + ' ' + mediaInfo_Get(handle, Stream_Video, aStreamIx, 'FrameRate', Info_Text, Info_Name) + ' fps';

    vBitRate := stringReplace(vBitRate, ' ', ',', []);
    case pos(' (', vDuration) > 1 of TRUE: vDuration := copy(vDuration, 1, pos(' (', vDuration) - 1); end;

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 0));
  end;

  function createAudioStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'BitRate/String',         Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'Duration/String5',       Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'Format',                 Info_Text, Info_Name);
    vID         := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'ID',                     Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'Language/String',        Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'Title',                  Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'StreamKind',             Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(handle, Stream_Audio, aStreamIx, 'SamplingRate/String',    Info_Text, Info_Name);

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 2));
  end;

  function createTextStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'BitRate/String',          Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'Duration/String5',        Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'Format',                  Info_Text, Info_Name);
    vID         := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'ID',                      Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'Language/String',         Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'Title',                   Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(handle, Stream_Text, aStreamIx, 'StreamKind',              Info_Text, Info_Name);
    vInfo       := '';

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 4));
  end;

  function cleanChapterTitle(const aChapterTitle: string): string;
  begin
    result := aChapterTitle;
    var posColon := pos(':', result);
    case posColon > 0 of TRUE: delete(result, 1, posColon); end;
  end;

  function cleanChapterStartSS(const aStartSS: string): integer;
  begin
    try
      var vTimeSpan := TTimeSpan.parse(aStartSS);
      result := trunc(vTimeSpan.totalSeconds);
    except
      result := 0;
    end;
  end;

  function calcChapterEndSS: boolean;
  begin
    for var i := 0 to chapterCount - 1 do
      case i = chapterCount - 1 of  TRUE: FMediaChapters[i].chapterEndSS := duration;
                                   FALSE: FMediaChapters[i].chapterEndSS := FMediaChapters[i + 1].chapterStartSS - 1; end;
  end;

  function createChapters: boolean;
  begin
    var chapterBegin: integer; var chapterEnd: integer;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Menu,         0, 'Chapters_Pos_Begin',  Info_Text, Info_Name), chapterBegin) of FALSE: chapterBegin := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Menu,         0, 'Chapters_Pos_End',    Info_Text, Info_Name), chapterEnd)   of FALSE: chapterEnd   := 0; end;

    case chapterEnd > chapterBegin of  TRUE: FChapterCount := chapterEnd - chapterBegin;
                                      FALSE: FChapterCount := 0; end;
    case chapterCount = 0 of TRUE: EXIT; end;

    FMediaChapters.clear;
    for var i := chapterBegin to chapterEnd - 1 do begin
      var vChapter := TMediaChapter.create;
      FMediaChapters.add(vChapter);
      vChapter.chapterTitle   := cleanChapterTitle(string(MediaInfo_GetI(handle, Stream_Menu, 0, i, Info_Text))); // title
      vChapter.chapterStartSS := cleanChapterStartSS(MediaInfo_GetI(handle, Stream_Menu, 0, i, Info_Name));       // position
    end;

    case FMediaChapters[0].chapterStartSS <> 0 of TRUE: begin
                                                          FMediaChapters.insert(0, TMediaChapter.create);
                                                          inc(FChapterCount); end;end;

    calcChapterEndSS;
  end;

begin
  result := FALSE;
  case mediaInfoDLL_Load('MediaInfo.dll') of FALSE: EXIT; end;
  mediaInfo_Option(0, 'Internet', 'No');
  handle := MediaInfo_New();
  case handle = 0 of TRUE: EXIT; end;
  try
    case aURL <> '' of TRUE: FURL := aURL; end;
    mediaInfo_Open(handle, PWideChar(FURL));
    FMediaStreams.clear;

    FOverallFrameRate := mediaInfo_Get(handle, Stream_General,  0, 'FrameRate',       Info_Text, Info_Name);
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'OverallBitRate',  Info_Text, Info_Name), FOverallBitRate)    of FALSE: FOverallBitRate   := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'Duration',        Info_Text, Info_Name), FDuration)          of FALSE: FDuration         := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Audio,        0, 'BitRate',         Info_Text, Info_Name), FAudioBitRate)      of FALSE: FAudioBitRate     := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'Width',           Info_Text, Info_Name), FWidth)             of FALSE: FWidth            := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'Height',          Info_Text, Info_Name), FHeight)            of FALSE: FHeight           := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'BitRate',         Info_Text, Info_Name), FVideoBitRate)      of FALSE: FVideoBitRate     := 0; end;

    case tryStrToInt(mediaInfo_Get(handle, Stream_Image,        0, 'Width',           Info_Text, Info_Name), FImageWidth)        of FALSE: FImageWidth       := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Image,        0, 'Height',          Info_Text, Info_Name), FImageHeight)       of FALSE: FImageHeight      := 0; end;

    FStereoMono := mediaInfo_Get(handle, Stream_Audio,    0, 'Title',        Info_Text, Info_Name);

    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'GeneralCount',    Info_Text, Info_Name), FGeneralCount)      of FALSE: FGeneralCount     := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'VideoCount',      Info_Text, Info_Name), FVideoCount)        of FALSE: FVideoCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'AudioCount',      Info_Text, Info_Name), FAudioCount)        of FALSE: FAudioCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'TextCount',       Info_Text, Info_Name), FTextCount)         of FALSE: FTextCount        := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'OtherCount',      Info_Text, Info_Name), FOtherCount)        of FALSE: FOtherCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'ImageCount',      Info_Text, Info_Name), FImageCount)        of FALSE: FImageCount       := 0; end;

    FHasCoverArt := mediaInfo_Get(handle, Stream_General, 0, 'Cover',        Info_Text, Info_Name);

    for var vStreamIx := 0 to streamCount - 1 do begin
      case mediaInfo_Get(handle, Stream_Video, vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createVideoStream(vStreamIx); end;
      case mediaInfo_Get(handle, Stream_Audio, vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createAudioStream(vStreamIx); end;
      case mediaInfo_Get(handle, Stream_Text,  vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createTextStream(vStreamIx); end;
    end;

    createChapters;

    FNeedInfo := FALSE;
  finally
    mediaInfo_close(handle);
  end;
end;

procedure TMediaInfo.setURL(const aValue: string);
begin
  FNeedInfo := aValue <> FURL;
  FURL      := aValue;
end;

{ TMediaStream }

constructor TMediaStream.create(const aID, aStreamType, aDuration, aFormat, aBitRate, aTitle, aLanguage, aInfo: string; const aIconIx: integer);
begin
  FID         := aID;
  FStreamType := aStreamType;
  FDuration   := aDuration;
  FFormat     := aFormat;
  FBitRate    := aBitRate;
  FTitle      := aTitle;
  FLanguage   := aLanguage;
  FInfo       := aInfo;

  FIconIx     := aIconIx;
  FSelected   := TRUE;
end;

initialization
  gMI := NIL;

finalization
  case gMI <> NIL of TRUE: gMI.free; end;

end.
