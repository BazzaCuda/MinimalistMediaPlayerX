{   MMP: Minimalist Media Player
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
unit TMediaInfoClass;

interface

uses
  system.generics.collections,
  vcl.stdCtrls,
  TMediaStreamClass;

type
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
    FHandle: THandle;
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
    function loadDLL: boolean;
  public
    constructor create;
    destructor Destroy; override;
    function getMediaInfo(const aURL: string = ''): boolean;
    function getMetaData(const aMemo: TMemo): boolean;
    function sortStreams: boolean;
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
    property videoBitRate:      string  read getVideoBitRate;
    property videoCount:        integer read FVideoCount;
    property X:                 integer read FWidth;
    property Y:                 integer read FHeight;
    property XY:                string  read getXY;

    property lowestID:          integer read FLowestID write FLowestID;
    property selectedCount:     integer read getSelectedCount;
  end;

implementation

uses
  system.generics.defaults, system.sysUtils, system.timeSpan,
  mediaInfoDLL,
  mmpMPVFormatting,
  mmpFileUtils,
  _debugWindow;

{ TMediaInfo }

constructor TMediaInfo.create;
begin
  inherited;
  FMediaStreams := TObjectList<TMediaStream>.create;
  FMediaStreams.ownsObjects := TRUE;
  FMediaChapters := TObjectList<TMediaChapter>.create;
  FMediaChapters.ownsObjects := TRUE;
end;

destructor TMediaInfo.Destroy;
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

function TMediaInfo.getDuration: integer;
begin
  result := FDuration div 1000;
end;

function TMediaInfo.getFileSize: string;
begin
  result := mmpFormatFileSize(mmpFileSize(FURL));
end;

function TMediaInfo.getMetaData(const aMemo: TMemo): boolean;
begin
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

function TMediaInfo.getMediaInfo(const aURL: string = ''): boolean;
var
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

    vBitRate    := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'BitRate/String',         Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Duration/String5',       Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Format',                 Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'ID',                     Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Language/String',        Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Title',                  Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'StreamKind',             Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Width',                  Info_Text, Info_Name) + 'x' + mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Height', Info_Text, Info_Name) + ' ' + mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'FrameRate', Info_Text, Info_Name) + ' fps';

    vBitRate := stringReplace(vBitRate, ' ', ',', []);
    case pos(' (', vDuration) > 1 of TRUE: vDuration := copy(vDuration, 1, pos(' (', vDuration) - 1); end;

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 0));
  end;

  function createAudioStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'BitRate/String',         Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Duration/String5',       Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Format',                 Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'ID',                     Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Language/String',        Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Title',                  Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'StreamKind',             Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'SamplingRate/String',    Info_Text, Info_Name);

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 2));
  end;

  function createTextStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'BitRate/String',          Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Duration/String5',        Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Format',                  Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'ID',                      Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Language/String',         Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Title',                   Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'StreamKind',              Info_Text, Info_Name);
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
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Menu,         0, 'Chapters_Pos_Begin',  Info_Text, Info_Name), chapterBegin) of FALSE: chapterBegin := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Menu,         0, 'Chapters_Pos_End',    Info_Text, Info_Name), chapterEnd)   of FALSE: chapterEnd   := 0; end;

    case chapterEnd > chapterBegin of  TRUE: FChapterCount := chapterEnd - chapterBegin;
                                      FALSE: FChapterCount := 0; end;
    case chapterCount = 0 of TRUE: EXIT; end;

    FMediaChapters.clear;
    for var i := chapterBegin to chapterEnd - 1 do begin
      var vChapter := TMediaChapter.create;
      FMediaChapters.add(vChapter);
      vChapter.chapterTitle   := cleanChapterTitle(string(MediaInfo_GetI(FHandle, Stream_Menu, 0, i, Info_Text))); // title
      vChapter.chapterStartSS := cleanChapterStartSS(MediaInfo_GetI(FHandle, Stream_Menu, 0, i, Info_Name));       // position
    end;

    case FMediaChapters[0].chapterStartSS <> 0 of TRUE: begin
                                                          FMediaChapters.insert(0, TMediaChapter.create);
                                                          inc(FChapterCount); end;end;

    calcChapterEndSS;

    case (FMediaChapters[0].chapterStartSS = 0) and (FMediaChapters[0].chapterEndSS = -1) of TRUE:  begin
                                                                                                      FMediaChapters.delete(0);
                                                                                                      dec(FChapterCount);
                                                                                                    end;end; // delete initial bogus 0:00:00-0:00:00 chapter
  end;

begin
  result := FALSE;

  case loadDLL of FALSE: EXIT; end;

  case aURL <> '' of TRUE: FURL := aURL; end;
  mediaInfo_Open(FHandle, PWideChar(FURL));

  try
    FMediaStreams.clear;

    FOverallFrameRate := mediaInfo_Get(FHandle, Stream_General,  0, 'FrameRate',       Info_Text, Info_Name);
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'OverallBitRate',  Info_Text, Info_Name), FOverallBitRate)    of FALSE: FOverallBitRate   := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'Duration',        Info_Text, Info_Name), FDuration)          of FALSE: FDuration         := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Audio,        0, 'BitRate',         Info_Text, Info_Name), FAudioBitRate)      of FALSE: FAudioBitRate     := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'Width',           Info_Text, Info_Name), FWidth)             of FALSE: FWidth            := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'Height',          Info_Text, Info_Name), FHeight)            of FALSE: FHeight           := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'BitRate',         Info_Text, Info_Name), FVideoBitRate)      of FALSE: FVideoBitRate     := 0; end;

    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Image,        0, 'Width',           Info_Text, Info_Name), FImageWidth)        of FALSE: FImageWidth       := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_Image,        0, 'Height',          Info_Text, Info_Name), FImageHeight)       of FALSE: FImageHeight      := 0; end;

    case (FImageWidth = 0) and (FImageHeight = 0) and (FWidth <> 0) and (FHeight <> 0) of TRUE: begin                            // MediaInfo reports some images as video streams!
                                                                                                  FImageWidth   := FWidth;
                                                                                                  FImageHeight  := FHeight; end;end;

    FStereoMono := mediaInfo_Get(FHandle, Stream_Audio,    0, 'Title',        Info_Text, Info_Name);

    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'GeneralCount',    Info_Text, Info_Name), FGeneralCount)      of FALSE: FGeneralCount     := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'VideoCount',      Info_Text, Info_Name), FVideoCount)        of FALSE: FVideoCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'AudioCount',      Info_Text, Info_Name), FAudioCount)        of FALSE: FAudioCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'TextCount',       Info_Text, Info_Name), FTextCount)         of FALSE: FTextCount        := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'OtherCount',      Info_Text, Info_Name), FOtherCount)        of FALSE: FOtherCount       := 0; end;
    case tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'ImageCount',      Info_Text, Info_Name), FImageCount)        of FALSE: FImageCount       := 0; end;

    FHasCoverArt := mediaInfo_Get(FHandle, Stream_General, 0, 'Cover',        Info_Text, Info_Name);

    for var vStreamIx := 0 to streamCount - 1 do begin
      case mediaInfo_Get(FHandle, Stream_Video, vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createVideoStream(vStreamIx); end;
      case mediaInfo_Get(FHandle, Stream_Audio, vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createAudioStream(vStreamIx); end;
      case mediaInfo_Get(FHandle, Stream_Text,  vStreamIx, 'StreamKind', Info_Text, Info_Name) <> '' of TRUE: createTextStream(vStreamIx); end;
    end;

    createChapters;

  finally
    mediaInfo_close(FHandle);
  end;
end;

function TMediaInfo.loadDLL: boolean;
begin
  result := FALSE;
  case FHandle = 0 of TRUE: begin
                              case mediaInfoDLL_Load('MediaInfo.dll') of FALSE: EXIT; end;
                              mediaInfo_Option(0, 'Internet', 'No');
                              FHandle := MediaInfo_New(); end;end;
  result := FHandle <> 0;
end;

function TMediaInfo.sortStreams: boolean;
begin
  mediaStreams.sort(TComparer<TMediaStream>.construct(
      function (const L, R: TMediaStream): integer
      begin
         case length(L.ID) = 1 of TRUE: L.ID := '0' + L.ID; end;
         case length(R.ID) = 1 of TRUE: R.ID := '0' + R.ID; end;
         result := compareText(L.ID, R.ID)
      end
      ));
end;

end.
