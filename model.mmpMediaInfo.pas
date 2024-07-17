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
unit model.mmpMediaInfo;

interface

uses
  system.generics.collections,
  vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts,
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

  TMetaData = record
    mdAudioBitRate:       integer;
    mdAudioCount:         integer;
    mdChapterCount:       integer;
    mdDisplayAspectRatio: string;
    mdDuration:           integer;
    mdFrameRate:          string;
    mdGeneralCount:       integer;
    mdHasCoverArt:        string;
    mdHeight:             integer;
    mdImageCount:         integer;
    mdImageHeight:        integer;
    mdImageWidth:         integer;
    mdOtherCount:         integer;
    mdOverallBitRate:     integer;
    mdStereoMono:         string;
    mdTextCount:          integer;
    mdVideoBitRate:       integer;
    mdVideoCount:         integer;
    mdWidth:              integer;
  end;

  IMediaInfo = interface
    ['{993AE566-A6CC-4681-B166-813C8CCD53EE}']
    function getChapterCount:       integer;
    function getImageHeight:        integer;
    function getImageWidth:         integer;
    function getLowestID:           integer;
    function getMediaChapters:      TObjectList<TMediaChapter>;
    function getMediaInfo(const aURL: string; const aMediaType: TMediaType): boolean;
    function getMediaStreams: TObjectList<TMediaStream>;
    function getMetaData(const aMemo: TMemo):       boolean;
    function getSelectedCount:      integer;
    function getStreamCount:        integer;

    function sortStreams: boolean;

    property chapterCount:      integer                     read getChapterCount;
    property imageHeight:       integer                     read getImageHeight;
    property imageWidth:        integer                     read getImageWidth;
    property lowestID:          integer                     read getLowestID;
    property mediaChapters:     TObjectList<TMediaChapter>  read getMediaChapters;
    property mediaStreams:      TObjectList<TMediaStream>   read getMediaStreams;
    property selectedCount:     integer                     read getSelectedCount;
    property streamCount:       integer                     read getStreamCount;
  end;

  TMediaInfo = class(TInterfacedObject, IMediaInfo)
  private
    FMD:                TMetaData;
    FHandle:            THandle;

    FLowestID:          integer;
    FMediaChapters:     TObjectList<TMediaChapter>;
    FMediaStreams:      TObjectList<TMediaStream>;
    FURL:               string;

    function getAudioBitRate:       string;
    function getDuration:           integer;
    function getDisplayAspectRatio: string;
    function getFileSize:           string;
    function getHasCoverArt:        boolean;
    function getStereoMono:         string;
    function getOverallFrameRate:   string;
    function getOverallBitRate:     string;
    function getVideoBitRate:       string;
    function getXY:                 string;
    function loadDLL:               boolean;
    function onNotify(const aNotice: INotice): INotice;
  public
    constructor create;
    destructor Destroy; override;

    function getChapterCount:       integer;
    function getImageHeight:        integer;
    function getImageWidth:         integer;
    function getLowestID:           integer;
    function getMediaInfo(const aURL: string; const aMediaType: TMediaType): boolean;
    function getMediaChapters:      TObjectList<TMediaChapter>;
    function getMediaStreams:       TObjectList<TMediaStream>;
    function getMetaData(const aMemo: TMemo):       boolean;
    function getSelectedCount:      integer;
    function getStreamCount:        integer;

    function sortStreams: boolean;
    function notify(const aNotice: INotice): INotice;

    procedure setLowestID(const aValue: integer);

    property audioBitRate:        string  read getAudioBitRate;
    property audioCount:          integer read FMD.mdAudioCount;
    property chapterCount:        integer read getChapterCount;
    property displayAspectRatio:  string  read getDisplayAspectRatio;
    property duration:            integer read getDuration;
    property fileSize:            string  read getFileSize;
    property generalCount:        integer read FMD.mdGeneralCount;
    property hasCoverArt:         boolean read getHasCoverArt;
    property imageCount:          integer read FMD.mdImageCount;
    property imageHeight:         integer read getImageHeight;
    property imageWidth:          integer read getImageWidth;
    property mediaChapters:       TObjectList<TMediaChapter> read FMediaChapters;
    property mediaStreams:        TObjectList<TMediaStream>  read FMediaStreams;
    property otherCount:          integer read FMD.mdOtherCount;
    property overallBitRate:      string  read getOverallBitRate;
    property overallFrameRate:    string  read getOverallFrameRate;
    property stereoMono:          string  read getStereoMono;
    property streamCount:         integer read getStreamCount;
    property textCount:           integer read FMD.mdTextCount;
    property videoBitRate:        string  read getVideoBitRate;
    property videoCount:          integer read FMD.mdVideoCount;
    property X:                   integer read FMD.mdWidth;
    property Y:                   integer read FMD.mdHeight;
    property XY:                  string  read getXY;

    property lowestID:            integer read getLowestID write FLowestID;
    property selectedCount:       integer read getSelectedCount;
  end;

function MI: IMediaInfo;

implementation

uses
  system.generics.defaults, system.sysUtils, system.timeSpan,
  mediaInfoDLL,
  mmpFileUtils, mmpUtils,
  _debugWindow;

var gMI: IMediaInfo;
function MI: IMediaInfo;
begin
  case gMI = NIL of TRUE: gMI := TMediaInfo.create; end;
  result := gMI;
end;

{ TMediaInfo }

constructor TMediaInfo.create;
begin
  inherited;
  FMediaStreams               := TObjectList<TMediaStream>.create;
  FMediaStreams.ownsObjects   := TRUE;
  FMediaChapters              := TObjectList<TMediaChapter>.create;
  FMediaChapters.ownsObjects  := TRUE;
  appNotifier.subscribe(newSubscriber(onNotify));
end;

destructor TMediaInfo.Destroy;
begin
  FMediaStreams.free;
  FMediaChapters.free;
  inherited;
end;

function TMediaInfo.getAudioBitRate: string;
begin
  result := format('AR:  %d Kb/s', [round(FMD.mdAudioBitRate / 1000)]);
end;

function TMediaInfo.getChapterCount: integer;
begin
  result := FMD.mdChapterCount;
end;

function TMediaInfo.getHasCoverArt: boolean;
begin
  result := FMD.mdHasCoverArt = 'Yes';
end;

function TMediaInfo.getImageHeight: integer;
begin
  result := FMD.mdImageHeight;
end;

function TMediaInfo.getImageWidth: integer;
begin
  result := FMD.mdImageWidth;
end;

function TMediaInfo.getLowestID: integer;
begin
  result := FLowestID;
end;

function TMediaInfo.getDisplayAspectRatio: string;
begin
  result := format('AR:  %s', [FMD.mdDisplayAspectRatio]);
end;

function TMediaInfo.getDuration: integer;
begin
  result := FMD.mdDuration div 1000;
end;


function mmpFormatFileSize(const aSize: int64): string;
begin
 case aSize >= 1052266987 of  TRUE: try result := format('FS:   %.3f GB', [aSize / 1024 / 1024 / 1024]); except end;  // >= 0.98 of 1GB
                             FALSE: case aSize < 1024 * 1024 of  TRUE: try result := format('FS:   %d KB', [trunc(aSize / 1024)]); except end;
                                                                FALSE: try result := format('FS:   %.2f MB', [aSize / 1024 / 1024]); except end;end;end;
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
  aMemo.lines.add(displayAspectRatio);
  aMemo.lines.add(overallFrameRate);
  aMemo.lines.add(overallBitRate);
  aMemo.lines.add(audioBitRate);
  aMemo.lines.add(videoBitRate);
  aMemo.lines.add(stereoMono);
  aMemo.lines.add(fileSize);
end;

function TMediaInfo.getOverallBitRate: string;
begin
  result := format('BR:   %d Kb/s', [round(FMD.mdOverallBitRate / 1000)]);
end;

function TMediaInfo.getOverallFrameRate: string;
begin
  case FMD.mdFrameRate = '' of   TRUE: result := 'FR: ';
                                FALSE: result := format('FR:   %s fps', [FMD.mdFrameRate]); end;
end;

function TMediaInfo.getSelectedCount: integer;
begin
  result := 0;
  for var vMediaStream in FMediaStreams do case vMediaStream.selected of TRUE: inc(result); end;
end;

function TMediaInfo.getStereoMono: string;
begin
  result := 'SM:  ' + FMD.mdStereoMono;
end;

function TMediaInfo.getStreamCount: integer;
begin
  result := FMD.mdGeneralCount + FMD.mdVideoCount + FMD.mdAudioCount + FMD.mdTextCount + FMD.mdOtherCount + FMD.mdImageCount;
end;

function TMediaInfo.getVideoBitRate: string;
begin
  result := format('VR:  %d Kb/s', [round(FMD.mdVideoBitRate / 1000)]);
end;

function TMediaInfo.getXY: string;
begin
  case (X <> 0) and (Y <> 0) of  TRUE: result := format('XY:  %d x %d', [X, Y]);
                                FALSE: result := format('XY:  %d x %d', [FMD.mdImageWidth, FMD.mdImageHeight]); end;
end;

function TMediaInfo.getMediaChapters: TObjectList<TMediaChapter>;
begin
  result := FMediaChapters;
end;

function TMediaInfo.getMediaInfo(const aURL: string; const aMediaType: TMediaType): boolean;
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

    vBitRate    := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'BitRate/String',          Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Duration/String5',        Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Format',                  Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'ID',                      Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Language/String',         Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Title',                   Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'StreamKind',              Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Width',                   Info_Text, Info_Name) + 'x'
                 + mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'Height',                  Info_Text, Info_Name) + ' '
                 + mediaInfo_Get(FHandle, Stream_Video, aStreamIx, 'FrameRate',               Info_Text, Info_Name) + ' fps';

    vBitRate := stringReplace(vBitRate, ' ', ',', []);
    case pos(' (', vDuration) > 1 of TRUE: vDuration := copy(vDuration, 1, pos(' (', vDuration) - 1); end;

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 0));
  end;

  function createAudioStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'BitRate/String',          Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Duration/String5',        Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Format',                  Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'ID',                      Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Language/String',         Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'Title',                   Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'StreamKind',              Info_Text, Info_Name);
    vInfo       := mediaInfo_Get(FHandle, Stream_Audio, aStreamIx, 'SamplingRate/String',     Info_Text, Info_Name);

    FMediaStreams.add(TMediaStream.create(vID, vStreamType, vDuration, vFormat, vBitRate, vTitle, vLanguage, vInfo, 2));
  end;

  function createTextStream(aStreamIx: integer): boolean;
  begin
    vBitRate    := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'BitRate/String',           Info_Text, Info_Name);
    vDuration   := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Duration/String5',         Info_Text, Info_Name);
    vFormat     := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Format',                   Info_Text, Info_Name);
    vID         := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'ID',                       Info_Text, Info_Name);
    vLanguage   := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Language/String',          Info_Text, Info_Name);
    vTitle      := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'Title',                    Info_Text, Info_Name);

    vStreamType := mediaInfo_Get(FHandle, Stream_Text, aStreamIx, 'StreamKind',               Info_Text, Info_Name);
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

    case chapterEnd > chapterBegin of  TRUE: FMD.mdChapterCount := chapterEnd - chapterBegin;
                                      FALSE: FMD.mdChapterCount := 0; end;
    case chapterCount = 0 of TRUE: EXIT; end;

    for var i := chapterBegin to chapterEnd - 1 do begin
      var vChapter := TMediaChapter.create;
      FMediaChapters.add(vChapter);
      vChapter.chapterTitle   := cleanChapterTitle(string(MediaInfo_GetI(FHandle, Stream_Menu, 0, i, Info_Text))); // title
      vChapter.chapterStartSS := cleanChapterStartSS(MediaInfo_GetI(FHandle, Stream_Menu, 0, i, Info_Name));       // position
    end;

    case FMediaChapters[0].chapterStartSS <> 0 of TRUE: begin
                                                          FMediaChapters.insert(0, TMediaChapter.create);
                                                          inc(FMD.mdChapterCount); end;end;

    calcChapterEndSS;

    case (FMediaChapters[0].chapterStartSS = 0) and (FMediaChapters[0].chapterEndSS = -1) of TRUE:  begin
                                                                                                      FMediaChapters.delete(0);
                                                                                                      dec(FMD.mdChapterCount);
                                                                                                    end;end; // delete initial bogus 0:00:00-0:00:00 chapter
  end;

begin
  result := FALSE;

  case loadDLL of FALSE: EXIT; end;

  FMD := default(TMetaData);
  FMediaStreams.clear;
  FMediaChapters.clear;

  FURL := aURL;

  try
    mediaInfo_Open(FHandle, PWideChar(FURL));
  except EXIT; end;

  try try

    FMD.mdFrameRate :=  mediaInfo_Get(FHandle, Stream_General,      0, 'FrameRate',       Info_Text, Info_Name);
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'OverallBitRate',  Info_Text, Info_Name), FMD.mdOverallBitRate)    of FALSE: FMD.mdOverallBitRate   := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'Duration',        Info_Text, Info_Name), FMD.mdDuration)          of FALSE: FMD.mdDuration         := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Audio,        0, 'BitRate',         Info_Text, Info_Name), FMD.mdAudioBitRate)      of FALSE: FMD.mdAudioBitRate     := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'Width',           Info_Text, Info_Name), FMD.mdWidth)             of FALSE: FMD.mdWidth            := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'Height',          Info_Text, Info_Name), FMD.mdHeight)            of FALSE: FMD.mdHeight           := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'BitRate',         Info_Text, Info_Name), FMD.mdVideoBitRate)      of FALSE: FMD.mdVideoBitRate     := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Video,        0, 'BitRate',         Info_Text, Info_Name), FMD.mdVideoBitRate)      of FALSE: FMD.mdVideoBitRate     := 0; end;

    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Image,        0, 'Width',           Info_Text, Info_Name), FMD.mdImageWidth)        of FALSE: FMD.mdImageWidth       := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_Image,        0, 'Height',          Info_Text, Info_Name), FMD.mdImageHeight)       of FALSE: FMD.mdImageHeight      := 0; end;

    FMD.mdDisplayAspectRatio := mediaInfo_Get(FHandle, Stream_Video, 0, 'DisplayAspectRatio',  Info_Text, Info_Name);

    case (FMD.mdImageWidth = 0) and (FMD.mdImageHeight = 0) and (FMD.mdWidth <> 0) and (FMD.mdHeight <> 0) of TRUE: begin                 // MediaInfo reports some images as video streams!
                                                                                                  FMD.mdImageWidth   := FMD.mdWidth;
                                                                                                  FMD.mdImageHeight  := FMD.mdHeight; end;end;

    FMD.mdStereoMono :=    mediaInfo_Get(FHandle, Stream_Audio,     0, 'Title',           Info_Text, Info_Name);

    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'GeneralCount',    Info_Text, Info_Name), FMD.mdGeneralCount)      of FALSE: FMD.mdGeneralCount     := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'VideoCount',      Info_Text, Info_Name), FMD.mdVideoCount)        of FALSE: FMD.mdVideoCount       := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'AudioCount',      Info_Text, Info_Name), FMD.mdAudioCount)        of FALSE: FMD.mdAudioCount       := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'TextCount',       Info_Text, Info_Name), FMD.mdTextCount)         of FALSE: FMD.mdTextCount        := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'OtherCount',      Info_Text, Info_Name), FMD.mdOtherCount)        of FALSE: FMD.mdOtherCount       := 0; end;
    case    tryStrToInt(mediaInfo_Get(FHandle, Stream_General,      0, 'ImageCount',      Info_Text, Info_Name), FMD.mdImageCount)        of FALSE: FMD.mdImageCount       := 0; end;

    FMD.mdHasCoverArt := mediaInfo_Get(FHandle, Stream_General,     0, 'Cover',           Info_Text, Info_Name);

    for var vStreamIx := 0 to streamCount - 1 do begin
      case              mediaInfo_Get(FHandle, Stream_Video, vStreamIx, 'StreamKind',     Info_Text, Info_Name) <> '' of TRUE: createVideoStream(vStreamIx); end;
      case              mediaInfo_Get(FHandle, Stream_Audio, vStreamIx, 'StreamKind',     Info_Text, Info_Name) <> '' of TRUE: createAudioStream(vStreamIx); end;
      case              mediaInfo_Get(FHandle, Stream_Text,  vStreamIx, 'StreamKind',     Info_Text, Info_Name) <> '' of TRUE: createTextStream (vStreamIx); end;
    end;

    case aMediaType of mtAudio, mtVideo: createChapters; end;

    sortStreams;

    setLowestID(mmpIfThenElse(MI.mediaStreams.count > 0, strToIntDef(MI.mediaStreams[0].ID, 0), 0));

  finally mediaInfo_close(FHandle); end;
    result := TRUE;
  except end;

  notifyApp(newNotice(evGSHasCoverArt, getHasCoverArt));
end;

function TMediaInfo.getMediaStreams: TObjectList<TMediaStream>;
begin
  result := FMediaStreams;
end;

function TMediaInfo.loadDLL: boolean;
begin
  result := FALSE;
  case FHandle = 0 of TRUE: begin
                              try
                                case mediaInfoDLL_Load('MediaInfo.dll') of FALSE: EXIT; end;
                                mediaInfo_Option(0, 'Internet', 'No');
                                FHandle := MediaInfo_New();
                              except end;end;end;
  result := FHandle <> 0;
end;

function TMediaInfo.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TMediaInfo.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evMIGetMediaInfo:   getMediaInfo(aNotice.text, aNotice.mediaType);
    evMIFillMetaData:   getMetaData(aNotice.component as TMemo);
    evMIReqHasCoverArt: aNotice.tf := getHasCoverArt;
  end;
end;

procedure TMediaInfo.setLowestID(const aValue: integer);
begin
  FLowestID := aValue;
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

initialization
  gMI := NIL;
  MI;

end.
