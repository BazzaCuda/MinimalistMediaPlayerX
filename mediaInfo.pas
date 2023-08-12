{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit mediaInfo;

interface

uses
  vcl.stdCtrls;

type
  TMediaInfo = class(TObject)
  private
    FAudioBitRate: integer;
    FFileSize: int64;
    FHeight: integer;
    FStereoMono: string;
    FWidth: integer;
    FNeedInfo: boolean;
    FOverallFrameRate: string;
    FOverallBitRate: integer;
    FURL: string;
    FVideoBitRate: integer;
    function getAudioBitRate: string;
    function getFileSize: string;
    function getOverallFrameRate: string;
    function getOverallBitRate: string;
    function getVideoBitRate: string;
    function getXY: string;
    function getStereoMono: string;
    procedure setURL(const Value: string);
  public
    function getData(const aMemo: TMemo): boolean;
    function initMediaInfo(const aURL: string): boolean;
    property audioBitRate:      string  read getAudioBitRate;
    property fileSize:          string  read getFileSize;
    property overallBitRate:    string  read getOverallBitRate;
    property overallFrameRate:  string  read getOverallFrameRate;
    property stereoMono:        string  read getStereoMono;
    property URL:               string  read FURL write setURL;
    property videoBitRate:      string  read getVideoBitRate;
    property X:                 integer read FWidth;
    property Y:                 integer read FHeight;
    property XY:                string  read getXY;
  end;

function MI: TMediaInfo;

implementation

uses
  mediaInfoDLL, system.sysUtils, commonUtils, _debugWindow;

var
  gMI: TMediaInfo;

function MI: TMediaInfo;
begin
  case gMI = NIL of TRUE: gMI := TMediaInfo.create; end;
  result := gMI;
end;

{ TMediaInfo }

function TMediaInfo.getAudioBitRate: string;
begin
  result := format('AR:  %d Kb/s', [round(FAudioBitRate / 1000)]);
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

function TMediaInfo.getStereoMono: string;
begin
  result := 'SM:  ' + copy(FStereoMono, 1, pos(' ', FStereoMono) - 1); // "Stereo / Stereo" -> "Stereo"
  result := 'SM:  ' + FStereoMono;
end;

function TMediaInfo.getVideoBitRate: string;
begin
  result := format('VR:  %d Kb/s', [round(FVideoBitRate / 1000)]);
end;

function TMediaInfo.getXY: string;
begin
  result := format('XY:  %d x %d', [X, Y]);
end;

function TMediaInfo.initMediaInfo(const aURL: string): boolean;
var
  handle: THandle;
begin
  result := FALSE;
  case mediaInfoDLL_Load('MediaInfo.dll') of FALSE: EXIT; end;
  mediaInfo_Option(0, 'Internet', 'No');
  handle := MediaInfo_New();
  case handle = 0 of TRUE: EXIT; end;
  try
    mediaInfo_Open(handle, PWideChar(aURL));
    FURL := aURL;
    FOverallFrameRate := mediaInfo_Get(handle, Stream_General,  0, 'FrameRate',       Info_Text, Info_Name);
    case tryStrToInt(mediaInfo_Get(handle, Stream_General,      0, 'OverallBitRate',  Info_Text, Info_Name), FOverallBitRate)    of FALSE: FOverallBitRate   := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Audio,        0, 'BitRate',         Info_Text, Info_Name), FAudioBitRate)      of FALSE: FAudioBitRate     := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'Width',           Info_Text, Info_Name), FWidth)             of FALSE: FWidth            := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'Height',          Info_Text, Info_Name), FHeight)            of FALSE: FHeight           := 0; end;
    case tryStrToInt(mediaInfo_Get(handle, Stream_Video,        0, 'BitRate',         Info_Text, Info_Name), FVideoBitRate)      of FALSE: FVideoBitRate     := 0; end;

    FStereoMono := mediaInfo_Get(handle, Stream_Audio,  0, 'Title',         Info_Text, Info_Name);

//    FNeedInfo := FALSE;         // get info each time the user asks for it
  finally
    mediaInfo_close(handle);
  end;
end;

procedure TMediaInfo.setURL(const Value: string);
begin
  FNeedInfo := value <> FURL;
  FURL      := value;
end;

initialization
  gMI := NIL;

finalization
  case gMI <> NIL of TRUE: gMI.free; end;

end.
