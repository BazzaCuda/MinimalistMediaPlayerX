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
unit model.mmpPreviewSheet;

interface

uses
  bazFuncDefs;

function mmpCreatePreviewSheet(const aMediaFilePath: string): TVoid;

implementation

uses
  system.math, system.sysUtils,
  bazCmd,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpExportExec, mmpFileUtils, mmpFormatting, mmpGlobalState, mmpShellUtils, mmpStringUtils, mmpUtils,
  view.mmpFormProgress,
  model.mmpConfigFile, model.mmpMediaInfo,
  _debugWindow;

function mmpFirstStreamType(const aStreamType: TStreamType): integer;
begin
  result := -1;
  for var i := 0 to MI.mediaStreams.count - 1 do
    case MI.mediaStreams[i].streamType = aStreamType of TRUE: begin
                                                                result := i;
                                                                BREAK; end;end;
end;

function mmpFirstAudio: string;
begin
  result := EMPTY;

  var vIx := mmpFirstStreamType(TStreamType.stAudio);

  case vIx <> -1 of TRUE: begin
                            result := format('%s %s %s %s', [MI.mediaStreams[vIx].format, MI.mediaStreams[vIx].language, MI.mediaStreams[vIx].bitRate, MI.mediaStreams[vIx].info]);
                            result := mmpRemoveSingleQuotes(result);end;end;
//  debugString('mmpFirstAudio', result);
end;

function mmpFirstVideo: string;
begin
  result := EMPTY;

  var vIx := mmpFirstStreamType(TStreamType.stVideo);

  case vIx <> -1 of TRUE: begin
                            result := format('%s %s %s %s', [MI.mediaStreams[vIx].format, MI.mediaStreams[vIx].duration, MI.mediaStreams[vIx].info, MI.mediaStreams[vIx].bitRate]); // info: width x height fps
                            result := mmpRemoveSingleQuotes(result);
                            end;end;
//  debugString('mmpFirstVideo', result);
end;

function checkInterval(var aSkipIntro: integer; var aSkipOutro: integer; aTotalFrames: integer; aDuration: integer): double;
begin
  result := (GS.duration - aSkipIntro - aSkipOutro) / (aTotalFrames - 1);
  case result * (aTotalFrames - 1) <= aDuration of TRUE: EXIT; end;

  aSkipIntro := 0;
  result := (aDuration - aSkipOutro) / (aTotalFrames - 1);
  case result * (aTotalFrames - 1) <= aDuration of TRUE: EXIT; end;

  aSkipOutro := 0;
  result := aDuration / (aTotalFrames - 1);
  case (result < 1) of TRUE: result := 1; end;
  case result * (aTotalFrames - 1) <= aDuration of TRUE: EXIT; end;

  result := aDuration / (aTotalFrames - 1);
end;

function mmpCreatePreviewSheet(const aMediaFilePath: string): TVoid;
var
  vProcessHandle: THANDLE;
  vCancelled:     boolean;
  i:              integer;
begin
  var vSkipIntroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_INTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_INTRO], 5);
  var vSkipOutroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_OUTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_OUTRO], 5);
  var vSkipIntro:           integer := round(GS.duration * vSkipIntroPercent / 100);
  var vSkipOutro:           integer := round(GS.duration * vSkipOutroPercent / 100);
  var vMediaFile:           string  := mmp.cmd(evPLReqCurrentItem).text;
  var vMediaDir:            string  := extractFilePath(vMediaFile);
  var vDSQMediaFile:        string  := mmpDoubleSingleQuotes(extractFileName(vMediaFile));

  var vCols:                integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_COLUMNS] <> 0, CF.asInteger[CONF_PREVIEW_COLUMNS], 4);
  var vRows:                integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_ROWS] <> 0, CF.asInteger[CONF_PREVIEW_ROWS], 5);
  var vTotalFrames:         integer := vCols * vRows;
  var vIntervalSS:          double  := checkInterval(vSkipIntro, vSkipOutro, vTotalFrames, GS.duration - 1);
  var vThumbWidth:          integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_THUMB_WIDTH] <> 0, CF.asInteger[CONF_PREVIEW_THUMB_WIDTH], 300);

  var FProgressForm         := mmpNewProgressForm;
  FProgressForm.buttons     := FALSE;
  FProgressForm.heading     := 'Create Preview Contact Sheet';
  FProgressForm.subHeading  := 'Grabbing frames...';
  FProgressForm.onCancel    := NIL;
  FProgressForm.formShow;

  for i := 0 to vTotalFrames - 1 do
  begin
    var vCurrentSeek: double := vSkipIntro + (i * vIntervalSS);
    case (vCurrentSeek >= GS.duration) of TRUE: vCurrentSeek := GS.duration; end;

    var vTempName:    string  := format('mmpPreview_%3.3d.jpg', [i + 1]);
    var vTempPath:    string  := vMediaDir + vTempName;

    var vExtractVF:   string  := format(' -vf "setpts=PTS+%d/TB,scale=%d:-1:flags=lanczos:out_range=pc,drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%%{pts\:gmtime\:0\:%%H\\\:%%M\\\:%%S}'':fontcolor=0x4B96AF:fontsize=12:x=w-tw-8:y=h-th-8"', [trunc(vCurrentSeek), vThumbWidth]);
    var vExtractCmd:  string  := format(' -an -sn -ss %g -i %s %s -frames:v 1 -q:v 2 -y %s', [vCurrentSeek, mmpQuoted(vMediaFile), vExtractVF, mmpQuoted(vTempPath)]);

    FProgressForm.subHeading  := format('Preview %3.3d of %d', [i + 1, vTotalFrames]);

    mmpExportExecAndWait(vExtractCmd, rtFFmpeg, vProcessHandle, vCancelled, {EMPTY} changeFileExt(vMediaFile, '.log'));
  end;

  FProgressForm.subHeading := 'Nearly done...';

  var vInputs:        string := '';
  var vLabels:        string := '';
  for i := 1 to vTotalFrames do
  begin
    vInputs := vInputs + format(' -i %s', [mmpQuoted(vMediaDir + format('mmpPreview_%3.3d.jpg', [i]))]);
    vLabels := vLabels + format('[%d:v]', [i - 1]);
  end;

  var vTile:          string := format('tile=%dx%d:padding=4:margin=4:color=black,pad=iw:ih+75:0:75:color=black,', [vCols, vRows]);
  var vDrawLine1:     string := format('drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=14:x=10:y=10,', [vDSQMediaFile]);

  var vFileSize:      int64  := mmpFileSize(vMediaFile);
  var vLine2:         string := mmpEscapeColons(format('Size: %s (%.0n bytes)', [mmpFormatFileSize(vFileSize), vFileSize * 1.0]));
  var vDrawLine2:     string := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=28,', [vLine2]);

  var vLine3:         string := mmpEscapeColons('Audio: ' + mmpFirstAudio);
  var vDrawLine3:     string := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=46,', [vLine3]);

  var vLine4:         string := mmpEscapeColons('Video: ' + mmpFirstVideo);
  var vDrawLine4:     string := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=64,', [vLine4]);

  var vDrawLogo1:     string := 'drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''MMP'':fontcolor=0x404040:fontsize=48:x=w-tw-10:y=10,';
  var vDrawLogo2:     string := 'drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''Minimalist Media Player'':fontcolor=0x404040:fontsize=12:x=w-tw-5:y=48"';

  var vImageExt:      string := trim(lowerCase(stringReplace(CF[CONF_PREVIEW_IMAGE_FORMAT], '.', '', [rfReplaceAll])));
  case vImageExt = EMPTY of TRUE: vImageExt := 'jpg'; end;

  var vImageFile:     string := changeFileExt(vMediaFile, '.' + vImageExt);
  var vFilterComplex: string := ' -filter_complex "' + vLabels + format('concat=n=%d:v=1:a=0,', [vTotalFrames]);
  var vEpilogue:      string := ' -frames:v 1 -q:v 2 -y ' + mmpQuoted(vImageFile);

  var vCmdLine:       string := vInputs + vFilterComplex + vTile + vDrawLine1 + vDrawLine2 + vDrawLine3 + vDrawLine4 + vDrawLogo1 + vDrawLogo2 + vEpilogue;

  mmpExportExecAndWait(vCmdLine, rtFFmpeg, vProcessHandle, vCancelled, EMPTY {changeFileExt(vMediaFile, '.log')});

  FProgressForm.subHeading := 'preview sheet created';

  case fileExists(vImageFile) and CF.asBoolean[CONF_PREVIEW_SHOW_PREVIEW] of TRUE: mmpShellExec(GS.mainForm.Handle, paramStr(0), '"' + vImageFile + '" noplaylist'); end;

  for i := 1 to vTotalFrames do begin
    var vFileName := vMediaDir + format('mmpPreview_%3.3d.jpg', [i]);
    case fileExists(vFileName) of TRUE: mmpDeleteThisFile(vMediaDir + format('mmpPreview_%3.3d.jpg', [i]), [], TRUE, TRUE, FALSE); end;end;
end;

function mmpCreatePreviewSheet_v1(const aMediaFilePath: string): TVoid;
var
  vProcessHandle: THANDLE;
  vCancelled:     boolean;
begin
  var vSkipIntroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_INTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_INTRO], 5);
  var vSkipOutroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_OUTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_OUTRO], 5);
  var vSkipIntro:           integer := round(GS.duration * vSkipIntroPercent / 100);
  var vSkipOutro:           integer := round(GS.duration * vSkipOutroPercent / 100);
  var vMediaFile:           string  := mmp.cmd(evPLReqCurrentItem).text;
  var vDSQMediaFile:        string  := mmpDoubleSingleQuotes(extractFileName(vMediaFile));
  var vPrelim:              string  := ' -threads 1 -skip_frame nokey -an -sn';
  var vSeek:                string  := format(' -ss %d -lowres 2', [vSkipIntro]);
  var vInputFile:           string  := format(' -i %s',  [mmpQuoted(vMediaFile)]);
  var vVF:                  string  := format(' -vf "setpts=PTS+%d/TB,', [vSkipIntro + 1]); // Presentation TimeStamp
  var vCols:                integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_COLUMNS] <> 0, CF.asInteger[CONF_PREVIEW_COLUMNS], 4);
  var VRows:                integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_ROWS] <> 0, CF.asInteger[CONF_PREVIEW_ROWS], 5);
  var vIntervalSS:          integer := ceil((GS.duration - vSkipIntro - vSkipOutro) / ((vCols * vRows) - 1));
  var vInterval:            string  := format(' select=''isnan(prev_selected_t)+gte(t-prev_selected_t,%d)'',', [vIntervalSS]);
  var vThumbWidth:          integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_THUMB_WIDTH] <> 0, CF.asInteger[CONF_PREVIEW_THUMB_WIDTH], 300);
  var vScale:               string  := format('scale=%d:-1,', [vThumbWidth]);
  var vDrawTimeStamp:       string  := 'drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%{pts\:gmtime\:0\:%H\\\:%M\\\:%S}'':fontcolor=0x4B96AF:fontsize=12:x=w-tw-8:y=h-th-8,';
  var vTile:                string  := format('tile=%dx%d:padding=4:margin=4:color=black,pad=iw:ih+75:0:75:color=black,', [vCols, vRows]);
  var vDrawLine1:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=14:x=10:y=10,', [vDSQMediaFile]);
  var vFileSize:            int64   := mmpFileSize(vMediaFile);
  var vLine2:               string  := format('Size: %s (%.0n bytes)', [mmpFormatFileSize(vFileSize), vFileSize * 1.0]);
      vLine2                        := mmpEscapeColons(vLine2);
  var vDrawLine2:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=28,', [vLine2]);
  var vLine3:               string  := 'Audio: ' + mmpFirstAudio;
      vLine3                        := mmpEscapeColons(vLine3);
  var vDrawLine3:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=46,', [vLine3]);
  var vLine4:               string  := 'Video: ' + mmpFirstVideo;
      vLine4                        := mmpEscapeColons(vLine4);
  var vDrawLine4:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=64,', [vLine4]);
  var vDrawLogo1:           string  := 'drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''MMP'':fontcolor=0x404040:fontsize=48:x=w-tw-10:y=10,';
  var vDrawLogo2:           string  := 'drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''Minimalist Media Player'':fontcolor=0x404040:fontsize=12:x=w-tw-5:y=48"';
  var vImage:               string  := ' -frames:v 1 -update 1 -q:v 2';
  var vImageFile:           string  := changeFileExt(vMediaFile, '.jpg');
  var vOutputFile:          string  := format(' -y %s', [mmpQuoted(vImageFile)]);

  case pos(lowerCase('vp9'), vLine4) > 0 of TRUE: vPrelim := stringReplace(vPrelim, '-threads 1', '-threads 0', [rfReplaceAll]); end;

  var vCmdLine := vPrelim + vSeek + vInputFile + vVF + vInterval + vScale + vDrawTimeStamp + vTile + vDrawLine1 + vDrawLine2 + vDrawLine3 + vDrawLine4 + vDrawLogo1 + vDrawLogo2 + vImage + vOutputFile;

  mmp.cmd(evSTOpInfo, 'creating preview sheet...');

  mmpExportExecAndWait(vCmdLine, rtFFmpeg, vProcessHandle, vCancelled, {EMPTY} changeFileExt(vMediaFile, '.log'));

  mmp.cmd(evSTOpInfo, 'preview sheet created');

  case fileExists(vImageFile) and CF.asBoolean[CONF_PREVIEW_SHOW_PREVIEW] of TRUE: mmpShellExec(GS.mainForm.Handle, paramStr(0), '"' + vImageFile + '" noplaylist'); end;
end;


end.
