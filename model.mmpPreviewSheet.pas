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
  mmpConsts, mmpExportExec, mmpFileUtils, mmpFormatting, mmpGlobalState, mmpShellUtils, mmpUtils,
  view.mmpFormProgress,
  model.mmpConfigFile;

function mmpCreatePreviewSheet(const aMediaFilePath: string): TVoid;
var
  vProcessHandle: THANDLE;
  vCancelled:     boolean;
begin
  var vSkipIntroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_INTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_INTRO], 5);
  var vSkipOutroPercent:    integer := mmp.use<integer>(CF.asInteger[CONF_PREVIEW_SKIP_OUTRO] <> 0, CF.asInteger[CONF_PREVIEW_SKIP_OUTRO], 5);
  var vSkipIntro:           integer := round(GS.duration * vSkipIntroPercent / 100);
  var vSkipOutro:           integer := round(GS.duration * vSkipOutroPercent / 100);
  var vMediaFile:           string  := mmp.cmd(evPLReqCurrentItem).text;
  var vEscapedMediaFile:    string  := stringReplace(extractFileName(vMediaFile), '''', '''''', [rfReplaceAll]);
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
  var vDrawLine1:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=14:x=10:y=10,', [vEscapedMediaFile]);
  var vFileSize:            int64   := mmpFileSize(vMediaFile);
  var vLine2:               string  := format('Size: %s (%.0n bytes)', [mmpFormatFileSize(vFileSize), vFileSize * 1.0]);
      vLine2                        := stringReplace(vLine2, ':', '\:', [rfReplaceAll]);
  var vDrawLine2:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=28,', [vLine2]);
  var vLine3:               string  := 'Audio: ';
      vLine3                        := stringReplace(vLine3, ':', '\:', [rfReplaceAll]);
  var vDrawLine3:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=46,', [vLine3]);
  var vLine4:               string  := format('Video: %dx%d, Duration: %s', [mmp.cmd(evMPReqVideoWidth).integer, mmp.cmd(evMPReqVideoHeight).integer, mmpFormatTime(GS.duration)]);
      vLine4                        := stringReplace(vLine4, ':', '\:', [rfReplaceAll]);
  var vDrawLine4:           string  := format('drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''%s'':fontcolor=0xAF964B:fontsize=12:x=10:y=64,', [vLine4]);
  var vDrawLogo1:           string  := 'drawtext=fontfile=''C\:/Windows/Fonts/verdanab.ttf'':text=''MMP'':fontcolor=0x404040:fontsize=48:x=w-tw-10:y=10,';
  var vDrawLogo2:           string  := 'drawtext=fontfile=''C\:/Windows/Fonts/verdana.ttf'':text=''Minimalist Media Player'':fontcolor=0x404040:fontsize=12:x=w-tw-5:y=48"';
  var vImage:               string  := ' -frames:v 1 -update 1 -q:v 2';
  var vImageFile:           string  := changeFileExt(vMediaFile, '.jpg');
  var vOutputFile:          string  := format(' -y %s', [mmpQuoted(vImageFile)]);

  var vCmdLine := vPrelim + vSeek + vInputFile + vVF + vInterval + vScale + vDrawTimeStamp + vTile + vDrawLine1 + vDrawLine2 + vDrawLine3 + vDrawLine4 + vDrawLogo1 + vDrawLogo2 + vImage + vOutputFile;

//  var FProgressForm         := mmpNewProgressForm;
//  FProgressForm.heading     := 'Create Preview Contact Sheet';
//  FProgressForm.subHeading  := 'Please wait...';
//  FProgressForm.onCancel    := NIL;
//  FProgressForm.formShow;

  mmp.cmd(evSTOpInfo, 'creating preview sheet...');

  mmpExportExecAndWait(vCmdLine, rtFFmpeg, vProcessHandle, vCancelled, EMPTY {changeFileExt(vMediaFile, '.log')});

  mmp.cmd(evSTOpInfo, 'preview sheet created');

  case fileExists(vImageFile) and CF.asBoolean[CONF_PREVIEW_SHOW_PREVIEW] of TRUE: mmpShellExec(GS.mainForm.Handle, paramStr(0), '"' + vImageFile + '" noplaylist'); end;
end;


end.
