{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit consts;

interface

uses
  winApi.messages;

const
  MENU_ABOUT_ID   = WM_USER + 2001;
  MENU_HELP_ID    = WM_USER + 2002;

  WIN_CLOSEAPP       = WM_USER + 2003;
  WIN_RESIZE         = WM_USER + 2004;
  WIN_POSITION       = WM_USER + 2005;
  WIN_CONTROLS       = WM_USER + 2006;
  WIN_RESTART        = WM_USER + 2007;
  WIN_TAB            = WM_USER + 2008;
  WIN_TABTAB         = WM_USER + 2009;
  WIN_TABALT         = WM_USER + 2010;
  WIN_CAPTION        = WM_USER + 2011;
  WIN_PAUSE_PLAY     = WM_USER + 2012;
  WIN_GREATER        = WM_USER + 2013;
  WIN_AUTOCENTER_OFF = WM_USER + 2014;
  WIN_SYNC_MEDIA     = WM_USER + 2015;

  WM_PROGRESSBAR_CLICK   = WM_USER + 2101;
  WM_TICK                = WM_USER + 2102;
  WM_ADJUST_ASPECT_RATIO = WM_USER + 2103;
  WM_CENTRE_WINDOW       = WM_USER + 2104;
  WM_KEY_UP              = WM_USER + 2105;
  WM_CHECK_SCREEN_LIMITS = WM_USER + 2106;
  WM_SMALLER_WINDOW      = WM_USER + 2107;
  WM_PLAY_CURRENT_ITEM   = WM_USER + 2108;
  WM_SHOW_WINDOW         = WM_USER + 2109;

  POT_PLAYER   = 'C:\Program Files\DAUM\PotPlayer\PotPlayerMini64.exe';
  LOSSLESS_CUT = 'C:\Program Files\LosslessCut-win-x64\LosslessCut.exe';
  SHOTCUT      = 'C:\Program Files\Shotcut\shotcut.exe';

  PB_DEFAULT_COLOR = $202020;
  ST_DEFAULT_COLOR = $707070;

type
  TMediaType = (mtUnk, mtAudio, mtVideo);
  TMediaTypeRec = record
    mimeType: string;
    mediaType: TMediaType;
    typeName: string;
    fileExts: string;
  end;

const
  mediaTypes: array[0..82] of TMediaTypeRec = (

// DVD/Blu-ray audio formats
(mimeType: 'audio/ac3';             mediaType: mtAudio; typeName: 'AC-3 Audio';                 fileExts: '.ac3.a52'),
(mimeType: 'audio/eac3';            mediaType: mtAudio; typeName: 'E-AC-3 audio';               fileExts: '.eac3'),
(mimeType: 'audio/vnd.dolby.mlp';   mediaType: mtAudio; typeName: 'MLP audio';                  fileExts: '.mlp'),
(mimeType: 'audio/vnd.dts' ;        mediaType: mtAudio; typeName: 'DTS audio';                  fileExts: '.dts'),
(mimeType: 'audio/vnd.dts.hd' ;     mediaType: mtAudio; typeName: 'DTS-HD audio';               fileExts: '.dts-hd.dtshd'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'TrueHD audio';               fileExts: '.true-hd.thd.truehd.thd+ac3'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'True audio';                 fileExts: '.tta'),
// Uncompressed formats
(mimeType: '';                      mediaType: mtAudio; typeName: 'PCM audio';                  fileExts: '.pcm'),
(mimeType: 'audio/wav';             mediaType: mtAudio; typeName: 'Wave audio';                 fileExts: '.wav'),
(mimeType: 'audio/aiff';            mediaType: mtAudio; typeName: 'AIFF audio';                 fileExts: '.aiff.aif.aifc'),
(mimeType: 'audio/amr';             mediaType: mtAudio; typeName: 'AMR audio';                  fileExts: '.amr'),
(mimeType: 'audio/amr-wb';          mediaType: mtAudio; typeName: 'AMR-WB audio';               fileExts: '.awb'),
(mimeType: 'audio/basic';           mediaType: mtAudio; typeName: 'AU audio';                   fileExts: '.au.snd'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'Linear PCM audio';           fileExts: '.lpcm'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw YUV video';              fileExts: '.yuv'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'YUV4MPEG2 video';            fileExts: '.y4m'),
// Free lossless formats
(mimeType: 'audio/x-ape';           mediaType: mtAudio; typeName: 'Monkey''s audio';            fileExts: '.ape'),
(mimeType: 'audio/x-wavpack';       mediaType: mtAudio; typeName: 'WavPack audio';              fileExts: '.wv'),
(mimeType: 'audio/x-shorten';       mediaType: mtAudio; typeName: 'Shorten audio';              fileExts: '.shn'),
// MPEG formats
(mimeType: 'video/vnd.dlna.mpeg-tts'; mediaType: mtVideo; typeName: 'MPEG-2 Transport Stream';  fileExts: '.m2ts.m2t.mts.mtv.ts.tsv.tsa.tts.trp'),
(mimeType: 'audio/vnd.dlna.adts';   mediaType: mtAudio; typeName: 'ADTS audio';                 fileExts: '.adts.adt'),
(mimeType: 'audio/mpeg';            mediaType: mtAudio; typeName: 'MPEG audio';                 fileExts: '.mpa.m1a.m2a.mp1.mp2'),
(mimeType: 'audio/mpeg';            mediaType: mtAudio; typeName: 'MP3 audio';                  fileExts: '.mp3'),
(mimeType: 'video/mpeg';            mediaType: mtVideo; typeName: 'MPEG video';                 fileExts: '.mpeg.mpg.mpe.mpeg2.m1v.m2v.mp2v.mpv.mpv2.mod.tod'),
(mimeType: 'video/dvd';             mediaType: mtVideo; typeName: 'video Object';               fileExts: '.vob.vro'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Enhanced VOB';               fileExts: '.evob.evo'),
(mimeType: 'video/mp4';             mediaType: mtVideo; typeName: 'MPEG-4 video';               fileExts: '.mpeg4.m4v.mp4.mp4v.mpg4'),
(mimeType: 'audio/mp4';             mediaType: mtAudio; typeName: 'MPEG-4 audio';               fileExts: '.m4a'),
(mimeType: 'audio/aac';             mediaType: mtAudio; typeName: 'Raw AAC audio';              fileExts: '.aac'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw H.264/AVC video';        fileExts: '.h264.avc.x264.264'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw H.265/HEVC video';       fileExts: '.hevc.h265.x265.265'),
// Xiph formats
(mimeType: 'audio/flac';            mediaType: mtAudio; typeName: 'FLAC audio';                 fileExts: '.flac'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Ogg audio';                  fileExts: '.oga.ogg'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Opus audio';                 fileExts: '.opus'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Speex audio';                fileExts: '.spx'),
(mimeType: 'video/ogg';             mediaType: mtVideo; typeName: 'Ogg video';                  fileExts: '.ogv.ogm'),
(mimeType: 'application/ogg';       mediaType: mtVideo; typeName: 'Ogg video';                  fileExts: '.ogx'),
// Matroska formats
(mimeType: 'video/x-matroska';      mediaType: mtVideo; typeName: 'Matroska video';             fileExts: '.mkv'),
(mimeType: 'video/x-matroska';      mediaType: mtVideo; typeName: 'Matroska 3D video';          fileExts: '.mk3d'),
(mimeType: 'audio/x-matroska';      mediaType: mtAudio; typeName: 'Matroska audio';             fileExts: '.mka'),
(mimeType: 'video/webm';            mediaType: mtVideo; typeName: 'WebM video';                 fileExts: '.webm'),
(mimeType: 'audio/webm';            mediaType: mtAudio; typeName: 'WebM audio';                 fileExts: '.weba'),
// Misc formats
(mimeType: 'video/avi';             mediaType: mtVideo; typeName: 'video Clip';                 fileExts: '.avi.vfw'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'DivX video';                 fileExts: '.divx'),
(mimeType: '';                      mediaType: mtVideo; typeName: '3ivx video';                 fileExts: '.3iv'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'XVID video';                 fileExts: '.xvid'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'NUT video';                  fileExts: '.nut'),
(mimeType: 'video/flc';             mediaType: mtVideo; typeName: 'FLIC video';                 fileExts: '.flic.fli.flc'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Nullsoft Streaming video';   fileExts: '.nsv'),
(mimeType: 'application/gxf';       mediaType: mtVideo; typeName: 'General Exchange Format';    fileExts: '.gxf'),
(mimeType: 'application/mxf';       mediaType: mtVideo; typeName: 'Material Exchange Format';   fileExts: '.mxf'),
// Windows Media formats
(mimeType: 'audio/x-ms-wma';        mediaType: mtAudio; typeName: 'Windows Media audio';        fileExts: '.wma'),
(mimeType: 'video/x-ms-wm';         mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.wm'),
(mimeType: 'video/x-ms-wmv';        mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.wmv'),
(mimeType: 'video/x-ms-asf';        mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.asf'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Microsoft Recorded TV Show'; fileExts: '.dvr-ms.dvr'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Windows Recorded TV Show';   fileExts: '.wtv'),
// DV formats
(mimeType: '';                      mediaType: mtVideo; typeName: 'DV video';                   fileExts: '.dv.hdv'),
// Flash Video formats
(mimeType: 'video/x-flv';           mediaType: mtVideo; typeName: 'Flash video';                fileExts: '.flv'),
(mimeType: 'video/mp4';             mediaType: mtVideo; typeName: 'Flash video';                fileExts: '.f4v'),
(mimeType: 'audio/mp4';             mediaType: mtAudio; typeName: 'Flash audio';                fileExts: '.f4a'),
// QuickTime formats
(mimeType: 'video/quicktime';       mediaType: mtVideo; typeName: 'QuickTime video';            fileExts: '.qt.mov'),
(mimeType: 'video/quicktime';       mediaType: mtVideo; typeName: 'QuickTime HD video';         fileExts: '.hdmov'),
// Real Media formats
(mimeType: 'application/vnd.rn-realmedia';     mediaType: mtVideo; typeName: 'Real Media video'; fileExts: '.rm'),
(mimeType: 'application/vnd.rn-realmedia-vbr'; mediaType: mtVideo; typeName: 'Real Media video'; fileExts: '.rmvb'),
(mimeType: 'audio/vnd.rn-realaudio'; mediaType: mtAudio; typeName: 'Real Media audio';          fileExts: '.ra.ram'),
// 3GPP formats
(mimeType: 'audio/3gpp';            mediaType: mtAudio; typeName: '3GPP audio';                 fileExts: '.3ga'),
(mimeType: 'audio/3gpp2';           mediaType: mtAudio; typeName: '3GPP audio';                 fileExts: '.3ga2'),
(mimeType: 'video/3gpp';            mediaType: mtVideo; typeName: '3GPP video';                 fileExts: '.3gpp.3gp'),
(mimeType: 'video/3gpp2';           mediaType: mtVideo; typeName: '3GPP video';                 fileExts: '.3gp2.3g2'),
// Video game formats
(mimeType: '';                      mediaType: mtAudio; typeName: 'AY audio';                   fileExts: '.ay'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'GBS audio';                  fileExts: '.gbs'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'GYM audio';                  fileExts: '.gym'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'HES audio';                  fileExts: '.hes'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'KSS audio';                  fileExts: '.kss'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'NSF audio';                  fileExts: '.nsf'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'NSFE audio';                 fileExts: '.nsfe'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'SAP audio';                  fileExts: '.sap'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'SPC audio';                  fileExts: '.spc'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'VGM audio';                  fileExts: '.vgm'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'VGZ audio';                  fileExts: '.vgz'),
// Playlist formats
(mimeType: 'audio/x-mpegurl';       mediaType: mtAudio; typeName: 'M3U Playlist';               fileExts: '.m3u.m3u8'),
(mimeType: 'audio/x-scpls';         mediaType: mtAudio; typeName: 'PLS Playlist';               fileExts: '.pls')
);

implementation

end.
