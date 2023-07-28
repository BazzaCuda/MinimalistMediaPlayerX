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
  WIN_CLOSEAPP    = WM_USER + 2003;
  WIN_RESIZE      = WM_USER + 2004;
  WIN_POSITION    = WM_USER + 2005;
  WIN_CONTROLS    = WM_USER + 2006;
  WIN_RESTART     = WM_USER + 2007;
  WIN_TAB         = WM_USER + 2008;
  WIN_CAPTION     = WM_USER + 2009;
  WIN_PAUSE_PLAY  = WM_USER + 2010;

  WM_PROGRESSBAR_CLICK   = WM_USER + 2011;
  WM_TICK                = WM_USER + 2012;
  WM_ADJUST_ASPECT_RATIO = WM_USER + 2013;
  WM_CENTRE_WINDOW       = WM_USER + 2014;
  WM_KEY_UP              = WM_USER + 2015;
  WM_CHECK_SCREEN_LIMITS = WM_USER + 2016;
  WM_SMALLER_WINDOW      = WM_USER + 2017;

  POT_PLAYER   = 'C:\Program Files\DAUM\PotPlayer\PotPlayerMini64.exe';
  LOSSLESS_CUT = 'B:\Tools\LosslessCut-win-x64\LosslessCut.exe';
  SHOTCUT      = 'C:\Program Files\Shotcut\shotcut.exe';

  EXTS_1 = '.txt.ini.jpg.jpeg.bmp.ico.png.url.srt.sub.sami.ssa.xml.exe.html.mhtml.htm.snz.jfif.gif.dll.pdf.doc.docx.xls.xlsx.jp2.ods.djvu.epub.zip.rar.7z.dic';
  EXTS_2 = '.rtf.md.bz2.arj.cab.gz.iso.jar.lz.lzh.tar.uue.xz.z.zipx.zst.cbr.cbz.pl.py.pas.dfm.scpt.dmg.plist.css.body.offsets.index.data.css.dist.bom.slip.msg.bat.go';
  EXTS_3 = '.conf.sh.dpk.res.dfm.identcache.local.dcu.yml.csv.sub.queued.par.par2.nfo.bup.ifo.download.crdownload.vhdx.avhdx.luac.msi';
  EXTS_FILTER = EXTS_1 + EXTS_2 + EXTS_3;


type
  TMediaTypeRec = record
    mimeType: string;
    mediaType: string;
    typeName: string;
    fileExts: string;
  end;


implementation
  function mediaTypeRec(A, B, C, D: string): TMediaTypeRec;
  begin
    result.mimeType := A;
    result.mediaType := B;
    result.typeName := C;
    result.fileExts := D;
  end;

const
  mpvMediaTypes: TArray<TMediaTypeRec> = [


// DVD/Blu-ray audio formats
mediaTypeRec('audio/ac3',                        'audio', 'AC-3 Audio',                 '.ac3.a52'),
mediaTypeRec('audio/eac3',                       'audio', 'E-AC-3 audio',               '.eac3'),
mediaTypeRec('audio/vnd.dolby.mlp',              'audio', 'MLP audio',                  '.mlp'),
mediaTypeRec('audio/vnd.dts' ,                   'audio', 'DTS audio',                  '.dts'),
mediaTypeRec('audio/vnd.dts.hd' ,                'audio', 'DTS-HD audio',               '.dts-hd.dtshd'),
mediaTypeRec('',                                 'audio', 'TrueHD audio',               '.true-hd.thd.truehd.thd+ac3'),
mediaTypeRec('',                                 'audio', 'True audio',                 '.tta'),
// Uncompressed formats
mediaTypeRec('',                                 'audio', 'PCM audio',                  '.pcm'),
mediaTypeRec('audio/wav',                        'audio', 'Wave audio',                 '.wav'),
mediaTypeRec('audio/aiff',                       'audio', 'AIFF audio',                 '.aiff.aif.aifc'),
mediaTypeRec('audio/amr',                        'audio', 'AMR audio',                  '.amr'),
mediaTypeRec('audio/amr-wb',                     'audio', 'AMR-WB audio',               '.awb'),
mediaTypeRec('audio/basic',                      'audio', 'AU audio',                   '.au.snd'),
mediaTypeRec('',                                 'audio', 'Linear PCM audio',           '.lpcm'),
mediaTypeRec('',                                 'video', 'Raw YUV video',              '.yuv'),
mediaTypeRec('',                                 'video', 'YUV4MPEG2 video',            '.y4m'),
// Free lossless formats
mediaTypeRec('audio/x-ape',                      'audio', 'Monkey''s audio',             '.ape'),
mediaTypeRec('audio/x-wavpack',                  'audio', 'WavPack audio',              '.wv'),
mediaTypeRec('audio/x-shorten',                  'audio', 'Shorten audio',              '.shn'),
// MPEG formats
mediaTypeRec('video/vnd.dlna.mpeg-tts',          'video', 'MPEG-2 Transport Stream',    '.m2ts.m2t.mts.mtv.ts.tsv.tsa.tts.trp'),
mediaTypeRec('audio/vnd.dlna.adts',              'audio', 'ADTS audio',                 '.adts.adt'),
mediaTypeRec('audio/mpeg',                       'audio', 'MPEG audio',                 '.mpa.m1a.m2a.mp1.mp2'),
mediaTypeRec('audio/mpeg',                       'audio', 'MP3 audio',                  '.mp3'),
mediaTypeRec('video/mpeg',                       'video', 'MPEG video',                 '.mpeg.mpg.mpe.mpeg2.m1v.m2v.mp2v.mpv.mpv2.mod.tod'),
mediaTypeRec('video/dvd',                        'video', 'Video Object',               '.vob.vro'),
mediaTypeRec('',                                 'video', 'Enhanced VOB',               '.evob.evo'),
mediaTypeRec('video/mp4',                        'video', 'MPEG-4 video',               '.mpeg4.m4v.mp4.mp4v.mpg4'),
mediaTypeRec('audio/mp4',                        'audio', 'MPEG-4 audio',               '.m4a'),
mediaTypeRec('audio/aac',                        'audio', 'Raw AAC audio',              '.aac'),
mediaTypeRec('',                                 'video', 'Raw H.264/AVC video',        '.h264.avc.x264.264'),
mediaTypeRec('',                                 'video', 'Raw H.265/HEVC video',       '.hevc.h265.x265.265'),
// Xiph formats
mediaTypeRec('audio/flac',                       'audio', 'FLAC audio',                 '.flac'),
mediaTypeRec('audio/ogg',                        'audio', 'Ogg audio',                  '.oga.ogg'),
mediaTypeRec('audio/ogg',                        'audio', 'Opus audio',                 '.opus'),
mediaTypeRec('audio/ogg',                        'audio', 'Speex audio',                '.spx'),
mediaTypeRec('video/ogg',                        'video', 'Ogg video',                  '.ogv.ogm'),
mediaTypeRec('application/ogg',                  'video', 'Ogg video',                  '.ogx'),
// Matroska formats
mediaTypeRec('video/x-matroska',                 'video', 'Matroska video',             '.mkv'),
mediaTypeRec('video/x-matroska',                 'video', 'Matroska 3D video',          '.mk3d'),
mediaTypeRec('audio/x-matroska',                 'audio', 'Matroska audio',             '.mka'),
mediaTypeRec('video/webm',                       'video', 'WebM video',                 '.webm'),
mediaTypeRec('audio/webm',                       'audio', 'WebM audio',                 '.weba'),
// Misc formats
mediaTypeRec('video/avi',                        'video', 'Video Clip',                 '.avi.vfw'),
mediaTypeRec('',                                 'video', 'DivX video',                 '.divx'),
mediaTypeRec('',                                 'video', '3ivx video',                 '.3iv'),
mediaTypeRec('',                                 'video', 'XVID video',                 '.xvid'),
mediaTypeRec('',                                 'video', 'NUT video',                  '.nut'),
mediaTypeRec('video/flc',                        'video', 'FLIC video',                 '.flic.fli.flc'),
mediaTypeRec('',                                 'video', 'Nullsoft Streaming video',   '.nsv'),
mediaTypeRec('application/gxf',                  'video', 'General Exchange Format',    '.gxf'),
mediaTypeRec('application/mxf',                  'video', 'Material Exchange Format',   '.mxf'),
// Windows Media formats
mediaTypeRec('audio/x-ms-wma',                   'audio', 'Windows Media audio',        '.wma'),
mediaTypeRec('video/x-ms-wm',                    'video', 'Windows Media video',        '.wm'),
mediaTypeRec('video/x-ms-wmv',                   'video', 'Windows Media video',        '.wmv'),
mediaTypeRec('video/x-ms-asf',                   'video', 'Windows Media video',        '.asf'),
mediaTypeRec('',                                 'video', 'Microsoft Recorded TV Show', '.dvr-ms.dvr'),
mediaTypeRec('',                                 'video', 'Windows Recorded TV Show',   '.wtv'),
// DV formats
mediaTypeRec('',                                 'video', 'DV video',                   '.dv.hdv'),
// Flash Video formats
mediaTypeRec('video/x-flv',                      'video', 'Flash video',                '.flv'),
mediaTypeRec('video/mp4',                        'video', 'Flash video',                '.f4v'),
mediaTypeRec('audio/mp4',                        'audio', 'Flash audio',                '.f4a'),
// QuickTime formats
mediaTypeRec('video/quicktime',                  'video', 'QuickTime video',            '.qt.mov'),
mediaTypeRec('video/quicktime',                  'video', 'QuickTime HD video',         '.hdmov'),
// Real Media formats
mediaTypeRec('application/vnd.rn-realmedia',     'video', 'Real Media video',           '.rm'),
mediaTypeRec('application/vnd.rn-realmedia-vbr', 'video', 'Real Media video',           '.rmvb'),
mediaTypeRec('audio/vnd.rn-realaudio',           'audio', 'Real Media audio',           '.ra.ram'),
// 3GPP formats
mediaTypeRec('audio/3gpp',                       'audio', '3GPP audio',                 '.3ga'),
mediaTypeRec('audio/3gpp2',                      'audio', '3GPP audio',                 '.3ga2'),
mediaTypeRec('video/3gpp',                       'video', '3GPP video',                 '.3gpp.3gp'),
mediaTypeRec('video/3gpp2',                      'video', '3GPP video',                 '.3gp2.3g2'),
// Video game formats
mediaTypeRec('',                                 'audio', 'AY audio',                   '.ay'),
mediaTypeRec('',                                 'audio', 'GBS audio',                  '.gbs'),
mediaTypeRec('',                                 'audio', 'GYM audio',                  '.gym'),
mediaTypeRec('',                                 'audio', 'HES audio',                  '.hes'),
mediaTypeRec('',                                 'audio', 'KSS audio',                  '.kss'),
mediaTypeRec('',                                 'audio', 'NSF audio',                  '.nsf'),
mediaTypeRec('',                                 'audio', 'NSFE audio',                 '.nsfe'),
mediaTypeRec('',                                 'audio', 'SAP audio',                  '.sap'),
mediaTypeRec('',                                 'audio', 'SPC audio',                  '.spc'),
mediaTypeRec('',                                 'audio', 'VGM audio',                  '.vgm'),
mediaTypeRec('',                                 'audio', 'VGZ audio',                  '.vgz'),
// Playlist formats
mediaTypeRec('audio/x-mpegurl',                  'audio', 'M3U Playlist',               '.m3u.m3u8'),
mediaTypeRec('audio/x-scpls',                    'audio', 'PLS Playlist',               '.pls')];

end.
