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
unit mmpConsts; // and Types

interface

uses
  winApi.messages, winApi.windows,
  system.classes,
  vcl.extCtrls;

const
  MMP_TITLE           = 'MMP: Minimalist Media Player';
  MMP_CHECK           = 'MMP_CHECK';
  MMP_STYLE_DARKMODE  = 'DarkMode';
  MMP_WIKI_URL        = 'https://minimalistmediaplayer.com';

  EMPTY               = '';
  BACKSLASH           = '\';
  MPV_ERROR_SUCCESS   = 0;
  SINGLE_SPACE        = ' ';

  _1KB = 1024;
  _1MB = 1024 * 1024;
  _1GB = 1024 * 1024 * 1024;
  _xGB = 1052266987; // 0.98 of 1GB

  SECS_PER_HOUR = 3600;

  MIN_SCALE_FACTOR      =  50;
  MAX_SCALE_FACTOR      = 100;
  DEFAULT_SCALE_FACTOR  =  90;

  WIN_AUTOCENTER_OFF    = WM_APP + 2001;
  WIN_CAPTION           = WM_APP + 2002;
  WIN_CLOSEAPP          = WM_APP + 2003;
  WIN_GREATER           = WM_APP + 2004;
  WIN_MAX_SIZE_OFF      = WM_APP + 2005;
  WIN_PAUSE_PLAY        = WM_APP + 2006;
  WIN_RESIZE            = WM_APP + 2007;
  WIN_START_OVER        = WM_APP + 2008;
  WIN_SYNC_MEDIA        = WM_APP + 2009;
  WIN_TAB               = WM_APP + 2010;
  WIN_TABTAB            = WM_APP + 2011;
  WIN_TOGGLE_CONTROLS   = WM_APP + 2012;
  WIN_TOGGLE_EDIT_MODE  = WM_APP + 2013;
  WIN_TOGGLE_REPEAT     = WM_APP + 2014;
  WIN_MUTE_UNMUTE       = WM_APP + 2015;

  WIN_TERMINATE         = WM_APP + 3000;

  POT_PLAYER   = 'C:\Program Files\DAUM\PotPlayer\PotPlayerMini64.exe';
  LOSSLESS_CUT = 'C:\Program Files\LosslessCut-win-x64\LosslessCut.exe';
  SHOTCUT      = 'C:\Program Files\Shotcut\shotcut.exe';

  PB_DEFAULT_BACKGROUND_COLOR = $000000 + 1; // clBlack + 1; just enough to be different from the clBlack transparent color

  PB_DEFAULT_COLOR = $202020; // higher is lighter
  ST_DEFAULT_COLOR = $707070;
  TL_DEFAULT_COLOR = $3F3F3F;
  PB_COLOR_DELTA   = $343434;

  DARK_MODE_DARK   = $2B2B2B;
  DARK_MODE_LIGHT  = $232323;
  DARK_MODE_SILVER = $C0C0C0; // clSilver
  DARK_MODE_DKGRAY = $808080; // clDkGray

  FONT_TAHOMA      = 'Tahoma';
  FONT_SEGOE_UI    = 'Segoe UI';

  DEFAULT_REPEAT_DELAY_MS = 100;
  MILLISECONDS     = 1000;

  THUMB_DEFAULT_SIZE  = 160;
  THUMB_MARGIN        = 10;
  THUMB_NO_IMAGES     = 'No images in this folder';

  UI_DEFAULT_AUDIO_HEIGHT = 56;

  IMAGE_DISPLAY_DURATION        = 2;    // in seconds, to match the setting in mpv.conf
  IMAGE_DISPLAY_DURATION_STRING = '2';

  SLIDESHOW_DELTA_MS          = 100;

  CMDLINE_OPTION_NOPLAYLIST   = 'noplaylist';

  CONF_ALLOW_INTO_WINDOWS     = 'allowIntoWindows';
  CONF_AUDIO_DELETE           = 'audioDelete';
  CONF_AUTO_UPDATE            = 'autoUpdate';
  CONF_BASE_FOLDER            = 'baseFolder';
  CONF_BROWSER                = 'browser';
  CONF_DELETE_METHOD          = 'deleteMethod';
  CONF_DIRTY_CHARS            = 'dirtyChars';
  CONF_EXIT_APP               = 'exitApp';
  CONF_EXIT_BROWSER           = 'exitBrowser';
  CONF_FOLDER_DELETE          = 'folderDelete';
  CONF_KEEP_DELETE            = 'keepDelete';
  CONF_IMAGE_DELETE           = 'imageDelete';
  CONF_LOGGING                = 'logging';
  CONF_MAIN                   = 'main';
  CONF_MAIN_CAPTION           = 'caption';
  CONF_MUTED                  = 'muted';
  CONF_NEXT_FOLDER_ON_EMPTY   = 'nextFolderOnEmpty';
  CONF_NEXT_FOLDER_ON_END     = 'nextFolderOnEnd';
  CONF_OPEN_IMAGE             = 'openImage';
  CONF_PLAY_EDITED            = 'playEdited';
  CONF_PLAYLIST_FORMAT        = 'playlistFormat';
  CONF_PROGRESS_BAR           = 'progressBar';
  CONF_REPEAT_DELAY_MS        = 'repeatDelayMs';
  CONF_SCALE_FACTOR           = 'scaleFactor';
  CONF_SHOW_METADATA          = 'showMetaData';
  CONF_SHUFFLE_MODE           = 'shuffleMode';
  CONF_SLIDESHOW_INTERVAL_MS  = 'slideshowIntervalMs';
  CONF_START_IN_EDITOR        = 'startInEditor';
  CONF_TIME_CAPTION           = 'timeCaption';
  CONF_VIDEO_DELETE           = 'videoDelete';
  CONF_VOLUME                 = 'volume';

  CONF_CAT_F1                 = 'catF1';
  CONF_CAT_F2                 = 'catF2';
  CONF_CAT_F3                 = 'catF3';
  CONF_CAT_F4                 = 'catF4';

  MPV_IMAGE_DISPLAY_DURATION  = 'image-display-duration';

  CONF_FOLDERS: array[1..16] of string
                            = ('baseFolder', 'copied', 'moved', 'saved', 'folder1', 'folder2', 'folder3', 'folder4', 'folder5', 'folder6',
                                                                         'folder7', 'folder8', 'folder9', 'folder10', 'folder11', 'folder12');

//  const DIRTY_CHARS = '!@#$^{}+=_`.%^''&';
  const DIRTY_CHARS = '''';
var
  nopoint: TPoint;

type
  TKeyOp = (koNone,
            koCloseApp, koVolUp, koVolDn, koTab, koTabTab, koPausePlay, koFrameForwards, koFrameBackwards, koBrightnessUp, koBrightnessDn,
            koZoomIn, koZoomOut, koStartOver, koShowCaption, koMuteUnmute, koPlayFirst, koPlayNext, koPlayPrev, koPlayLast, koPanLeft,
            koPanRight, koPanUp, koPanDn, koRotateR, koRotateL, koFullscreen, koZoomReset, koGreaterWindow, koToggleControls, koRunPot,
            koRunCut, koRunShot, koToggleProgressBar, koCentreWindow, koMinimizeWindow, koDeleteCurrentItem, koRenameFile, koSpeedUp, koSpeedDn, koSpeedReset,
            koEscape, koClipboard, koKeep, koReloadPlaylist, koPanReset, koBrightnessReset, koBookmarkSave, koBookmarkLoad, koBookmarkDelete, koRotateReset,
            koContrastUp, koContrastDn, koContrastReset, koGammaUp, koGammaDn, koSaturationUp, koSaturationDn, koGammaReset, koSaturationReset, koResetAll,
            koToggleHelp, koBrighterPB, koDarkerPB, koTogglePlaylist, koCloseEvery, koArrangeAll, koSyncMedia, koScreenshot, koToggleSubtitles, koToggleRepeat,
            koToggleEditMode, koAboutBox, koMaximize, koCycleAudio, koCycleSubs, koPrevChapter, koNextChapter, koThumbnails, koAdjustAspectRatio, koWiki,
            koToggleNumlock, koKeepDelete, koPlayNextFolder, koPlayPrevFolder, koImageInBrowser, koExploreFolder, koPBReset, koSysVolMax, koToggleFiltering, koCleanup,
            koKeepCatF1, koKeepCatF2, koKeepCatF3, koKeepCatF4, koKeepMove, koKeepSave, koConfig, koPlayEdited, koRenameCleanFile, koToggleSkipExcluded, koToggleShuffle);

  TDeleteMethod = (dmRecycle, dmStandard, dmShred);
  TKeyDirection = (kdDn, kdUp);
  TRenameType   = (rtUser, rtKeep, rtKeepCatF1, rtKeepCatF2, rtKeepCatF3, rtKeepCatF4, rtKeepMove, rtKeepSave, rtKeepClean);
  TReasonType   = (rtNextFolderUser, rtNextFolderOnEnd, rtNextFolderOnEmpty);

  TSnapshot = record
    key:              WORD;
    shiftState:       TShiftState;
    keyDirection:     TKeyDirection;
    keyOp:            TKeyOp;
    handled:          boolean;
  end;

  TMediaType = (mtUnk, mtAudio, mtVideo, mtImage, mtAudioVideo);
  TSetOfMediaType = set of TMediaType;
  TMediaTypeRec = record
    mimeType: string;
    mediaType: TMediaType;
    typeName: string;
    fileExts: string;
  end;

  THelpType = (htMain, htImages);
  TWndRec = record
    HWND:       HWND;
    pt:         TPoint;
    height:     integer;
    helpType:   THelpType;
    createNew:  boolean;
  end;

  TFnnKeyApp = (F10_APP, F11_APP, F12_APP);

  THostType = (htMPVHost, htThumbsHost);

  TSlideshowDirection = (sdForwards, sdBackwards);

  TAnonFunc = reference to function: boolean;

const
  mmpMediaTypeStrings: array[0..3] of string = ('unk', 'audio', 'video', 'image');
  FnnKeyApps: array[F10_APP..F12_APP] of string = ('F10', 'F11', 'F12');
  mediaTypes: array[0..89] of TMediaTypeRec = (

// manually added image formats
(mimeType: 'image/jpeg';            mediaType: mtImage; typeName: 'JPEG image';                 fileExts: '.jpg.jpeg.'),
(mimeType: 'image/bmp';             mediaType: mtImage; typeName: 'BITMAP image';               fileExts: '.bmp.'),
(mimeType: 'image/png';             mediaType: mtImage; typeName: 'PNG image';                  fileExts: '.png.'),
(mimeType: 'image/webp';            mediaType: mtImage; typeName: 'WEBP image';                 fileExts: '.webp.'),
(mimeType: 'image/gif';             mediaType: mtImage; typeName: 'GIF image';                  fileExts: '.gif.'),
(mimeType: 'image/avif';            mediaType: mtImage; typeName: 'AVIF image';                 fileExts: '.avif.'),
(mimeType: 'image/jfif';            mediaType: mtImage; typeName: 'JFIF image';                 fileExts: '.jfif.'),
(mimeType: 'image/jxl';             mediaType: mtImage; typeName: 'JXL image';                  fileExts: '.jxl.'),
(mimeType: 'image/vtx';             mediaType: mtImage; typeName: 'VTX image';                  fileExts: '.vtx.'),

// DVD/Blu-ray audio formats
(mimeType: 'audio/ac3';             mediaType: mtAudio; typeName: 'AC-3 Audio';                 fileExts: '.ac3.a52.'),
(mimeType: 'audio/eac3';            mediaType: mtAudio; typeName: 'E-AC-3 audio';               fileExts: '.eac3.'),
(mimeType: 'audio/vnd.dolby.mlp';   mediaType: mtAudio; typeName: 'MLP audio';                  fileExts: '.mlp.'),
(mimeType: 'audio/vnd.dts' ;        mediaType: mtAudio; typeName: 'DTS audio';                  fileExts: '.dts.'),
(mimeType: 'audio/vnd.dts.hd' ;     mediaType: mtAudio; typeName: 'DTS-HD audio';               fileExts: '.dts-hd.dtshd.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'TrueHD audio';               fileExts: '.true-hd.thd.truehd.thd+ac3.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'True audio';                 fileExts: '.tta.'),
// Uncompressed formats
(mimeType: '';                      mediaType: mtAudio; typeName: 'PCM audio';                  fileExts: '.pcm.'),
(mimeType: 'audio/wav';             mediaType: mtAudio; typeName: 'Wave audio';                 fileExts: '.wav.'),
(mimeType: 'audio/aiff';            mediaType: mtAudio; typeName: 'AIFF audio';                 fileExts: '.aiff.aif.aifc.'),
(mimeType: 'audio/amr';             mediaType: mtAudio; typeName: 'AMR audio';                  fileExts: '.amr.'),
(mimeType: 'audio/amr-wb';          mediaType: mtAudio; typeName: 'AMR-WB audio';               fileExts: '.awb.'),
(mimeType: 'audio/basic';           mediaType: mtAudio; typeName: 'AU audio';                   fileExts: '.au.snd.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'Linear PCM audio';           fileExts: '.lpcm.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw YUV video';              fileExts: '.yuv.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'YUV4MPEG2 video';            fileExts: '.y4m.'),
// Free lossless formats
(mimeType: 'audio/x-ape';           mediaType: mtAudio; typeName: 'Monkey''s audio';            fileExts: '.ape.'),
(mimeType: 'audio/x-wavpack';       mediaType: mtAudio; typeName: 'WavPack audio';              fileExts: '.wv.'),
(mimeType: 'audio/x-shorten';       mediaType: mtAudio; typeName: 'Shorten audio';              fileExts: '.shn.'),
// MPEG formats
(mimeType: 'video/vnd.dlna.mpeg-tts'; mediaType: mtVideo; typeName: 'MPEG-2 Transport Stream';  fileExts: '.m2ts.m2t.mts.mtv.ts.tsv.tsa.tts.trp.'),
(mimeType: 'audio/vnd.dlna.adts';   mediaType: mtAudio; typeName: 'ADTS audio';                 fileExts: '.adts.adt.'),
(mimeType: 'audio/mpeg';            mediaType: mtAudio; typeName: 'MPEG audio';                 fileExts: '.mpa.m1a.m2a.mp1.mp2.'),
(mimeType: 'audio/mpeg';            mediaType: mtAudio; typeName: 'MP3 audio';                  fileExts: '.mp3.'),
(mimeType: 'video/mpeg';            mediaType: mtVideo; typeName: 'MPEG video';                 fileExts: '.mpeg.mpg.mpe.mpeg2.m1v.m2v.mp2v.mpv.mpv2.mod.tod.'),
(mimeType: 'video/dvd';             mediaType: mtVideo; typeName: 'video Object';               fileExts: '.vob.vro.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Enhanced VOB';               fileExts: '.evob.evo.'),
(mimeType: 'video/mp4';             mediaType: mtVideo; typeName: 'MPEG-4 video';               fileExts: '.mpeg4.m4v.mp4.mp4v.mpg4.'),
(mimeType: 'audio/mp4';             mediaType: mtAudio; typeName: 'MPEG-4 audio';               fileExts: '.m4a.'),
(mimeType: 'audio/aac';             mediaType: mtAudio; typeName: 'Raw AAC audio';              fileExts: '.aac.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw H.264/AVC video';        fileExts: '.h264.avc.x264.264.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Raw H.265/HEVC video';       fileExts: '.hevc.h265.x265.265.'),
// Xiph formats
(mimeType: 'audio/flac';            mediaType: mtAudio; typeName: 'FLAC audio';                 fileExts: '.flac.'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Ogg audio';                  fileExts: '.oga.ogg.'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Opus audio';                 fileExts: '.opus.'),
(mimeType: 'audio/ogg';             mediaType: mtAudio; typeName: 'Speex audio';                fileExts: '.spx.'),
(mimeType: 'video/ogg';             mediaType: mtVideo; typeName: 'Ogg video';                  fileExts: '.ogv.ogm.'),
(mimeType: 'application/ogg';       mediaType: mtVideo; typeName: 'Ogg video';                  fileExts: '.ogx.'),
// Matroska formats
(mimeType: 'video/x-matroska';      mediaType: mtVideo; typeName: 'Matroska video';             fileExts: '.mkv.'),
(mimeType: 'video/x-matroska';      mediaType: mtVideo; typeName: 'Matroska 3D video';          fileExts: '.mk3d.'),
(mimeType: 'audio/x-matroska';      mediaType: mtAudio; typeName: 'Matroska audio';             fileExts: '.mka.'),
(mimeType: 'video/webm';            mediaType: mtVideo; typeName: 'WebM video';                 fileExts: '.webm.'),
(mimeType: 'audio/webm';            mediaType: mtAudio; typeName: 'WebM audio';                 fileExts: '.weba.'),
// Misc formats
(mimeType: 'video/avi';             mediaType: mtVideo; typeName: 'video Clip';                 fileExts: '.avi.vfw.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'DivX video';                 fileExts: '.divx.'),
(mimeType: '';                      mediaType: mtVideo; typeName: '3ivx video';                 fileExts: '.3iv.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'XVID video';                 fileExts: '.xvid.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'NUT video';                  fileExts: '.nut.'),
(mimeType: 'video/flc';             mediaType: mtVideo; typeName: 'FLIC video';                 fileExts: '.flic.fli.flc.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Nullsoft Streaming video';   fileExts: '.nsv.'),
(mimeType: 'application/gxf';       mediaType: mtVideo; typeName: 'General Exchange Format';    fileExts: '.gxf.'),
(mimeType: 'application/mxf';       mediaType: mtVideo; typeName: 'Material Exchange Format';   fileExts: '.mxf.'),
// Windows Media formats
(mimeType: 'audio/x-ms-wma';        mediaType: mtAudio; typeName: 'Windows Media audio';        fileExts: '.wma.'),
(mimeType: 'video/x-ms-wm';         mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.wm.'),
(mimeType: 'video/x-ms-wmv';        mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.wmv.'),
(mimeType: 'video/x-ms-asf';        mediaType: mtVideo; typeName: 'Windows Media video';        fileExts: '.asf.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Microsoft Recorded TV Show'; fileExts: '.dvr-ms.dvr.'),
(mimeType: '';                      mediaType: mtVideo; typeName: 'Windows Recorded TV Show';   fileExts: '.wtv.'),
// DV formats
(mimeType: '';                      mediaType: mtVideo; typeName: 'DV video';                   fileExts: '.dv.hdv.'),
// Flash Video formats
(mimeType: 'video/x-flv';           mediaType: mtVideo; typeName: 'Flash video';                fileExts: '.flv.'),
(mimeType: 'video/mp4';             mediaType: mtVideo; typeName: 'Flash video';                fileExts: '.f4v.'),
(mimeType: 'audio/mp4';             mediaType: mtAudio; typeName: 'Flash audio';                fileExts: '.f4a.'),
// QuickTime formats
(mimeType: 'video/quicktime';       mediaType: mtVideo; typeName: 'QuickTime video';            fileExts: '.qt.mov.'),
(mimeType: 'video/quicktime';       mediaType: mtVideo; typeName: 'QuickTime HD video';         fileExts: '.hdmov.'),
// Real Media formats
(mimeType: 'application/vnd.rn-realmedia';     mediaType: mtVideo; typeName: 'Real Media video'; fileExts: '.rm.'),
(mimeType: 'application/vnd.rn-realmedia-vbr'; mediaType: mtVideo; typeName: 'Real Media video'; fileExts: '.rmvb.'),
(mimeType: 'audio/vnd.rn-realaudio'; mediaType: mtAudio; typeName: 'Real Media audio';          fileExts: '.ra.ram.'),
// 3GPP formats
(mimeType: 'audio/3gpp';            mediaType: mtAudio; typeName: '3GPP audio';                 fileExts: '.3ga.'),
(mimeType: 'audio/3gpp2';           mediaType: mtAudio; typeName: '3GPP audio';                 fileExts: '.3ga2.'),
(mimeType: 'video/3gpp';            mediaType: mtVideo; typeName: '3GPP video';                 fileExts: '.3gpp.3gp.'),
(mimeType: 'video/3gpp2';           mediaType: mtVideo; typeName: '3GPP video';                 fileExts: '.3gp2.3g2.'),
// Video game formats
(mimeType: '';                      mediaType: mtAudio; typeName: 'AY audio';                   fileExts: '.ay.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'GBS audio';                  fileExts: '.gbs.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'GYM audio';                  fileExts: '.gym.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'HES audio';                  fileExts: '.hes.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'KSS audio';                  fileExts: '.kss.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'NSF audio';                  fileExts: '.nsf.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'NSFE audio';                 fileExts: '.nsfe.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'SAP audio';                  fileExts: '.sap.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'SPC audio';                  fileExts: '.spc.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'VGM audio';                  fileExts: '.vgm.'),
(mimeType: '';                      mediaType: mtAudio; typeName: 'VGZ audio';                  fileExts: '.vgz ')
// Playlist formats
//(mimeType: 'audio/x-mpegurl';       mediaType: mtAudio; typeName: 'M3U Playlist';               fileExts: '.m3u.m3u8'),
//(mimeType: 'audio/x-scpls';         mediaType: mtAudio; typeName: 'PLS Playlist';               fileExts: '.pls')
);

implementation

uses
  system.types;

initialization
  noPoint := point(0, 0);

end.
