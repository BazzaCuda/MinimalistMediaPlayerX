unit MPVConst;

interface

const
  // see options.c  & https://mpv.io/manual/master/
  // Options
  STR_PATH = 'path';                   STR_DWIDTH = 'dwidth';
  STR_DHEIGHT = 'dheight';             STR_AUDIO_DELAY = 'audio-delay';
  STR_SUB_DELAY = 'sub-delay';         STR_SUB_POS = 'sub-pos';
  STR_SUB_SPEED = 'sub-speed';         STR_AUDIO_DEV_LIST = 'audio-device-list';
  STR_AS_CORRECT = 'audio-speed-correction';  STR_VERSION = 'version';
  STR_VS_CORRECT = 'video-speed-correction';
  STR_TASKBAR_PROG = 'taskbar-progress'; STR_AUDIO_FILE = 'audio-file';
  STR_SNAP_WIN = 'snap-window';        STR_ONTOP= 'ontop';
  STR_ONTOP_LEVEL = 'ontop-level';     STR_TRACK_LIST = 'track-list';
  STR_DURATION = 'duration';           STR_AUTOFIT= 'autofit';
  STR_WIN_SCALE = 'window-scale';      STR_WIN_MIN = 'window-minimized';
  STR_WIN_MAX = 'window-maximized';    STR_FULL_SCREEN= 'fullscreen';
  STR_FS_SCREEN_NAME = 'fs-screen-name';
  STR_VIDEO_ZOOM = 'video-zoom';       STR_WID = 'wid';
  STR_SCREEN = 'screen';               STR_KEEP_ASPECT = 'keepaspect';
  STR_PAUSE = 'pause';                 STR_TITLE = 'title';
  STR_SPEED = 'speed';                 STR_VOLUME = 'volume';
  STR_AID = 'aid';                     STR_VID = 'vid';
  STR_SID = 'sid';                     STR_AUDIO_DEV = 'audio-device';
  STR_WIDTH = 'width';                 STR_HEIGHT = 'height';
  STR_VIDEO_ROTATE = 'video-rotate';   STR_SHUFFLE = 'shuffle';
  STR_MUTE = 'mute';                   STR_PLAY_TIME = 'playback-time';
  STR_ALANG = 'alang';                 STR_SLANG = 'slang';
  STR_VLANG = 'vlang';                 STR_SUB_FILE = 'sub-file';
  STR_CHAP_LIST = 'chapter-list';      STR_BRIGHTNESS = 'brightness';
  STR_CONTRAST = 'contrast';           STR_SATURATION = 'saturation';
  STR_GAMMA = 'gamma';                 STR_HUE = 'hue';
  STR_VASPECT = 'video-aspect';        STR_LOG_FILE = 'log-file';
  STR_STM_BUF_SZ = 'stream-buffer-size';
  STR_CACHE = 'cache'; // demuxer-max-bytes / demuxer-max-back-bytes
  STR_SUB_AUTO = 'sub-auto';           STR_SUB_FILE_PATHS = 'sub-file-paths';
  STR_TIME_POS = 'time-pos';           STR_PERCENT_POS = 'percent-pos';
  STR_TIME_REMAIN = 'time-remaining';  STR_SEEKABLE = 'seekable';
  STR_CHAPTERS = 'chapters';           STR_CHAPTER = 'chapter';

  // Observe property change ID
  ID_PLAY_TIME = 1;                    ID_PAUSE = 2;
  ID_VOLUME = 3;                       ID_MUTE = 4;
  ID_AID = 5;                          ID_VID = 6;
  ID_SID = 7;                          ID_DURATION = 8;
  ID_FULL_SCREEN = 9;                  ID_SCREEN = 10;
  ID_VIDEO_ZOOM = 11;                  ID_TRACK_LIST = 12;
  ID_CHAP_LIST = 13;                   ID_AUDIO_DEV = 14;
  ID_SPEED = 15;                       ID_FS_SCREEN_NAME = 16;
  ID_ONTOP = 17;                       ID_ONTOP_LEVEL = 18;
  ID_TASKBAR_PROG = 19;                ID_SNAP_WIN = 20;
  ID_AUTOFIT = 21;                     ID_WIN_SCALE = 22;
  ID_WIN_MIN = 23;                     ID_WIN_MAX = 24;
  ID_WID = 25;                         ID_KEEP_ASPECT = 26;
  ID_TITLE = 27;                       ID_WIDTH = 28;
  ID_HEIGHT = 29;                      ID_VIDEO_ROTATE = 30;
  ID_SHUFFLE = 31;                     ID_ALANG = 32;
  ID_SLANG = 33;                       ID_VLANG = 34;
  ID_SUB_FILE = 35;                    ID_AUDIO_DEV_LIST = 36;

  // Commands
  CMD_LOAD_FILE = 'loadfile';
  CMD_SEEK = 'seek';
  CMD_STOP = 'stop';
  CMD_STEP = 'frame-step';
  CMD_BACK_STEP = 'frame-back-step';
  CMD_SHOW_TEXT = 'show-text';
  CMD_PRN_TEXT = 'print-text';
  CMD_SHOW_PROG = 'show-progress'; // show progress text/bar
  CMD_SUB_ADD = 'sub-add';
  CMD_SUB_REMOVE = 'sub-remove';
  CMD_VIDEO_RELOAD = 'video-reload';
  CMD_SCREEN_SHOT = 'screenshot';
  CMD_SCREEN_SHOT_FILE = 'screenshot-to-file';
  CMD_AB_LOOP = 'ab-loop';
  CMD_OVERLAY_ADD = 'overlay-add';
  CMD_OVERLAY_DEL = 'overlay-remove';
  CMD_OSD_OVERLAY = 'osd-overlay';
  CMD_LOAD_SCRIPT = 'load-script';
  CMD_AUDIO_FILTER = 'af';
  CMD_VIDEO_FILTER = 'vf';
  CMD_AF_CMD = 'af-command';
  CMD_VF_CMD = 'vf-command';
  CMD_LOAD_LIST = 'loadlist';
  CMD_PLIST_NEXT = 'playlist-next';
  CMD_PLIST_PREV = 'playlist-prev';
  CMD_PLIST_SHUF = 'playlist-shuffle';

implementation

end.
