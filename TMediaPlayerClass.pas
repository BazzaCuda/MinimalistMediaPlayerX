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
unit TMediaPlayerClass;

interface

uses
  system.classes,
  vcl.extCtrls, vcl.forms,
  mmpConsts, MPVBasePlayer;

type
  TTimerEvent = (tePlay, teClose);

  TPositionNotifyEvent = procedure(const aMax: integer; const aPosition: integer) of object;

  TMediaPlayer = class(TObject)
  strict private
    mpv: TMPVBasePlayer;
    FAllowBrowser:            boolean;
    FTimer:                   TTimer;
    FTimerEvent:              TTimerEvent;

    FBlankOutTimeCaption:     TAnonFunc;
    FResetTimeCaption:        TAnonFunc;

    FDontPlayNext:            boolean;
    FImageDisplayDuration:    string;
    FImageDisplayDurationMs:  double;
    FImagePaused:             boolean;
    FLocked:                  boolean;
    FMediaType:               TMediaType;
    FOnBeforeNew:             TNotifyEvent;
    FOnPlayNew:               TNotifyEvent;
    FOnPlayNext:              TNotifyEvent;
    FOnPosition:              TPositionNotifyEvent;
    FPlaying:                 boolean;
    FScreenshotDirectory:     string;
  private
    function  createTimeCaptionClosures: boolean;

    procedure onInitMPV(sender: TObject);
    procedure onTimerEvent(sender: TObject);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);

    function getFormattedDuration:  string;
    function getFormattedTime:      string;

    function pauseUnpauseImages:    boolean;

    {property setters}
    function  getDuration:    integer;
    function  getPosition:    integer;
    function  getVideoHeight: int64;
    function  getVideoWidth:  int64;
    procedure setKeepOpen(const value: boolean);
    procedure setPosition(const value: integer);
  public
    constructor create;
    destructor  Destroy; override;
    function allReset:            string;
    function autoPlayNext:        boolean;
    function brightnessDn:        string;
    function brightnessReset:     string;
    function brightnessUp:        string;
    function ceaseOps:            boolean;
    function chapterNext:         boolean;
    function chapterPrev:         boolean;
    function contrastUp:          string;
    function contrastDn:          string;
    function contrastReset:       string;
    function cycleAudio:          boolean;
    function cycleSubs:           boolean;
    function frameBackwards:      boolean;
    function frameForwards:       boolean;
    function gammaDn:             string;
    function gammaReset:          string;
    function gammaUp:             string;
    function initMediaPlayer:     boolean;
    function muteUnmute:          string;
    function openURL(const aURL: string): boolean;
    function panDn:               string;
    function panLeft:             string;
    function panReset:            string;
    function panRight:            string;
    function panUp:               string;
    function pause:               boolean;
    function pausePlay:           boolean;
    function play(const aURL: string): boolean;
    function playCurrent:         boolean;
    function playFirst:           boolean;
    function playLast:            boolean;
    function playNext:            boolean;
    function playNextFolder:      boolean;
    function playPrev:            boolean;
    function playPrevFolder:      boolean;
    function releasePlayer:       boolean;
    function resume:              boolean;
    function rotateLeft:          string;
    function rotateReset:         string;
    function rotateRight:         string;
    function saturationDn:        string;
    function saturationReset:     string;
    function saturationUp:        string;
    function setProgressBar:      boolean;
    function speedDn:             string;
    function speedReset:          string;
    function speedUp:             string;
    function startOver:           string;
    function stop:                boolean;
    function tab(const capsLock: boolean; const aFactor: integer = 0): string;
    function takeScreenshot:      string;
    function toggleFullscreen:    boolean;
    function toggleRepeat:        string;
    function toggleSubtitles:     string;
    function volDown:             string;
    function volUp:               string;
    function zoomIn:              string;
    function zoomOut:             string;
    function zoomReset:           string;
    property allowBrowser:        boolean                         write FAllowBrowser;
    property dontPlayNext:        boolean      read FDontPlayNext write FDontPlayNext;
    property duration:            integer      read getDuration;
    property formattedDuration:   string       read getFormattedDuration;
    property formattedTime:       string       read getFormattedTime;
    property imagesPaused:        boolean      read FImagePaused;
    property keepOpen:            boolean                         write setKeepOpen;
    property mediaType:           TMediaType   read FMediaType;
    property onBeforeNew:         TNotifyEvent read FOnBeforeNew  write FOnBeforeNew;
    property onPlayNew:           TNotifyEvent read FOnPlayNext   write FOnPlayNew;
    property onPlayNext:          TNotifyEvent read FOnPlayNext   write FOnPlayNext;
    property onPosition:          TPositionNotifyEvent read FOnPosition write FOnPosition;
    property playing:             boolean      read FPlaying      write FPlaying;
    property position:            integer      read getPosition   write setPosition;
    property videoHeight:         int64        read getVideoHeight;
    property videoWidth:          int64        read getVideoWidth;
  end;

implementation

uses
  winAPI.windows,
  system.sysUtils,
  vcl.controls, vcl.graphics,
  mpvConst,
  mmpFileUtils, mmpFolderNavigation, mmpKeyboard, mmpMPVCtrls, mmpMPVFormatting, mmpMPVProperties, mmpSingletons, mmpSysCommands, mmpUtils,
  formCaptions, formHelp, formMediaCaption,
  _debugWindow;

{ TMediaPlayer }

  function TMediaPlayer.allReset: string;
  begin
    brightnessReset;
    contrastReset;
    gammaReset;
    panReset;
    rotateReset;
    saturationReset;
    speedReset;
    zoomReset;
    result := 'All reset';
  end;

function TMediaPlayer.autoPlayNext: boolean;
begin
  case FImagePaused AND (FMediaType = mtImage) of TRUE: EXIT; end;
  playNext;
end;

function TMediaPlayer.brightnessDn: string;
begin
  result := mpvBrightnessDn(mpv);
end;

function TMediaPlayer.brightnessReset: string;
begin
  result := mpvBrightnessReset(mpv);
 end;

function TMediaPlayer.brightnessUp: string;
begin
  result := mpvBrightnessUp(mpv);
end;

function TMediaPlayer.ceaseOps: boolean;
begin
  mmpCancelDelay;
  FDontPlayNext := TRUE;
  mpvPause(mpv);
end;

function TMediaPlayer.chapterNext: boolean;
begin
  mpvChapterNext(mpv);
end;

function TMediaPlayer.chapterPrev: boolean;
begin
  mpvChapterPrev(mpv);
end;

function TMediaPlayer.contrastDn: string;
begin
  result := mpvContrastDn(mpv);
end;

function TMediaPlayer.contrastReset: string;
begin
  result := mpvContrastReset(mpv);
end;

function TMediaPlayer.contrastUp: string;
begin
  result := mpvContrastUp(mpv);
end;

constructor TMediaPlayer.create;
begin
  inherited;
  FImagePaused    := TRUE;
  FTimer          := TTimer.create(NIL);
  FTimer.enabled  := FALSE;
  FTimer.OnTimer  := onTimerEvent;
  FAllowBrowser   := TRUE;
  createTimeCaptionClosures;
end;

function TMediaPlayer.cycleAudio: boolean;
begin
  mpvCycleAudio(mpv);
end;

function TMediaPlayer.cycleSubs: boolean;
begin
  mpvCycleSubs(mpv);
end;

destructor TMediaPlayer.Destroy;
begin
  case FTimer <> NIL of TRUE: FTimer.free; end;
  releasePlayer;
  inherited;
end;

function TMediaPlayer.frameBackwards: boolean;
begin
  mpvFrameBackwards(mpv);
end;

function TMediaPlayer.frameForwards: boolean;
begin
  mpvFrameForwards(mpv);
end;

function TMediaPlayer.gammaDn: string;
begin
  result := mpvGammaDn(mpv);
end;

function TMediaPlayer.gammaReset: string;
begin
  result := mpvGammaReset(mpv);
end;

function TMediaPlayer.gammaUp: string;
begin
  result := mpvGammaUp(mpv);
end;

function TMediaPlayer.getDuration: integer;
begin
  result := mpvDuration(mpv);
end;

function TMediaPlayer.getFormattedDuration: string;
begin
  result := mpvFormattedDuration(mpv);
end;

function TMediaPlayer.getFormattedTime: string;
begin
  result := mpvFormattedTime(mpv);
end;

function TMediaPlayer.getPosition: integer;
begin
  result := mpvPosition(mpv);
end;

function TMediaPlayer.getVideoHeight: int64;
begin
  result := mpvVideoHeight(mpv);
end;

function TMediaPlayer.getVideoWidth: int64;
begin
  result := mpvVideoWidth(mpv);
end;

function TMediaPlayer.initMediaPlayer: boolean;
begin
//
end;

function TMediaPlayer.muteUnmute: string;
begin
  result := mpvMuteUnmute(mpv);
end;

procedure TMediaPlayer.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(sender as TMPVBasePlayer, mmpExePath);
end;

procedure TMediaPlayer.onStateChange(cSender: TObject; eState: TMPVPlayerState);
// no mpsStop event as yet
begin
  FPlaying := eState = mpsPlay;

  case (FMediaType = mtImage) and FImagePaused of TRUE:
         case eState of mpsPlay: begin FLocked := FALSE; EXIT; end;end;end;

  case (FMediaType = mtImage) and NOT FImagePaused of TRUE:
        case  eState of mpsPlay, mpsEnd:  begin FLocked := FALSE;
                                                case FDontPlayNext of FALSE:  begin
                                                                                mmpDelay(trunc(FImageDisplayDurationMs)); // code-controlled slideshow
                                                                                case FMediaType = mtImage of TRUE: playNext; end;end;end;end;end;end;

  case (FMediaType <> mtImage) of TRUE:
        case eState of  mpsPlay:        FLocked := FALSE;
                         mpsEnd: begin  FLocked := FALSE;
                                        case FDontPlayNext of FALSE: playNext; end;end;end;end;

end;

procedure TMediaPlayer.onTimerEvent(sender: TObject);
begin
  FTimer.enabled := FALSE;
  case FTimerEvent of
    tePlay:  play(PL.currentItem);
    teClose: begin ceaseOps; mmpSendSysCommandClose; end;
  end;
end;

function TMediaPlayer.openURL(const aURL: string): boolean;
begin
  result := FALSE;

  case mpv = NIL of TRUE: begin
    mpvCreate(mpv);

    mpv.OnStateChged := onStateChange;
    mpv.onInitMPV    := onInitMPV;

    mpvInitPlayer(mpv, UI.handle, mmpExePath, mmpExePath);  // THIS RECREATES THE INTERNAL MPV OBJECT IN TMVPBasePlayer
    mpvGetPropertyString(mpv, 'screenshot-directory', FScreenshotDirectory);

    case FImageDisplayDurationMs = 0 of TRUE: begin
                                                mpvGetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, FImageDisplayDuration);
                                                case tryStrToFloat(FImageDisplayDuration, FImageDisplayDurationMs) of FALSE: FImageDisplayDurationMs := IMAGE_DISPLAY_DURATION; end;

                                                FImageDisplayDurationMs := FImageDisplayDurationMs * 1000;

                                                FImageDisplayDuration := 'autoPlayNext'; // anything except 'inf'
                                                mpvSetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, 'inf'); end;end; // get the user's duration setting, if any, then override it.
  end;end;

  mpvOpenFile(mpv, aURL);

//  mpvSetPropertyString(mpv, 'start', '#9');

  result := TRUE;
end;

function TMediaPlayer.panDn: string;
begin
  result := mpvPanDn(mpv);
end;

function TMediaPlayer.panLeft: string;
begin
  result := mpvPanLeft(mpv);
end;

function TMediaPlayer.panReset: string;
begin
  result := mpvPanReset(mpv);
end;

function TMediaPlayer.panRight: string;
begin
  result := mpvPanRight(mpv);
end;

function TMediaPlayer.panUp: string;
begin
  result := mpvPanUp(mpv);
end;

function TMediaPlayer.pause: boolean;
begin
  mpvPause(mpv);
end;

function TMediaPlayer.pauseUnpauseImages: boolean;
begin
  case FMediaType = mtImage of FALSE: EXIT; end;
  FImagePaused := NOT FImagePaused;

  case FImagePaused of FALSE: playnext; end;

  case FImagePaused of  TRUE: ST.opInfo := 'slideshow paused';
                       FALSE: ST.opInfo := 'slideshow unpaused'; end;

  FDontPlayNext := FImagePaused;
end;

function TMediaPlayer.pausePlay: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;

  pauseUnpauseImages;

  case mpvState(mpv) of
    mpsPlay:  mpvPause(mpv);
    mpsPause: mpvResume(mpv);
  end;
end;

function TMediaPlayer.createTimeCaptionClosures: boolean;
var
  vColor: TColor;
begin
  vColor := 0;

  FBlankOutTimeCaption  := function:boolean  begin
                                              case vColor = 0 of TRUE:  begin
                                                                          vColor := ST.color;
                                                                          ST.color := $00000000; end;end;end;
  FResetTimeCaption     := function:boolean begin
                                              case vColor = 0 of FALSE: begin
                                                                          ST.color := vColor;
                                                                          vColor   := 0; end;end;end;
end;

function TMediaPlayer.play(const aURL: string): boolean;
begin
  result := FALSE;

  case assigned(FOnBeforeNew) of TRUE: FOnBeforeNew(SELF); end;

  MI.getMediaInfo(aURL);

  FMediaType := MT.mediaType(lowerCase(extractFileExt(PL.currentItem)));

  case FMediaType of mtImage: FBlankOutTimeCaption;
                         else FResetTimeCaption; end;


  mmpProcessMessages;

  openURL(aURL);

  mpvSetVolume(mpv, CF.asInteger['volume']);  // really only affects the first audio/video played
  mpvSetMute(mpv, CF.asBoolean['muted']);     // ditto

  case GV.autoCentre of  TRUE: case FMediaType of          mtVideo: UI.setWindowSize(-1, []); // must be done after MPV has opened the video
                                                  mtAudio, mtImage: UI.setWindowSize(UI.height, []); end;
                        FALSE: UI.setWindowSize(UI.height, []); end;
  UI.centreCursor;

  case FMediaType = mtVideo of TRUE: UI.tweakWindow; end;

  FDontPlayNext := (FMediaType = mtImage) and (FImageDisplayDuration = 'inf');

  case ST.showData of TRUE: MI.getMetaData(ST.dataMemo); end;
  MC.caption := PL.formattedItem;

  mmpProcessMessages;

  SA.postToAll(WM_PROCESS_MESSAGES, KBNumLock);

  case assigned(FOnPlayNew) of  TRUE: FOnPlayNew(SELF); end;
  UI.centreCursor;

  case FAllowBrowser and (FMediaType = mtImage) and (lowerCase(CF['openImage']) = 'browser') of TRUE: begin FAllowBrowser := FALSE; UI.showThumbnails(htMPVHost); end;end;

//  FAllowBrowser := FALSE; // only allow the launch image

  result := TRUE;
end;

function TMediaPlayer.playCurrent: boolean;
begin
  pause;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := TRUE;
end;

function TMediaPlayer.playFirst: boolean;
begin
  pause;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.first;
end;

function TMediaPlayer.playLast: boolean;
begin
  pause;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.last;
end;

function TMediaPlayer.playNext: boolean;
begin
  case GV.closeApp of TRUE: EXIT; end;
  pause;

  var vDontExit := PL.isLast and (FMediaType = mtImage);

  case FLocked of TRUE: EXIT; end;
  FLocked := TRUE;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.next;
  case FTimer.enabled of FALSE: begin
                                  case vDontExit of TRUE: EXIT; end; // ah, the irony!
                                  FTimerEvent    := teClose;
                                  FTimer.enabled := TRUE;
                                end;
                          TRUE: case assigned(FOnPlayNext) of TRUE: FOnPlayNext(SELF); end;end; // currently just updates the playlist window
end;

function TMediaPlayer.playNextFolder: boolean;
// reload playlist from vNextFolder and play first item
var
  vNextFolder: string;
begin
  vNextFolder := mmpNextFolder(PL.currentFolder, nfForwards);
  ST.opInfo := vNextFolder;
  case vNextFolder = '' of FALSE: PL.fillPlaylist(vNextFolder) end;
  case PL.hasItems of  TRUE: play(PL.currentItem);
                      FALSE: mpvStop(mpv); end; // if the folder is empty we want a blank screen
  result := vNextFolder <> '';
end;

function TMediaPlayer.playPrev: boolean;
begin
  pause;

  case FLocked of TRUE: EXIT; end;
  FLocked := TRUE;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.prev;
end;

function TMediaPlayer.playPrevFolder: boolean;
// reload playlist from vPrevFolder and play first item
var
  vPrevFolder: string;
begin
  vPrevFolder := mmpNextFolder(PL.currentFolder, nfBackwards);
  ST.opInfo := vPrevFolder;
  case vPrevFolder = '' of FALSE: PL.fillPlaylist(vPrevFolder); end;
  case PL.hasItems of  TRUE: play(PL.currentItem);
                      FALSE: mpvStop(mpv); end; // if the folder is empty we want a blank screen
  result := vPrevFolder <> '';
end;

function TMediaPlayer.resume: boolean;
begin
  mpvResume(mpv);
end;

function TMediaPlayer.rotateLeft: string;
begin
  result := mpvRotateLeft(mpv);
end;

function TMediaPlayer.rotateReset: string;
begin
  result := mpvRotateReset(mpv);
end;

function TMediaPlayer.rotateRight: string;
begin
  result := mpvRotateRight(mpv);
end;

function TMediaPlayer.releasePlayer: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  freeAndNIL(mpv);
end;

function TMediaPlayer.saturationDn: string;
begin
  result := mpvSaturationDn(mpv);
end;

function TMediaPlayer.saturationReset: string;
begin
  result := mpvSaturationReset(mpv);
end;

function TMediaPlayer.saturationUp: string;
begin
  result := mpvSaturationUp(mpv);
end;

procedure TMediaPlayer.setKeepOpen(const value: boolean);
begin
  mpvSetKeepOpen(mpv, value); // ensure libmpv MPV_EVENT_END_FILE_ event at the end of every media file
end;

procedure TMediaPlayer.setPosition(const value: integer);
begin
  mpvSeek(mpv, value);
  postMessage(GV.appWnd, WM_TICK, 0, 0); // immediately update the time
end;

function TMediaPlayer.setProgressBar: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  case PB.max <> trunc(mpvDuration(mpv)) of TRUE: PB.max := trunc(mpvDuration(mpv)); end;
  case mpvDuration(mpv) > 0 of TRUE: PB.position := trunc(mpvPosition(mpv)); end;

  case assigned(FOnPosition) of TRUE: FOnPosition(trunc(mpvDuration(mpv)), trunc(mpvPosition(mpv))); end;
end;

function TMediaPlayer.speedDn: string;
begin
  result := mpvSpeedDn(mpv);
end;

function TMediaPlayer.speedReset: string;
begin
  result := mpvSpeedReset(mpv);
  mmpDelay(100);
end;

function TMediaPlayer.speedUp: string;
begin
  result := mpvSpeedUp(mpv);
end;

function TMediaPlayer.startOver: string;
begin
  result := mpvStartOver(mpv);
end;

function TMediaPlayer.stop: boolean;
begin
  mpvStop(mpv);
end;

function TMediaPlayer.tab(const capsLock: boolean; const aFactor: integer = 0): string;
var
  vFactor: integer;
  vTab: integer;
  newInfo: string;
begin
  case aFactor > 0 of  TRUE: vFactor := aFactor;
                      FALSE: vFactor := 100; end;

  case capsLock of TRUE: vFactor := 200; end; // alt-key does the same as it can be a pain having the CapsLock key on all the time
  case ssShift in mmpShiftState of TRUE: vFactor := 50; end;

  vTab := trunc(duration / vFactor);
  case (vTab = 0) or (aFactor = -1) of TRUE: vTab := 1; end;

  case ssCtrl  in mmpShiftState of  TRUE: position := position - vTab;
                                   FALSE: position := position + vTab; end;

  case aFactor = -1 of  TRUE: newInfo := 'TAB = 1s';
                       FALSE: newInfo := format('%dth = %s', [vFactor, mmpFormatSeconds(round(duration / vFactor))]); end;

  case ssCtrl in mmpShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                  FALSE: newInfo := '>> ' + newInfo;
  end;
  result := newInfo;
end;

function TMediaPlayer.takeScreenshot: string;
begin
  case FScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, PL.currentFolder);           // otherwise screenshots of an image go to Windows/System32 !!
                                    FALSE: result := mpvTakeScreenshot(mpv, FScreenshotDirectory); end;
end;

function TMediaPlayer.toggleFullscreen: boolean;
begin
  UI.toggleMaximized;
  postMessage(GV.appWnd, WM_TICK, 0, 0);
end;

function TMediaPlayer.toggleRepeat: string;
begin
  result := mpvToggleRepeat(mpv);
end;

function TMediaPlayer.toggleSubtitles: string;
begin
  result := mpvToggleSubtitles(mpv);
end;

function TMediaPlayer.volDown: string;
begin
  result := mpvVolDown(mpv);
end;

function TMediaPlayer.volUp: string;
begin
  result := mpvVolUp(mpv);
end;

function TMediaPlayer.zoomIn: string;
begin
  result := mpvZoomIn(mpv);
end;

function TMediaPlayer.zoomOut: string;
begin
  result := mpvZoomOut(mpv);
end;

function TMediaPlayer.zoomReset: string;
begin
  result := mpvZoomReset(mpv);
end;

end.
