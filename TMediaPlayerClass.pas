{   Minimalist Media Player
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
  forms, vcl.extCtrls, system.classes, MPVBasePlayer, consts;

type
  TTimerEvent = (tePlay, teClose);

  TPositionNotifyEvent = procedure(const aMax: integer; const aPosition: integer) of object;

  TMediaPlayer = class(TObject)
  strict private
    mpv: TMPVBasePlayer;
    FTimer: TTimer;
    FTimerEvent: TTimerEvent;

    FAlwaysPot: boolean;
    FDontPlayNext: boolean;
    FImageDisplayDuration: string;
    FImageDisplayDurationSS: double;
    FImagePaused: boolean;
    FMediaType: TMediaType;
    FMuted: boolean;
    FOnBeforeNew: TNotifyEvent;
    FOnPlayNew: TNotifyEvent;
    FOnPlayNext: TNotifyEvent;
    FOnPosition: TPositionNotifyEvent;
    FPlaying: boolean;
    FPausePlay: boolean;
    FRepeat: boolean;
    FScreenshotDirectory: string;
    FVol:  double;
  private
    constructor create;
    procedure onInitMPV(sender: TObject);
    procedure onTimerEvent(sender: TObject);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);

    function checkPot: boolean;
    function getFormattedBrightness: string;
    function getFormattedContrast: string;
    function getFormattedDuration: string;
    function getFormattedGamma: string;
    function getFormattedSaturation: string;
    function getFormattedSpeed: string;
    function getFormattedTime: string;
    function getFormattedVol: string;

    function pauseUnpauseImages: boolean;

    {property setters}
    function  getDuration: integer;
    function  getPosition: integer;
    function  getVideoHeight: int64;
    function  getVideoWidth: int64;
    procedure setKeepOpen(const Value: boolean);
    procedure setPosition(const Value: integer);
  public
    destructor  Destroy; override;
    function allReset: string;
    function autoPlayNext: boolean;
    function brightnessDn: string;
    function brightnessReset: string;
    function brightnessUp: string;
    function chapterNext: boolean;
    function chapterPrev: boolean;
    function contrastUp: string;
    function contrastDn: string;
    function contrastReset: string;
    function cycleAudio: boolean;
    function cycleSubs: boolean;
    function frameBackwards: boolean;
    function frameForwards: boolean;
    function gammaDn: string;
    function gammaReset: string;
    function gammaUp: string;
    function initMediaPlayer: boolean;
    function muteUnmute: string;
    function openURL(const aURL: string): boolean;
    function panDn(const aShiftState: TShiftState): string;
    function panLeft(const aShiftState: TShiftState): string;
    function panReset: string;
    function panRight(const aShiftState: TShiftState): string;
    function panUp(const aShiftState: TShiftState): string;
    function pause: boolean;
    function pausePlay: boolean;
    function play(const aURL: string): boolean;
    function playCurrent: boolean;
    function playFirst: boolean;
    function playLast: boolean;
    function playNext: boolean;
    function playPrev: boolean;
    function releasePlayer: boolean;
    function resume: boolean;
    function rotateLeft: string;
    function rotateReset: string;
    function rotateRight: string;
    function saturationDn: string;
    function saturationReset: string;
    function saturationUp: string;
    function setProgressBar: boolean;
    function speedDn: string;
    function speedReset: string;
    function speedUp: string;
    function startOver: string;
    function stop: boolean;
    function tab(const aShiftState: TShiftState; const capsLock: boolean; const aFactor: integer = 0): string;
    function takeScreenshot: string;
    function toggleFullscreen: boolean;
    function toggleRepeat: string;
    function toggleSubtitles: string;
    function volDown: string;
    function volUp: string;
    function zoomIn: string;
    function zoomOut: string;
    function zoomReset: string;
    property alwaysPot:           boolean      read FAlwaysPot    write FAlwaysPot;
    property dontPlayNext:        boolean      read FDontPlayNext write FDontPlayNext;
    property duration:            integer      read getDuration;
    property formattedDuration:   string       read getFormattedDuration;
    property formattedSpeed:      string       read getFormattedSpeed;
    property formattedTime:       string       read getFormattedTime;
    property formattedVol:        string       read getFormattedVol;
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

function MP: TMediaPlayer;

implementation

uses
  vcl.controls, vcl.graphics, winAPI.windows, TGlobalVarsClass, formCaptions, TProgressBarClass, TKeyboardClass, TCommonUtilsClass, system.sysUtils,
  formCaption, TMediaInfoClass, mpvConst, TPlaylistClass, TUICtrlsClass, TSysCommandsClass, TConfigFileClass, formHelp, TSendAllClass, TMediaTypesClass, _debugWindow;

var
  gMP: TMediaPlayer;

function MP: TMediaPlayer;
begin
  case gMP = NIL of TRUE: gMP := TMediaPlayer.create; end;
  result := gMP;
end;

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
    UI.resetColor;
    result := 'All reset';
  end;

function TMediaPlayer.autoPlayNext: boolean;
begin
  case FImagePaused AND (FMediaType = mtImage) of TRUE: EXIT; end;
  playNext;
end;

function TMediaPlayer.brightnessDn: string;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness - 1);
  result := getFormattedBrightness;
end;

function TMediaPlayer.brightnessReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.SetPropertyInt64('brightness', 0);
  result := 'Brightness reset';
end;

function TMediaPlayer.brightnessUp: string;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness + 1);
  result := getFormattedBrightness;
end;

function TMediaPlayer.chapterNext: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr('add chapter 1');
end;

function TMediaPlayer.chapterPrev: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr('add chapter -1');
end;

function TMediaPlayer.checkPot: boolean;
begin
  case FAlwaysPot of TRUE: begin CU.delay(3000); MP.pause; UI.openExternalApp(F10_APP, PL.currentItem); end;end;
end;

function TMediaPlayer.contrastDn: string;
var
  contrast: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('contrast', contrast);
  mpv.SetPropertyInt64('contrast', contrast - 1);
  result := getFormattedContrast;
end;

function TMediaPlayer.contrastReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.SetPropertyInt64('contrast', 0);
  result := 'Contrast reset';
end;

function TMediaPlayer.contrastUp: string;
var
  contrast: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('contrast', contrast);
  mpv.SetPropertyInt64('contrast', contrast + 1);
  result := getFormattedContrast;
end;

constructor TMediaPlayer.create;
begin
  inherited;
  FTimer := TTimer.create(NIL);
  FTimer.enabled := FALSE;
  FTimer.OnTimer := onTimerEvent;
end;

function TMediaPlayer.cycleAudio: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr('cycle audio');
end;

function TMediaPlayer.cycleSubs: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr('cycle sub');
end;

destructor TMediaPlayer.Destroy;
begin
  case FTimer <> NIL of TRUE: FTimer.free; end;
  releasePlayer;
  inherited;
end;

function TMediaPlayer.frameBackwards: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr(CMD_BACK_STEP);
end;

function TMediaPlayer.frameForwards: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.commandStr(CMD_STEP);
end;

function TMediaPlayer.gammaDn: string;
var
  gamma: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('gamma', gamma);
  mpv.SetPropertyInt64('gamma', gamma - 1);
  result := getFormattedgamma;
end;

function TMediaPlayer.gammaReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.SetPropertyInt64('gamma', 0);
  result := 'Gamma reset';
end;

function TMediaPlayer.gammaUp: string;
var
  gamma: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('gamma', gamma);
  mpv.SetPropertyInt64('gamma', gamma + 1);
  result := getFormattedgamma;
end;

function TMediaPlayer.getDuration: integer;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := trunc(mpv.TotalSeconds);
end;

function TMediaPlayer.getFormattedBrightness: string;
var
  vBrightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', vBrightness);
  result := format('Brightness: %d', [vBrightness]);
end;

function TMediaPlayer.getFormattedContrast: string;
var
  vContrast: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('contrast', vContrast);
  result := format('Contrast: %d', [vContrast]);
end;

function TMediaPlayer.getFormattedDuration: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := CU.formatTime(trunc(mpv.totalSeconds));
end;

function TMediaPlayer.getFormattedGamma: string;
var
  vGamma: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('gamma', vGamma);
  result := format('Gamma: %d', [vGamma]);
end;

function TMediaPlayer.getFormattedSaturation: string;
var
  vSaturation: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('saturation', vSaturation);
  result := format('Saturation: %d', [vSaturation]);
end;

function TMediaPlayer.getFormattedSpeed: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Speed: %.2f', [mpv.PlaybackSpeed]);
end;

function TMediaPlayer.getFormattedTime: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := CU.formatTime(trunc(mpv.CurrentSeconds));
end;

function TMediaPlayer.getFormattedVol: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Volume: %d', [trunc(mpv.volume)]);
end;

function TMediaPlayer.getPosition: integer;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := trunc(mpv.CurrentSeconds);
end;

function TMediaPlayer.getVideoHeight: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.videoHeight;
end;

function TMediaPlayer.getVideoWidth: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.videoWidth;
end;

function TMediaPlayer.initMediaPlayer: boolean;
begin
  FVol := CF.asInteger['volume'];
end;

function TMediaPlayer.muteUnmute: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  FMuted    := NOT FMuted;
  mpv.mute  := FMuted;
  case mpv.mute of  TRUE: result := 'unmuted';
                   FALSE: result := 'muted'; end;
end;

procedure TMediaPlayer.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  with sender as TMPVBasePlayer do begin
    SetPropertyString('osc', 'no'); // On Screen Control
    SetPropertyString('force-window', 'yes');
    SetPropertyString('config-dir', CU.getExePath); // mpv.conf location
    SetPropertyString('config', 'yes');  // DISABLE USER ACCESS TO MPV.CONF? - NO!
    SetPropertyBool('keep-open', FALSE); // ensure libmpv MPV_EVENT_END_FILE_ event at the end of every media file
    SetPropertyBool('keep-open-pause', FALSE);

    setPropertyString('sub-font', 'Segoe UI');
    setPropertyString('sub-color', '#808080');
    setPropertyString('osd-color', '#808080');
    setPropertyString('osd-bold',  'yes');
    setPropertyString('osd-back-color', '#00000000');
    setPropertyString('osd-shadow-offset', '0');
    setPropertyString('screenshot-format', 'png');
    SetPropertyString('osd-font-size', '10');
    SetPropertyInt64('osd-duration', 3000);
    setPropertyString('osd-align-x', 'right');
    setPropertyString('osd-align-y', 'bottom');
    setPropertyString('osd-margin-x', '4');
    setPropertyString('osd-margin-y', '24');
    setPropertyString('screenshot-png-compression', '0');
    setPropertyString('screenshot-template', '%F %p %04n');
    setPropertyString('sid', '1');
    setPropertyString('image-display-duration', 'inf');
//    SetPropertyDouble('sub-delay', -00.99);
  end;
end;

procedure TMediaPlayer.onStateChange(cSender: TObject; eState: TMPVPlayerState);
begin
  FPlaying := eState = mpsPlay;

  case FImagePaused AND (FMediaType = mtImage) of TRUE: EXIT; end;

  case eState of
    mpsPlay: postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
    mpsEnd {, mpsStop}:  case FDontPlayNext of FALSE: begin
                                                        case FMediaType = mtImage of TRUE: CU.delay(trunc(FImageDisplayDurationSS) * 1000); end;
                                                        autoPlayNext;
                                                      end;end;
  end;
end;

procedure TMediaPlayer.onTimerEvent(sender: TObject);
begin
  FTimer.enabled := FALSE;
  case FTimerEvent of
    tePlay:  play(PL.currentItem);
    teClose: sendSysCommandClose(UI.handle);
  end;
end;

function TMediaPlayer.openURL(const aURL: string): boolean;
begin
  result := FALSE;
  // releasePlayer;
  case mpv = NIL of TRUE: begin
    mpv := TMPVBasePlayer.Create;
    mpv.OnStateChged := onStateChange;
    mpv.onInitMPV    := onInitMPV;
    mpv.initPlayer(intToStr(UI.handle), CU.getExePath, CU.getExePath, '');  // THIS RECREATES THE INTERNAL MPV OBJECT

    mpv.getPropertyString('image-display-duration', FImageDisplayDuration, FALSE);
    case tryStrToFloat(FImageDisplayDuration, FImageDisplayDurationSS) of FALSE: FImageDisplayDurationSS := 0; end;
    mpv.getPropertyString('screenshot-directory', FScreenshotDirectory, FALSE);
  end;end;

  mpv.openFile(aURL);

//  mpv.setPropertyString('start', '#9');

  result := TRUE;
//  ST.opInfo := format('%d x %d', [videoWidth, videoHeight]);
end;

function TMediaPlayer.panDn(const aShiftState: TShiftState): string;
var
  panY: double;
  multiplier: double;
begin
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in aShiftState of  TRUE: multiplier := 2;
                                 FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY + (0.001 * multiplier));
  result := 'Pan down';
end;

function TMediaPlayer.panLeft(const aShiftState: TShiftState): string;
var
  panX: double;
  multiplier: double;
begin
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in aShiftState of  TRUE: multiplier := 2;
                                 FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX - (0.001 * multiplier));
  result := 'Pan left';
end;

function TMediaPlayer.panReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  result := 'Pan reset';
end;

function TMediaPlayer.panRight(const aShiftState: TShiftState): string;
var
  panX: double;
  multiplier: double;
begin
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in aShiftState of  TRUE: multiplier := 2;
                                 FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX + (0.001 * multiplier));
  result := 'Pan right';
end;

function TMediaPlayer.panUp(const aShiftState: TShiftState): string;
var
  panY: double;
  multiplier: double;
begin
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in aShiftState of  TRUE: multiplier := 2;
                                 FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY - (0.001 * multiplier));
  result := 'Pan up';
end;

function TMediaPlayer.pause: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.pause;
end;

function TMediaPlayer.pauseUnpauseImages: boolean;
begin
  case FMediaType = mtImage of FALSE: EXIT; end;
  FImagePaused := NOT FImagePaused;

  case FImagePaused of  TRUE: begin mpv.setPropertyString('image-display-duration', 'inf'); end;              // prevent the [next] image from being closed when the duration has expired
                       FALSE: begin mpv.setPropertyString('image-display-duration', FImageDisplayDuration);   // restore the original setting
                                    case FImageDisplayDuration = 'inf' of FALSE: autoPlayNext; end;end;end;       // if there was a set duration before, continue with the next file

  case FImagePaused of  TRUE: ST.opInfo := 'slideshow paused';
                       FALSE: ST.opInfo := 'slideshow unpaused'; end;

  FDontPlayNext := FImagePaused;
end;

function TMediaPlayer.pausePlay: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  FPausePlay := TRUE;

  pauseUnpauseImages;

  case mpv.GetState of
    mpsPlay:  mpv.pause;
    mpsPause: mpv.Resume;
  end;
end;

function TMediaPlayer.play(const aURL: string): boolean;
begin
  result := FALSE;

  FPausePlay := FALSE;
  FRepeat    := FALSE;

  case assigned(FOnBeforeNew) of TRUE: FOnBeforeNew(SELF); end;

  MI.initMediaInfo(aURL);

  FMediaType := MT.mediaType(lowerCase(extractFileExt(PL.currentItem)));
  // reset the window size for an audio file in case the previous file was a video, or the previous audio had an image but this one doesn't
  case UI.autoCentre OR (FMediaType = mtAudio) of TRUE: UI.setWindowSize(FMediaType, MI.hasCoverArt); end;
  UI.centreCursor;

  openURL(aURL);
  mpv.volume := FVol;
  mpv.mute   := FMuted;

  FDontPlayNext := (FMediaType = mtImage) and (FImageDisplayDuration = 'inf');

  case ST.showData of TRUE: MI.getData(ST.dataMemo); end;
  MC.caption := PL.formattedItem;

  application.processMessages;

  SA.postToAll(WM_PROCESS_MESSAGES);

  checkPot;

  case assigned(FOnPlayNew) of  TRUE: FOnPlayNew(SELF); end;
  UI.centreCursor;

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
  pause;

  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.next;
  case FTimer.enabled of FALSE: begin
                                  FTimerEvent    := teClose;
                                  FTimer.enabled := TRUE; end;end;
  case assigned(FOnPlayNext) of TRUE: FOnPlayNext(SELF); end;
end;

function TMediaPlayer.playPrev: boolean;
begin
  pause;
  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.prev;
end;

function TMediaPlayer.resume: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.resume;
end;

function TMediaPlayer.rotateLeft: string;
var
  rot: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot - 45);
  result := 'Rotate left';
end;

function TMediaPlayer.rotateReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('video-rotate', 0);
  result := 'Rotate reset';
end;

function TMediaPlayer.rotateRight: string;
var
  rot: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot + 45);
  result := 'Rotate right';
end;

function TMediaPlayer.releasePlayer: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  freeAndNIL(mpv);
end;

function TMediaPlayer.saturationDn: string;
var
  saturation: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('saturation', saturation);
  mpv.SetPropertyInt64('saturation', saturation - 1);
  result := getFormattedsaturation;
end;

function TMediaPlayer.saturationReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.SetPropertyInt64('saturation', 0);
  result := 'Saturation reset';
end;

function TMediaPlayer.saturationUp: string;
var
  saturation: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('saturation', saturation);
  mpv.SetPropertyInt64('saturation', saturation + 1);
  result := getFormattedsaturation;
end;

procedure TMediaPlayer.setKeepOpen(const Value: boolean);
begin
  mpv.SetPropertyBool('keep-open', Value); // ensure libmpv MPV_EVENT_END_FILE_ event at the end of every media file
end;

procedure TMediaPlayer.setPosition(const Value: integer);
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.Seek(value, FALSE);
  postMessage(GV.appWnd, WM_TICK, 0, 0); // immediately update the time
end;

function TMediaPlayer.setProgressBar: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  case PB.max <> trunc(mpv.totalSeconds) of TRUE: PB.max := trunc(mpv.totalSeconds); end;
  case mpv.totalSeconds > 0 of TRUE: PB.position := trunc(mpv.currentSeconds); end;

  case assigned(FOnPosition) of TRUE: FOnPosition(trunc(mpv.totalSeconds), trunc(mpv.currentSeconds)); end;
end;

function TMediaPlayer.speedDn: string;
var
  speed: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed - 0.01);
  result := formattedSpeed;
end;

function TMediaPlayer.speedReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('speed', 1.00);
  CU.delay(100);
  result := 'Speed reset';
end;

function TMediaPlayer.speedUp: string;
var
  speed: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed + 0.01);
  result := formattedSpeed;
end;

function TMediaPlayer.startOver: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.Seek(0, FALSE);
  result := 'Start over';
end;

function TMediaPlayer.stop: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.Stop;
end;

function TMediaPlayer.tab(const aShiftState: TShiftState; const capsLock: boolean; const aFactor: integer = 0): string;
var
  vFactor: integer;
  vTab: integer;
  newInfo: string;
begin
  case aFactor > 0 of  TRUE: vFactor := aFactor;
                      FALSE: vFactor := 100; end;

  case capsLock of TRUE: vFactor := 200; end; // alt-key does the same as it can be a pain having the CapsLock key on all the time
  case ssShift in aShiftState of TRUE: vFactor := 50; end;

  vTab := trunc(duration / vFactor);
  case (vTab = 0) or (aFactor = -1) of TRUE: vTab := 1; end;

  case ssCtrl  in aShiftState of  TRUE: position := position - vTab;
                                 FALSE: position := position + vTab; end;

  case aFactor = -1 of  TRUE: newInfo := 'TAB = 1s';
                       FALSE: newInfo := format('%dth = %s', [vFactor, CU.formatSeconds(round(duration / vFactor))]); end;

  case ssCtrl in aShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                FALSE: newInfo := '>> ' + newInfo;
  end;
  result := newInfo;
end;

function TMediaPlayer.takeScreenshot: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  case FScreenshotDirectory = '' of TRUE: mpv.setPropertyString('screenshot-directory', PL.currentFolder); end; // otherwise screenshots of an image go to Windows/System32 !!
  mpv.commandStr(CMD_SCREEN_SHOT + ' window');
  result := '';
end;

function TMediaPlayer.toggleFullscreen: boolean;
begin
  UI.toggleMaximized;
  postMessage(GV.appWnd, WM_TICK, 0, 0);
end;

function TMediaPlayer.toggleRepeat: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  FRepeat := NOT FRepeat;
  case FRepeat of  TRUE: mpv.setPropertyString('loop-file', 'yes');
                  FALSE: mpv.setPropertyString('loop-file', 'no'); end;
  case FRepeat of  TRUE: result := 'repeat on';
                  FALSE: result := 'repeat off'; end;
end;

function TMediaPlayer.toggleSubtitles: string;
var vSid: string;
begin
  mpv.GetPropertyString('sid', vSid);
  case vSid = 'no' of  TRUE: mpv.setPropertyString('sid', '1');
                      FALSE: mpv.setPropertyString('sid', 'no'); end;
  mpv.GetPropertyString('sid', vSid);
  case vSid = 'no' of  TRUE: result := 'subtitles off';
                      FALSE: result := 'subtitles on'; end;
end;

function TMediaPlayer.volDown: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume - 1;
  FVol       := mpv.volume;
  CF.value['volume'] := intToStr(trunc(mpv.volume));
  result := formattedVol;
end;

function TMediaPlayer.volUp: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume + 1;
  FVol       := mpv.volume;
  CF.value['volume'] := intToStr(trunc(mpv.volume));
  result := formattedVol;
end;

function TMediaPlayer.zoomIn: string;
var
  zoomX, zoomY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX + 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY + 0.01);
  result := 'Zoom in';
end;

function TMediaPlayer.zoomOut: string;
var
  zoomX, zoomY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX - 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY - 0.01);
  result := 'Zoom out';
end;

function TMediaPlayer.zoomReset: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  mpv.setPropertyDouble('video-scale-x', 1.00);
  mpv.setPropertyDouble('video-scale-y', 1.00);
  result := 'Zoom reset';
end;

initialization
  gMP := NIL;

finalization
  case gMP <> NIL of TRUE: gMP.free; end;

end.
