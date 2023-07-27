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
unit mediaPlayer;

interface

uses
  forms, vcl.extCtrls, system.classes, MPVBasePlayer;

type
  TTimerEvent = (tePlay, teClose);

  TMediaPlayer = class(TObject)
  strict private
    mpv: TMPVBasePlayer;
    FTimer: TTimer;
    FTimerEvent: TTimerEvent;

    FForm: TForm;
    FVol:  double;
    FX:    int64;
    FY:    int64;
  private
    constructor create;
    procedure onTimerEvent(sender: TObject);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);

    function checkPot(aAlwaysPot: boolean): boolean;
    function getFormattedBrightness: string;
    function getFormattedSpeed: string;
    function getFormattedVol: string;

    {property setters}
    function  getDuration: integer;
    function  getPosition: integer;
    function  getSubTitle: string;
    procedure setPosition(const Value: integer);
    function  getFormattedDuration: string;
    function  getFormattedTime: string;
    function  getVideoHeight: int64;
    function  getVideoWidth: int64;
    function  getVolume: integer;
    procedure setVolume(const Value: integer);
  public
    destructor  Destroy; override;
    function brightnessDn: boolean;
    function brightnessReset: boolean;
    function brightnessUp: boolean;
    function frameBackwards: boolean;
    function frameForwards: boolean;
    function initMediaPlayer(aForm: TForm): boolean;
    function muteUnmute: boolean;
    function openURL(aURL: string): boolean;
    function panDn: boolean;
    function panLeft: boolean;
    function panReset: boolean;
    function panRight: boolean;
    function panUp: boolean;
    function pause: boolean;
    function pausePlay: boolean;
    function play(aURL: string): boolean;
    function playFirst: boolean;
    function playLast: boolean;
    function playNext: boolean;
    function playPrev: boolean;
    function releasePlayer: boolean;
    function resume: boolean;
    function rotateLeft: boolean;
    function rotateReset: boolean;
    function rotateRight: boolean;
    function setProgressBar: boolean;
    function speedDn: boolean;
    function speedReset: boolean;
    function speedUp: boolean;
    function startOver: boolean;
    function stop: boolean;
    function tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
    function toggleFullscreen: boolean;
    function volDown: boolean;
    function volUp: boolean;
    function zoomEnd: boolean;
    function zoomIn: boolean;
    function zoomOut: boolean;
    property duration:            integer   read getDuration;
    property formattedDuration:   string    read getFormattedDuration;
    property formattedSpeed:      string    read getFormattedSpeed;
    property formattedTime:       string    read getFormattedTime;
    property formattedVol:        string    read getFormattedVol;
    property position:            integer   read getPosition  write setPosition;
    property subTitle:            string    read getSubTitle;
    property videoHeight:         int64     read getVideoHeight;
    property videoWidth:          int64     read getVideoWidth;
    property volume:              integer   read getVolume    write setVolume;
  end;

function MP: TMediaPlayer;

implementation

uses
  vcl.controls, vcl.graphics, winAPI.windows, globalVars, formSubtitles, progressBar, keyboard, commonUtils, system.sysUtils,
  formCaption, mediaInfo, mpvConst, consts, playlist, UIctrls, sysCommands, _debugWindow;

var
  gMP: TMediaPlayer;

function MP: TMediaPlayer;
begin
  case gMP = NIL of TRUE: gMP := TMediaPlayer.create; end;
  result := gMP;
end;

{ TMediaPlayer }

function TMediaPlayer.brightnessDn: boolean;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness - 1);
  ST.opInfo := getFormattedBrightness;
end;

function TMediaPlayer.brightnessReset: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.SetPropertyInt64('brightness', 0);
  ST.opInfo := 'Brightness reset';
end;

function TMediaPlayer.brightnessUp: boolean;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness + 1);
  ST.opInfo := getFormattedBrightness;
end;

function TMediaPlayer.checkPot(aAlwaysPot: boolean): boolean;
begin
  case aAlwaysPot of TRUE: begin CU.delay(3000); MP.pause; UI.openExternalApp(POT_PLAYER, PL.currentItem); end;end;
end;

constructor TMediaPlayer.create;
begin
  inherited;
  FTimer := TTimer.create(NIL);
  FTimer.enabled := FALSE;
  FTimer.OnTimer := onTimerEvent;
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

function TMediaPlayer.getFormattedDuration: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := CU.formatTime(trunc(mpv.totalSeconds));
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

function TMediaPlayer.getSubTitle: string;
begin
//  result := MMFMediaEngine.pr_TimedTextNotify.SubTitle;
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

function TMediaPlayer.getVolume: integer;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := trunc(mpv.volume);
end;

function TMediaPlayer.initMediaPlayer(aForm: TForm): boolean;
begin
end;

function TMediaPlayer.muteUnmute: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.mute := not mpv.mute;
  case mpv.mute of  TRUE: ST.opInfo := 'unmuted';
                   FALSE: ST.opInfo := 'muted'; end;
end;

procedure TMediaPlayer.onTimerEvent(sender: TObject);
begin
  FTimer.enabled := FALSE;
  case FTimerEvent of
    tePlay:  play(PL.currentItem);
    teClose: sendSysCommandClose(UI.handle);
  end;
end;

procedure TMediaPlayer.onStateChange(cSender: TObject; eState: TMPVPlayerState);
begin
  case eState of
    mpsPlay: postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
    mpsEnd: playNext;
  end;
end;

function TMediaPlayer.openURL(aURL: string): boolean;
var
  hr: HRESULT;
begin
  result := FALSE;
  releasePlayer;
  case mpv = NIL of TRUE: begin
    mpv := TMPVBasePlayer.Create;
    mpv.OnStateChged := onStateChange;
    mpv.initPlayer(intToStr(UI.handle), CU.getExePath, CU.getExePath, '');  // THIS RECREATES THE INTERNAL MPV OBJECT
    mpv.setPropertyString('sub-font', 'Segoe UI');
    mpv.setPropertyString('sub-color', '#808080');
  end;end;
  mpv.openFile(aURL);
  result := TRUE;
end;

function TMediaPlayer.panDn: boolean;
var
  panY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY + 0.001);
  ST.opInfo := 'Pan down';
end;

function TMediaPlayer.panLeft: boolean;
var
  panX: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX - 0.001);
  ST.opInfo := 'Pan left';
end;

function TMediaPlayer.panReset: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  ST.opInfo := 'Pan reset';
end;

function TMediaPlayer.panRight: boolean;
var
  panX: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX + 0.001);
  ST.opInfo := 'Pan right';
end;

function TMediaPlayer.panUp: boolean;
var
  panY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY - 0.001);
  ST.opInfo := 'Pan up';
end;

function TMediaPlayer.pause: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.pause;
end;

function TMediaPlayer.pausePlay: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  case mpv.GetState of
    mpsPlay:  mpv.pause;
    mpsPause: mpv.Resume;
  end;
end;

function TMediaPlayer.play(aURL: string): boolean;
begin
  result := FALSE;
  openURL(aURL);
  mpv.volume := FVol;
  MI.URL     := aURL;
  case ST.showData of TRUE: MI.getData(ST.dataMemo); end;
  MC.caption := PL.formattedItem;
  postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
  application.processMessages;
  checkPot(GV.alwaysPot);
  result := TRUE;
end;

function TMediaPlayer.playFirst: boolean;
begin
  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.first;
end;

function TMediaPlayer.playLast: boolean;
begin
  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.last;
end;

function TMediaPlayer.playNext: boolean;
begin
  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.next;
  case FTimer.enabled of FALSE: begin
                                  FTimerEvent    := teClose;
                                  FTimer.enabled := TRUE; end;end;
end;

function TMediaPlayer.playPrev: boolean;
begin
  FTimer.interval := 100;
  FTimerEvent     := tePlay;
  FTimer.enabled  := PL.prev;
end;

function TMediaPlayer.resume: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.resume;
end;

function TMediaPlayer.rotateLeft: boolean;
var
  rot: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot - 45);
  ST.opInfo := 'Rotate left';
end;

function TMediaPlayer.rotateReset: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('video-rotate', 0);
  ST.opInfo := 'Rotate reset';
end;

function TMediaPlayer.rotateRight: boolean;
var
  rot: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot + 45);
  ST.opInfo := 'Rotate right';
end;

function TMediaPlayer.releasePlayer: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  freeAndNIL(mpv);
end;

procedure TMediaPlayer.setPosition(const Value: integer);
var
  hr: HRESULT;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.Seek(value, FALSE);
  postMessage(GV.appWnd, WM_TICK, 0, 0); // immediately update the time
end;

function TMediaPlayer.setProgressBar: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  case PB.max <> trunc(mpv.totalSeconds) of TRUE: PB.max := trunc(mpv.TotalSeconds); end;
  case mpv.TotalSeconds > 0 of TRUE: PB.position := trunc(mpv.CurrentSeconds); end;
end;

procedure TMediaPlayer.setVolume(const Value: integer);
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := value;
  FVol       := mpv.volume;
  ST.opInfo  := formattedVol;
end;

function TMediaPlayer.speedDn: boolean;
var
  speed: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed - 0.01);
  ST.opInfo := formattedSpeed;
end;

function TMediaPlayer.speedReset: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('speed', 1.00);
  CU.delay(100);
  ST.opInfo := formattedSpeed;
end;

function TMediaPlayer.speedUp: boolean;
var
  speed: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed + 0.01);
  ST.opInfo := formattedSpeed;
end;

function TMediaPlayer.startOver: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  play(PL.currentItem);
  //mpv.Seek(0, FALSE);
  ST.opInfo := 'Start over';
end;

function TMediaPlayer.stop: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.Stop;
end;

function TMediaPlayer.tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
var
  vFactor: integer;
  vTab: integer;
  newInfo: string;
begin
  vFactor := 100;

  case capsLock               of TRUE: vFactor := 200; end;
//  case ssShift in aShiftState of TRUE: vFactor := 20; end; // might remove shift-t in favor of the help window
  case ssAlt   in aShiftState of TRUE: vFactor := 10; end;
  vTab := trunc(duration / vFactor);
  case (vTab = 0) or (aFactor = -1) of TRUE: vTab := 1; end;

  case ssCtrl  in aShiftState of  TRUE: position := position - vTab;
                                 FALSE: position := position + vTab; end;

  case aFactor = -1 of  TRUE: newInfo := 'TAB = 1s';
                       FALSE: newInfo := format('%dth = %s', [vFactor, CU.formatSeconds(round(duration / vFactor))]); end;

  case ssCtrl in aShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                FALSE: newInfo := '>> ' + newInfo;
  end;
  ST.opInfo := newInfo;
end;

function TMediaPlayer.toggleFullscreen: boolean;
begin
  UI.toggleMaximized;
  postMessage(GV.appWnd, WM_TICK, 0, 0);
end;

function TMediaPlayer.volDown: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume - 1;
  FVol       := mpv.volume;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.volUp: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume + 1;
  FVol       := mpv.volume;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.zoomEnd: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  mpv.setPropertyDouble('video-scale-x', 1.00);
  mpv.setPropertyDouble('video-scale-y', 1.00);
  ST.opInfo := 'Zoom reset';
end;

function TMediaPlayer.zoomIn: boolean;
var
  zoomX, zoomY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX + 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY + 0.01);
  ST.opInfo := 'Zoom in';
end;

function TMediaPlayer.zoomOut: boolean;
var
  zoomX, zoomY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX - 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY - 0.01);
  ST.opInfo := 'Zoom out';
end;

initialization
  gMP := NIL;

finalization
  case gMP <> NIL of TRUE: gMP.free; end;

end.
