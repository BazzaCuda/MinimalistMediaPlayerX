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
  TPlayState  = (psPaused, psPlaying);

  TMediaPlayer = class(TObject)
  strict private
    mpv: TMPVBasePlayer;
    FPlayState: TPlayState;
    FPlayTimer: TTimer;
  private
    constructor create;
    procedure onPlayTimerEvent(sender: TObject);
    procedure onStateChange(cSender: TObject; eState: TMPVPlayerState);

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
    function  getFormattedVol: string;
  public
    destructor  Destroy; override;
    function adjustAspectRatio(aForm: TForm; X: int64; Y: int64): boolean;
    function brightnessUp: boolean;
    function brightnessDn: boolean;
    function frameBackwards: boolean;
    function frameForwards: boolean;
    function initMediaPlayer(aForm: TForm): boolean;
    function muteUnmute: boolean;
    function openURL(aURL: string): boolean;
    function panDn: boolean;
    function panLeft: boolean;
    function panRight: boolean;
    function panUp: boolean;
    function pause: boolean;
    function pausePlay: boolean;
    function play: boolean;
    function playNext: boolean;
    function playPrev: boolean;
    function releasePlayer: boolean;
    function rotateLeft: boolean;
    function rotateRight: boolean;
    function setProgressBar: boolean;
    function startOver: boolean;
    function stop: boolean;
    function tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
    function toggleFullscreen: boolean;
    function volDown: boolean;
    function volUp: boolean;
    function zoomIn: boolean;
    function zoomOut: boolean;
    property duration:            integer   read getDuration;
    property formattedDuration:   string    read getFormattedDuration;
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
  formCaption, mediaInfo, mpvConst, consts, playlist, UIctrls, _debugWindow;

var
  gMP: TMediaPlayer;

function MP: TMediaPlayer;
begin
  case gMP = NIL of TRUE: gMP := TMediaPlayer.create; end;
  result := gMP;
end;

{ TMediaPlayer }

function TMediaPlayer.adjustAspectRatio(aForm: TForm; X: int64; Y: int64): boolean;
var
  vRatio: double;
begin
  debugFormat('x:%d y:%d', [X, Y]);
  case (X = 0) OR (Y = 0) of TRUE: EXIT; end;

//  vRatio := MI.Y / MI.X;
  vRatio := Y / X;

  aForm.height := trunc(aForm.width * vRatio) + 2;

//  debugFormat('vRatio: %f', [vRatio]);

  // checkScreenLimits
end;

function TMediaPlayer.brightnessDn: boolean;
var
  brightness: int64;
begin
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness - 1);
end;

function TMediaPlayer.brightnessUp: boolean;
var
  brightness: int64;
begin
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness + 1);
end;

constructor TMediaPlayer.create;
begin
  inherited;
  FPlayTimer := TTimer.create(NIL);
  FPlayTimer.enabled := FALSE;
  FPlayTimer.interval := 100;
  FPlayTimer.OnTimer := onPlayTimerEvent;
end;

destructor TMediaPlayer.Destroy;
begin
  case FPlayTimer <> NIL of TRUE: FPlayTimer.free; end;
  releasePlayer;
  inherited;
end;

function TMediaPlayer.frameBackwards: boolean;
begin
  mpv.commandStr(CMD_BACK_STEP);
end;

function TMediaPlayer.frameForwards: boolean;
begin
  mpv.commandStr(CMD_STEP);
end;

function TMediaPlayer.getDuration: integer;
begin
  result := trunc(mpv.TotalSeconds);
end;

function TMediaPlayer.getFormattedDuration: string;
begin
  result := formatTime(trunc(mpv.totalSeconds));
end;

function TMediaPlayer.getFormattedTime: string;
begin
  result := formatTime(trunc(mpv.CurrentSeconds));
end;

function TMediaPlayer.getFormattedVol: string;
begin
  result := format('Volume: %d', [trunc(mpv.volume)]);
end;

function TMediaPlayer.getPosition: integer;
begin
  result := trunc(mpv.CurrentSeconds);
end;

function TMediaPlayer.getSubTitle: string;
begin
//  result := MMFMediaEngine.pr_TimedTextNotify.SubTitle;
end;

function TMediaPlayer.getVideoHeight: int64;
begin
  result := mpv.videoHeight;
end;

function TMediaPlayer.getVideoWidth: int64;
begin
  result := mpv.videoWidth;
end;

function TMediaPlayer.getVolume: integer;
begin
  result := trunc(mpv.volume);
end;

function TMediaPlayer.initMediaPlayer(aForm: TForm): boolean;
begin
end;

function TMediaPlayer.muteUnmute: boolean;
begin
  mpv.mute := not mpv.mute;
  case mpv.mute of  TRUE: ST.opInfo := 'muted';
                   FALSE: ST.opInfo := 'unmuted'; end;
end;

procedure TMediaPlayer.onPlayTimerEvent(sender: TObject);
begin
  FPlayTimer.enabled := FALSE;
  openURL(PL.currentItem);
end;

procedure TMediaPlayer.onStateChange(cSender: TObject; eState: TMPVPlayerState);
begin
  case eState of
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
//    mpv.initPlayer(intToStr(FVideoPanel.Handle), '', '', '');
    mpv.initPlayer(intToStr(GV.mainForm.handle), '', '', '');
    mpv.setPropertyString('sub-font', 'Segoe UI');
    mpv.setPropertyString('sub-color', '#008000');
//    mpv.setPropertyString('autofit', format('W%dxH%d', [GV.mainForm.width, GV.mainForm.height]));
//    mpv.SetPropertyBool(STR_AUTOFIT, TRUE);
  end;end;
//  mpv.setPropertyString('--drm-draw-surface-size', format('%dx%d', [UI.mainForm.width, UI.mainForm.height - 50]));
//  mpv.setPropertyString('--geometry', format('100%:100-100-100%', [UI.mainForm.width, UI.mainForm.height - 50]));
  mpv.openFile(aURL);
  MC.caption := PL.formattedItem;
  postMessage(GV.mainWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
  result := TRUE;
end;

function TMediaPlayer.panDn: boolean;
var
  panY: double;
begin
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY + 0.001);
end;

function TMediaPlayer.panLeft: boolean;
var
  panX: double;
begin
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX - 0.001);
end;

function TMediaPlayer.panRight: boolean;
var
  panX: double;
begin
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX + 0.001);
end;

function TMediaPlayer.panUp: boolean;
var
  panY: double;
begin
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY - 0.001);
end;

function TMediaPlayer.pause: boolean;
begin
  mpv.Pause;
end;

function TMediaPlayer.pausePlay: boolean;
begin
  case mpv.GetState of
    mpsPlay: mpv.pause;
    mpsPause: mpv.Resume;
  end;
end;

function TMediaPlayer.play: boolean;
begin
  result := TRUE;
  case result of TRUE: FPlayState := psPlaying; end;
end;

function TMediaPlayer.playNext: boolean;
begin
  case PL.next of TRUE: FPlayTimer.enabled := TRUE; end;
end;

function TMediaPlayer.playPrev: boolean;
begin
  case PL.prev of TRUE: FPlayTimer.enabled := TRUE; end;
end;

function TMediaPlayer.rotateLeft: boolean;
var
  rot: int64;
begin
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot - 45);
end;

function TMediaPlayer.rotateRight: boolean;
var
  rot: int64;
begin
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot + 45);
end;

function TMediaPlayer.releasePlayer: boolean;
begin
  freeAndNIL(mpv);
end;

procedure TMediaPlayer.setPosition(const Value: integer);
var
  hr: HRESULT;
begin
  mpv.Seek(value, FALSE);
  postMessage(GV.mainWnd, WM_TICK, 0, 0); // immediately update the time
end;

function TMediaPlayer.setProgressBar: boolean;
begin
  case assigned(mpv) of FALSE: EXIT; end;
  PB.max := trunc(mpv.TotalSeconds);
  case mpv.TotalSeconds > 0 of TRUE: PB.position := trunc(mpv.CurrentSeconds); end;
end;

procedure TMediaPlayer.setVolume(const Value: integer);
begin
  mpv.volume := value;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.startOver: boolean;
begin
  mpv.CommandStr(CMD_VIDEO_RELOAD);
end;

function TMediaPlayer.stop: boolean;
begin
  mpv.Stop;
end;

function TMediaPlayer.tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
var
  vFactor: integer;
  vTab: integer;
begin
  case aFactor <> 0 of  TRUE: vFactor := aFactor;
                       FALSE: vFactor := 100; end;

  case capsLock               of TRUE: vFactor := 10; end;
  case ssShift in aShiftState of TRUE: vFactor := 20; end;
  case ssAlt   in aShiftState of TRUE: vFactor := 50; end;
  vTab := trunc(duration / vFactor);

//  pause; delay(100);
  case ssCtrl  in aShiftState of  TRUE: position := position - vTab;
                                 FALSE: position := position + vTab; end;
//  delay(100); play;

  var newInfo := format('%dth = %s', [vFactor, formatSeconds(round(duration / vFactor))]);
  case ssCtrl in aShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                FALSE: newInfo := '>> ' + newInfo;
  end;
  ST.opInfo := newInfo;
//  mpv.Resume;
end;

function TMediaPlayer.toggleFullscreen: boolean;  // don't think this should be here, should be calling a function in mainForm ?
begin
  case GV.mainForm.WindowState <> wsMaximized of  TRUE: GV.mainForm.windowState := wsMaximized;
                                                 FALSE: GV.mainForm.windowState := TWindowState.wsNormal; end;
end;

function TMediaPlayer.volDown: boolean;
begin
  mpv.volume := mpv.volume - 1;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.volUp: boolean;
begin
  mpv.volume := mpv.volume + 1;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.zoomIn: boolean;
var
  zoomX, zoomY: double;
begin
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX + 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY + 0.01);
end;

function TMediaPlayer.zoomOut: boolean;
var
  zoomX, zoomY: double;
begin
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX - 0.01);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY - 0.01);
end;

initialization
  gMP := NIL;

finalization
  case gMP <> NIL of TRUE: gMP.free; end;

end.
