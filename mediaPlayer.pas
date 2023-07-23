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
  TTimerEvent = (tePlay, teAspectRatio);

  TMediaPlayer = class(TObject)
  strict private
    mpv: TMPVBasePlayer;
    FTimer: TTimer;
    FTimerEvent: TTimerEvent;

    FForm: TForm;
    FX:    int64;
    FY:    int64;
  private
    constructor create;
    procedure onTimerEvent(sender: TObject);
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
    function  getFormattedSpeed: string;
  public
    destructor  Destroy; override;
//    function adjustAspectRatio(aForm: TForm; X: int64; Y: int64): boolean;
    function adjustAspectRatioDelayed(aForm: TForm; X: int64; Y: int64): boolean;
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
    function play(aURL: string): boolean;
    function playFirst: boolean;
    function playLast: boolean;
    function playNext: boolean;
    function playPrev: boolean;
    function releasePlayer: boolean;
    function resume: boolean;
    function rotateLeft: boolean;
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


function TMediaPlayer.adjustAspectRatioDelayed(aForm: TForm; X, Y: int64): boolean;
begin
  FForm := aForm; FX := X; FY := Y;

  FTimerEvent := teAspectRatio;
  FTimer.interval := 500;
  FTimer.enabled := TRUE;
end;

function TMediaPlayer.brightnessDn: boolean;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness - 1);
end;

function TMediaPlayer.brightnessUp: boolean;
var
  brightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.SetPropertyInt64('brightness', brightness + 1);
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

function TMediaPlayer.getFormattedDuration: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := formatTime(trunc(mpv.totalSeconds));
end;

function TMediaPlayer.getFormattedSpeed: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Speed: %.2f', [mpv.PlaybackSpeed]);
end;

function TMediaPlayer.getFormattedTime: string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := formatTime(trunc(mpv.CurrentSeconds));
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
  case mpv.mute of  TRUE: ST.opInfo := 'muted';
                   FALSE: ST.opInfo := 'unmuted'; end;
end;

procedure TMediaPlayer.onTimerEvent(sender: TObject);
begin
  FTimer.enabled := FALSE;
  case FTimerEvent of
    tePlay: play(PL.currentItem);
    teAspectRatio: postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0); //        adjustAspectRatio(FForm, FX, FY);
  end;
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
//    mpv.initPlayer(intToStr(UI.videoPanel.handle), '', '', '');
    mpv.setPropertyString('sub-font', 'Segoe UI');
    mpv.setPropertyString('sub-color', '#008000');
//    mpv.setPropertyString('osd-bar', 'yes');
//    mpv.setPropertyString('osd-level', '0');
//    mpv.setPropertyString('hr-seek', 'yes');
//    mpv.setPropertyString('osd-duration', '86400000');
//    mpv.setPropertyString('osc', 'yes'); // On Screen Control
//    mpv.setPropertyString('osc-layout', 'bottombar');
//    mpv.setPropertyString('osc-seekbarstyle', 'bar');
//    mpv.setPropertyInt64('osc-deadzonesize', 0);
//    mpv.setPropertyInt64('osc-minmousemove', 3);
//    mpv.setPropertyInt64('osc-hidetimeout', 1000);
//    mpv.setPropertyString('osc-vidscale', 'no');
//    mpv.setPropertyString('--no-osd-bar', '');
    mpv.initPlayer(intToStr(UI.mainForm.handle), ''{getExePath}, ''{getExePath}, '');
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
end;

function TMediaPlayer.panLeft: boolean;
var
  panX: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX - 0.001);
end;

function TMediaPlayer.panRight: boolean;
var
  panX: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX + 0.001);
end;

function TMediaPlayer.panUp: boolean;
var
  panY: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY - 0.001);
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
    mpsPlay: mpv.pause;
    mpsPause: mpv.Resume;
  end;
end;

function TMediaPlayer.play(aURL: string): boolean;
begin
  result := FALSE;
  openURL(aURL);
  MI.initMediaInfo(PL.currentItem);
  case ST.showData of TRUE: MI.getData(ST.dataMemo); end;
  MC.caption := PL.formattedItem;
  postMessage(GV.appWnd, WM_CENTRE_WINDOW, 0, 0);
//  postMessage(GV.appWnd, WM_ADJUST_ASPECT_RATIO, 0, 0);
  result := TRUE;
//  delay(1300);
//  mpv.command([CMD_OSD_OVERLAY, '0', '0', 'top-left', 'text', 'Hello, Delphi!']);
//  mpv.CommandStr('show-text "${osd-width}x${osd-height}"');
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
  FTimer.enabled   := PL.next;
  case PL.isLast of TRUE: sendSysCommandClose(UI.mainForm.handle); end;
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
end;

function TMediaPlayer.rotateRight: boolean;
var
  rot: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot + 45);
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
  ST.opInfo := formattedVol;
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
  delay(100);
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
  mpv.Seek(0, FALSE);
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
begin
  case aFactor <> 0 of  TRUE: vFactor := aFactor;
                       FALSE: vFactor := 100; end;  // default

//  case capsLock               of TRUE: vFactor := 10; end;
  case ssShift in aShiftState of TRUE: vFactor := 20; end;
  case ssAlt   in aShiftState of TRUE: vFactor := 50; end;
  vTab := trunc(duration / vFactor);

  case ssCtrl  in aShiftState of  TRUE: position := position - vTab;
                                 FALSE: position := position + vTab; end;

  var newInfo := format('%dth = %s', [vFactor, formatSeconds(round(duration / vFactor))]);
  case ssCtrl in aShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                FALSE: newInfo := '>> ' + newInfo;
  end;
  ST.opInfo := newInfo;
end;

function TMediaPlayer.toggleFullscreen: boolean;
begin
  UI.toggleMaximised;
  postMessage(GV.appWnd, WM_TICK, 0, 0);
end;

function TMediaPlayer.volDown: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume - 1;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.volUp: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := mpv.volume + 1;
  ST.opInfo := formattedVol;
end;

function TMediaPlayer.zoomEnd: boolean;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  mpv.setPropertyDouble('video-scale-x', 1.00);
  mpv.setPropertyDouble('video-scale-y', 1.00);
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
end;

initialization
  gMP := NIL;

finalization
  case gMP <> NIL of TRUE: gMP.free; end;

end.
