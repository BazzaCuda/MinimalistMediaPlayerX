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
unit model.mmpMPVCtrls;

interface

uses
  winApi.windows,
  system.classes,
  MPVBasePlayer, MPVConst,
  mmpConsts,
  model.mmpMPVFormatting;

function mpvCreate    (var   mpv: IMPVBasePlayer): boolean;
function mpvInitPlayer(const mpv: IMPVBasePlayer; const sWinHandle: HWND; const sScrShotDir: string; const sConfigDir: string; const sLogFile: string = EMPTY; fEventWait: double = 0.5): TMPVErrorCode;
function mpvOpenFile  (const mpv: IMPVBasePlayer; aURL: string): TMPVErrorCode;

function mpvBrightnessDn    (const mpv: IMPVBasePlayer): string;
function mpvBrightnessReset (const mpv: IMPVBasePlayer): string;
function mpvBrightnessUp    (const mpv: IMPVBasePlayer): string;
function mpvChapterNext     (const mpv: IMPVBasePlayer): boolean;
function mpvChapterPrev     (const mpv: IMPVBasePlayer): boolean;
function mpvContrastDn      (const mpv: IMPVBasePlayer): string;
function mpvContrastReset   (const mpv: IMPVBasePlayer): string;
function mpvContrastUp      (const mpv: IMPVBasePlayer): string;
function mpvCycleAudio      (const mpv: IMPVBasePlayer): boolean;
function mpvCycleSubs       (const mpv: IMPVBasePlayer): boolean;
function mpvFrameBackwards  (const mpv: IMPVBasePlayer): boolean;
function mpvFrameForwards   (const mpv: IMPVBasePlayer): boolean;
function mpvGammaDn         (const mpv: IMPVBasePlayer): string;
function mpvGammaReset      (const mpv: IMPVBasePlayer): string;
function mpvGammaUp         (const mpv: IMPVBasePlayer): string;
function mpvMute            (const mpv: IMPVBasePlayer; const aValue: boolean): string;
function mpvMuteUnmute      (const mpv: IMPVBasePlayer): string;
function mpvPanDn           (const mpv: IMPVBasePlayer): string;
function mpvPanLeft         (const mpv: IMPVBasePlayer): string;
function mpvPanReset        (const mpv: IMPVBasePlayer): string;
function mpvPanRight        (const mpv: IMPVBasePlayer): string;
function mpvPanUp           (const mpv: IMPVBasePlayer): string;
function mpvPause           (const mpv: IMPVBasePlayer): boolean;
function mpvPausePlay       (const mpv: IMPVBasePlayer): string;
function mpvResetAll        (const mpv: IMPVBasePlayer): string;
function mpvResume          (const mpv: IMPVBasePlayer): boolean;
function mpvRotateLeft      (const mpv: IMPVBasePlayer): string;
function mpvRotateReset     (const mpv: IMPVBasePlayer): string;
function mpvRotateRight     (const mpv: IMPVBasePlayer): string;
function mpvSaturationDn    (const mpv: IMPVBasePlayer): string;
function mpvSaturationReset (const mpv: IMPVBasePlayer): string;
function mpvSaturationUp    (const mpv: IMPVBasePlayer): string;
function mpvSeek            (const mpv: IMPVBasePlayer; const aValue: integer): boolean;
function mpvSpeedDn         (const mpv: IMPVBasePlayer): string;
function mpvSpeedReset      (const mpv: IMPVBasePlayer): string;
function mpvSpeedUp         (const mpv: IMPVBasePlayer): string;
function mpvStartOver       (const mpv: IMPVBasePlayer): string;
function mpvStop            (const mpv: IMPVBasePlayer): boolean;
function mpvSyncAudioDn     (const mpv: IMPVBasePlayer): string;
function mpvSyncAudioReset  (const mpv: IMPVBasePlayer): string;
function mpvSyncAudioUp     (const mpv: IMPVBasePlayer): string;
function mpvTakeScreenshot  (const mpv: IMPVBasePlayer; const aFolder: string): boolean;
function mpvToggleRepeat    (const mpv: IMPVBasePlayer): string;
function mpvToggleSubtitles (const mpv: IMPVBasePlayer): string;
function mpvVolDown         (const mpv: IMPVBasePlayer): string;
function mpvVolUp           (const mpv: IMPVBasePlayer): string;
function mpvZoomIn          (const mpv: IMPVBasePlayer): string;
function mpvZoomOut         (const mpv: IMPVBasePlayer): string;
function mpvZoomReset       (const mpv: IMPVBasePlayer): string;

implementation

uses
  system.sysUtils,
  vcl.forms,
  mmpKeyboardUtils,
  model.mmpConfigFile, model.mmpMPVProperties,
  _debugWindow;

function mpvCreate(var mpv: IMPVBasePlayer): boolean;
begin
  mpv    := TMPVBasePlayer.create;
  result := mpv <> NIL;
end;

function mpvInitPlayer(const mpv: IMPVBasePlayer; const sWinHandle: HWND; const sScrShotDir: string; const sConfigDir: string; const sLogFile: string = EMPTY; fEventWait: double = 0.5): TMPVErrorCode;
begin
  result := mpv.initPlayer(intToStr(sWinHandle), sScrShotDir, sConfigDir, sLogFile, fEventWait);  // THIS RECREATES THE INTERNAL MPV OBJECT
end;

function mpvOpenFile(const mpv: IMPVBasePlayer; aURL: string): TMPVErrorCode;
begin
  result := 0;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.openFile(aURL);
end;

//==========

function mpvBrightnessDn(const mpv: IMPVBasePlayer): string;
var
  brightness: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('brightness', brightness);
  mpv.setPropertyInt64('brightness', brightness - 1);
  result := mpvFormattedBrightness(mpv);
end;

function mpvBrightnessReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('brightness', 0);
  result := 'brightness reset';
end;

function mpvBrightnessUp(const mpv: IMPVBasePlayer): string;
var
  brightness: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('brightness', brightness);
  mpv.setPropertyInt64('brightness', brightness + 1);
  result := mpvFormattedBrightness(mpv);
end;

function mpvChapterNext(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr('add chapter 1') = MPV_ERROR_SUCCESS;
end;

function mpvChapterPrev(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr('add chapter -1') = MPV_ERROR_SUCCESS;
end;

function mpvContrastDn(const mpv: IMPVBasePlayer): string;
var
  contrast: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('contrast', contrast);
  mpv.setPropertyInt64('contrast', contrast - 1);
  result := mpvFormattedContrast(mpv);
end;

function mpvContrastReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('contrast', 0);
  result := 'contrast reset';
end;

function mpvContrastUp(const mpv: IMPVBasePlayer): string;
var
  contrast: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('contrast', contrast);
  mpv.setPropertyInt64('contrast', contrast + 1);
  result := mpvFormattedContrast(mpv);
end;

function mpvCycleAudio(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr('cycle audio') = MPV_ERROR_SUCCESS;
end;

function mpvCycleSubs(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr('cycle sub') = MPV_ERROR_SUCCESS;
end;

function mpvFrameBackwards(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr(CMD_BACK_STEP) = MPV_ERROR_SUCCESS;
end;

function mpvFrameForwards(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.commandStr(CMD_STEP) = MPV_ERROR_SUCCESS;
end;

function mpvGammaDn(const mpv: IMPVBasePlayer): string;
var
  gamma: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('gamma', gamma);
  mpv.setPropertyInt64('gamma', gamma - 1);
  result := mpvFormattedgamma(mpv);
end;

function mpvGammaReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('gamma', 0);
  result := 'gamma reset';
end;

function mpvGammaUp(const mpv: IMPVBasePlayer): string;
var
  gamma: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('gamma', gamma);
  mpv.setPropertyInt64('gamma', gamma + 1);
  result := mpvFormattedgamma(mpv);
end;

function mpvMute(const mpv: IMPVBasePlayer; const aValue: boolean): string;
var vValue: boolean;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpvSetMute(mpv, aValue);
  mpvGetMute(mpv, vValue);
  case vValue of    TRUE: result := 'muted';
                   FALSE: result := 'unmuted'; end;
  case vValue of    TRUE: CF[CONF_MUTED] := 'yes';
                   FALSE: CF[CONF_MUTED] := 'no'; end;
end;

function mpvMuteUnmute(const mpv: IMPVBasePlayer): string;
var vValue: boolean;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpvGetMute(mpv, vValue);
  mpvSetMute(mpv, NOT vValue);
  mpvGetMute(mpv, vValue);
  case vValue of    TRUE: result := 'muted';
                   FALSE: result := 'unmuted'; end;
  case vValue of    TRUE: CF[CONF_MUTED] := 'yes';
                   FALSE: CF[CONF_MUTED] := 'no'; end;
end;

function mpvPanDn(const mpv: IMPVBasePlayer): string;
var
  panY: double;
  multiplier: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: multiplier := 2;
                                   FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY + (0.001 * multiplier));
  result := 'pan down';
end;

function mpvPanLeft(const mpv: IMPVBasePlayer): string;
var
  panX: double;
  multiplier: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: multiplier := 2;
                                   FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX - (0.001 * multiplier));
  result := 'pan left';
end;

function mpvPanReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  result := 'pan reset';
end;

function mpvPanRight(const mpv: IMPVBasePlayer): string;
var
  panX: double;
  multiplier: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: multiplier := 2;
                                   FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.setPropertyDouble('video-pan-x', panX + (0.001 * multiplier));
  result := 'pan right';
end;

function mpvPanUp(const mpv: IMPVBasePlayer): string;
var
  panY: double;
  multiplier: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: multiplier := 2;
                                   FALSE: multiplier := 1; end;

  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.setPropertyDouble('video-pan-y', panY - (0.001 * multiplier));
  result := 'pan up';
end;

function mpvPause(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.pause = MPV_ERROR_SUCCESS;
end;

function mpvPausePlay(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  case mpvState(mpv) of
    mpsPlay:  mpvPause(mpv);
    mpsPause: mpvResume(mpv);
  end;
  case mpvState(mpv) of
    mpsPlay:  result := 'paused';
    mpsPause: result := 'unpaused';
  end;
end;

function mpvResetAll(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpvBrightnessReset(mpv);
  mpvContrastReset(mpv);
  mpvGammaReset(mpv);
  mpvPanReset(mpv);
  mpvRotateReset(mpv);
  mpvSaturationReset(mpv);
  mpvSpeedReset(mpv);
  mpvSyncAudioReset(mpv);
  mpvZoomReset(mpv);
  result := 'Reset All';
end;

function mpvResume(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.resume = MPV_ERROR_SUCCESS;
end;

function mpvRotateLeft(const mpv: IMPVBasePlayer): string;
var
  rot: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot - 45);
  result := 'rotate left';
end;

function mpvRotateReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('video-rotate', 0);
  result := 'rotate reset';
end;

function mpvRotateRight(const mpv: IMPVBasePlayer): string;
var
  rot: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.setPropertyInt64('video-rotate', rot + 45);
  result := 'rotate right';
end;

function mpvSaturationDn(const mpv: IMPVBasePlayer): string;
var
  saturation: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('saturation', saturation);
  mpv.setPropertyInt64('saturation', saturation - 1);
  result := mpvFormattedsaturation(mpv);
end;

function mpvSaturationReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyInt64('saturation', 0);
  result := 'saturation reset';
end;

function mpvSeek(const mpv: IMPVBasePlayer; const aValue: integer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.Seek(aValue, FALSE) = MPV_ERROR_SUCCESS;
end;

function mpvSaturationUp(const mpv: IMPVBasePlayer): string;
var
  saturation: int64;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('saturation', saturation);
  mpv.setPropertyInt64('saturation', saturation + 1);
  result := mpvFormattedsaturation(mpv);
end;

function mpvSpeedDn(const mpv: IMPVBasePlayer): string;
var
  speed: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed - 0.01);
  result := mpvFormattedSpeed(mpv);
end;

function mpvSpeedReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('speed', 1.00);
  result := 'speed reset';
end;

function mpvSpeedUp(const mpv: IMPVBasePlayer): string;
var
  speed: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('speed', speed);
  mpv.setPropertyDouble('speed', speed + 0.01);
  result := mpvFormattedSpeed(mpv);
end;

function mpvStartOver(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.seek(0, FALSE);
  result := 'start over';
end;

function mpvStop(const mpv: IMPVBasePlayer): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.stop = MPV_ERROR_SUCCESS;
end;

function mpvSyncAudioDn(const mpv: IMPVBasePlayer): string;
// delay the audio by increasing the value
var
  delay: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('audio-delay', delay);
  mpv.setPropertyDouble('audio-delay', delay + 0.01);
  result := mpvFormattedSyncAudio(mpv);
end;

function mpvSyncAudioReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('audio-delay', 0.00);
  result := 'sync reset';
end;

function mpvSyncAudioUp(const mpv: IMPVBasePlayer): string;
// bring the audio forward by reducing the value
var
  delay: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('audio-delay', delay);
  mpv.setPropertyDouble('audio-delay', delay - 0.01);
  result := mpvFormattedSyncAudio(mpv);
end;

function mpvTakeScreenshot(const mpv: IMPVBasePlayer; const aFolder: string): boolean;
begin
  result := FALSE;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyString('screenshot-directory', aFolder);
  result := mpv.commandStr(CMD_SCREEN_SHOT + ' window') = MPV_ERROR_SUCCESS;
end;

function mpvToggleRepeat(const mpv: IMPVBasePlayer): string;
var vLoop: string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyString('loop-file', vLoop);
  case vLoop = 'no' of  TRUE: mpv.setPropertyString('loop-file', 'yes');
                       FALSE: mpv.setPropertyString('loop-file', 'no'); end;
  mpv.getPropertyString('loop-file', vLoop);
  case vLoop = 'no' of  TRUE: result := 'repeat off';
                       FALSE: result := 'repeat on'; end;
end;

function mpvToggleSubtitles(const mpv: IMPVBasePlayer): string;
var vSid: string;
begin
  result := EMPTY;
  mpv.getPropertyString('sub', vSid);
  case vSid = 'no' of  TRUE: mpv.setPropertyString('sub', 'auto');
                      FALSE: mpv.setPropertyString('sub', 'no'); end;
  mpv.getPropertyString('sub', vSid);
  case vSid = 'no' of  TRUE: result := 'subtitles off';
                      FALSE: result := 'subtitles on'; end;
end;

function mpvVolDown(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpvMute(mpv, FALSE);
  mpv.volume := mpv.volume - 1;
  CF[CONF_VOLUME] := intToStr(trunc(mpv.volume));
  result := mpvFormattedVol(mpv);
end;

function mpvVolUp(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpvMute(mpv, FALSE);
  mpv.volume := mpv.volume + 1;
  CF[CONF_VOLUME] := intToStr(trunc(mpv.volume));
  result := mpvFormattedVol(mpv);
end;

function mpvZoomIn(const mpv: IMPVBasePlayer): string;
var
  zoomX, zoomY: double;
  dx: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: dx := 0.10;
                                   FALSE: dx := 0.01; end;

  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX + dx);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY + dx);
  result := 'zoom in';
end;

function mpvZoomOut(const mpv: IMPVBasePlayer): string;
var
  zoomX, zoomY: double;
  dx: double;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;

  case ssShift in mmpShiftState of  TRUE: dx := 0.10;
                                   FALSE: dx := 0.01; end;

  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.setPropertyDouble('video-scale-x', zoomX - dx);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  mpv.setPropertyDouble('video-scale-y', zoomY - dx);
  result := 'zoom out';
end;

function mpvZoomReset(const mpv: IMPVBasePlayer): string;
begin
  result := EMPTY;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyDouble('video-pan-x', 0.0);
  mpv.setPropertyDouble('video-pan-y', 0.0);
  mpv.setPropertyDouble('video-scale-x', 1.00);
  mpv.setPropertyDouble('video-scale-y', 1.00);
  result := 'zoom reset';
end;


end.
