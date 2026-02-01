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
unit mmpMPVProperties;

interface

uses
  system.types,
  MPVBasePlayer,
  mmpAction,
  _debugWindow;

function mpvAdjusted(const mpv: IMPVBasePlayer): boolean;
function mpvDuration(const mpv: IMPVBasePlayer): integer;
function mpvFileName(const mpv: IMPVBasePlayer): string;
function mpvPosition(const mpv: IMPVBasePlayer): integer;
function mpvState(const mpv: IMPVBasePlayer): TMPVPlayerState;
function mpvVideoHeight(const mpv: IMPVBasePlayer): int64;
function mpvVideoWidth(const mpv: IMPVBasePlayer): int64;
function mpvXY(const mpv: IMPVBasePlayer): TPoint;

//==========

function mpvGetPropertyString(const mpv: IMPVBasePlayer; const aProperty: string; var  aString: string): TMPVErrorCode;
function mpvSetPropertyString(const mpv: IMPVBasePlayer; const aProperty: string; const aValue: string): TMPVErrorCode;
function mpvgetMute(const mpv: IMPVBasePlayer; var aBoolean: boolean): TMPVErrorCode;
function mpvSetMute(const mpv: IMPVBasePlayer; const value: boolean): TVoid;
function mpvSetKeepOpen(const mpv: IMPVBasePlayer; const value: boolean): TVoid;
function mpvSetVolume(const mpv: IMPVBasePlayer; const aVolume: integer): TVoid;

function mpvSetDefaults(const mpv: IMPVBasePlayer; const aExePath: string): TVoid;

implementation

function mpvAdjusted(const mpv: IMPVBasePlayer): boolean;
var
  brightness: int64;
  contrast: int64;
  gamma: int64;
  panX: double;
  panY: double;
  rot: int64;
  saturation: int64;
  zoomX, zoomY: double;
begin
  mpv.GetPropertyInt64('brightness', brightness);
  mpv.GetPropertyInt64('contrast', contrast);
  mpv.GetPropertyInt64('gamma', gamma);
  mpv.getPropertyDouble('video-pan-x', panX);
  mpv.getPropertyDouble('video-pan-y', panY);
  mpv.getPropertyInt64('video-rotate', rot);
  mpv.GetPropertyInt64('saturation', saturation);
  mpv.getPropertyDouble('video-scale-x', zoomX);
  mpv.getPropertyDouble('video-scale-y', zoomY);
  result := brightness + contrast + gamma + panX + panY + rot + saturation + zoomX + zoomY <> 2; // video-scale-x = 1; video-scale-y = 1

//  debugFormat('brightness: %d, contrast: %d, gamma: %d, panX: %f, panY: %f, rot: %d, saturation: %d, zoomX: %f, zoomY: %f', [brightness, contrast, gamma, panX, panY, rot, saturation, zoomX, zoomY]);
//  debugBoolean('mpvAdjusted', result);
end;

function mpvDuration(const mpv: IMPVBasePlayer): integer;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := trunc(mpv.totalSeconds);
end;

function mpvFileName(const mpv: IMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.FileName;
end;

function mpvPosition(const mpv: IMPVBasePlayer): integer;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := trunc(mpv.currentSeconds);
end;

function mpvState(const mpv: IMPVBasePlayer): TMPVPlayerState;
begin
  result := mpsUnk;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.getState;
end;

function mpvVideoHeight(const mpv: IMPVBasePlayer): int64;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.videoHeight;
end;

function mpvVideoWidth(const mpv: IMPVBasePlayer): int64;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.videoWidth;
end;

function mpvXY(const mpv: IMPVBasePlayer): TPoint;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := point(mpvVideoWidth(mpv), mpvVideoHeight(mpv));
end;

//==========

function mpvGetPropertyString(const mpv: IMPVBasePlayer; const aProperty: string; var aString: string): TMPVErrorCode;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.getPropertyString(aProperty, aString, FALSE);
end;

function mpvSetPropertyString(const mpv: IMPVBasePlayer; const aProperty: string; const aValue: string): TMPVErrorCode;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  result := mpv.setPropertyString(aProperty, aValue);
end;

function mpvSetKeepOpen(const mpv: IMPVBasePlayer; const value: boolean): TVoid;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyBool('keep-open', value); // ensure libmpv MPV_EVENT_END_FILE_ event at the end of every media file
end;

function mpvgetMute(const mpv: IMPVBasePlayer; var aBoolean: boolean): TMPVErrorCode;
begin
  result := -1;
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyBool('mute', aBoolean);
end;

function mpvSetMute(const mpv: IMPVBasePlayer; const value: boolean): TVoid;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.setPropertyBool('mute', value);
end;

function mpvSetVolume(const mpv: IMPVBasePlayer; const aVolume: integer): TVoid;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.volume := aVolume;
end;

function mpvSetDefaults(const mpv: IMPVBasePlayer; const aExePath: string): TVoid;
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  case mpv = NIL of TRUE: EXIT; end;
  with mpv do begin
    SetPropertyString('osc', 'no'); // On Screen Control
    SetPropertyString('force-window', 'yes');
    SetPropertyString('config-dir', aExePath); // mpv.conf location
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
//    setPropertyString('sid', '1');
    setPropertyString('image-display-duration', 'inf');
//    SetPropertyDouble('sub-delay', -00.99);
  end;
end;

end.
