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
unit model.mmpMPVFormatting;

interface

uses
  MPVBasePlayer,
  mmpFormatting,
  _debugWindow;

function mpvFormattedBrightness (const mpv: IMPVBasePlayer): string;
function mpvFormattedContrast   (const mpv: IMPVBasePlayer): string;
function mpvFormattedDuration   (const mpv: IMPVBasePlayer): string;
function mpvFormattedGamma      (const mpv: IMPVBasePlayer): string;
function mpvFormattedSaturation (const mpv: IMPVBasePlayer): string;
function mpvFormattedSpeed      (const mpv: IMPVBasePlayer): string;
function mpvFormattedSyncAudio  (const mpv: IMPVBasePlayer): string;
function mpvFormattedTime       (const mpv: IMPVBasePlayer): string;
function mpvFormattedVol        (const mpv: IMPVBasePlayer): string;

implementation

uses
  system.sysUtils;

function mpvFormattedBrightness(const mpv: IMPVBasePlayer): string;
var
  vBrightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('brightness', vBrightness);
  result := format('Brightness: %d', [vBrightness]);
end;

function mpvFormattedContrast(const mpv: IMPVBasePlayer): string;
var
  vContrast: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('contrast', vContrast);
  result := format('Contrast: %d', [vContrast]);
end;

function mpvFormattedDuration(const mpv: IMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mmpFormatTime(trunc(mpv.totalSeconds));
end;

function mpvFormattedGamma(const mpv: IMPVBasePlayer): string;
var
  vGamma: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('gamma', vGamma);
  result := format('Gamma: %d', [vGamma]);
end;

function mpvFormattedSaturation(const mpv: IMPVBasePlayer): string;
var
  vSaturation: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyInt64('saturation', vSaturation);
  result := format('Saturation: %d', [vSaturation]);
end;

function mpvFormattedSpeed(const mpv: IMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Speed: %.2f', [mpv.PlaybackSpeed]);
end;

function mpvFormattedSyncAudio(const mpv: IMPVBasePlayer): string;
var
  vDelay: double;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.getPropertyDouble('audio-delay', vDelay);
  result := format('Sync: %+.2f', [vDelay]);
end;

function mpvFormattedTime(const mpv: IMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mmpFormatTime(trunc(mpv.CurrentSeconds));
end;

function mpvFormattedVol(const mpv: IMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Volume: %d', [trunc(mpv.volume)]);
end;

end.
