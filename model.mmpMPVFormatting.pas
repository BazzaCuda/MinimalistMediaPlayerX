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

function mpvFormattedBrightness (const mpv: TMPVBasePlayer): string;
function mpvFormattedContrast   (const mpv: TMPVBasePlayer): string;
function mpvFormattedDuration   (const mpv: TMPVBasePlayer): string;
function mpvFormattedGamma      (const mpv: TMPVBasePlayer): string;
function mpvFormattedSaturation (const mpv: TMPVBasePlayer): string;
function mpvFormattedSpeed      (const mpv: TMPVBasePlayer): string;
function mpvFormattedTime       (const mpv: TMPVBasePlayer): string;
function mpvFormattedVol        (const mpv: TMPVBasePlayer): string;

implementation

uses
  system.sysUtils;

function mpvFormattedBrightness(const mpv: TMPVBasePlayer): string;
var
  vBrightness: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('brightness', vBrightness);
  result := format('Brightness: %d', [vBrightness]);
end;

function mpvFormattedContrast(const mpv: TMPVBasePlayer): string;
var
  vContrast: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('contrast', vContrast);
  result := format('Contrast: %d', [vContrast]);
end;

function mpvFormattedDuration(const mpv: TMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mmpFormatTime(trunc(mpv.totalSeconds));
end;

function mpvFormattedGamma(const mpv: TMPVBasePlayer): string;
var
  vGamma: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('gamma', vGamma);
  result := format('Gamma: %d', [vGamma]);
end;

function mpvFormattedSaturation(const mpv: TMPVBasePlayer): string;
var
  vSaturation: int64;
begin
  case mpv = NIL of TRUE: EXIT; end;
  mpv.GetPropertyInt64('saturation', vSaturation);
  result := format('Saturation: %d', [vSaturation]);
end;

function mpvFormattedSpeed(const mpv: TMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Speed: %.2f', [mpv.PlaybackSpeed]);
end;

function mpvFormattedTime(const mpv: TMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := mmpFormatTime(trunc(mpv.CurrentSeconds));
end;

function mpvFormattedVol(const mpv: TMPVBasePlayer): string;
begin
  case mpv = NIL of TRUE: EXIT; end;
  result := format('Volume: %d', [trunc(mpv.volume)]);
end;

end.
