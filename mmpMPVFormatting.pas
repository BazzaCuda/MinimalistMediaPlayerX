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
unit mmpMPVFormatting;

interface

uses
  MPVBasePlayer;

function mmpFormatFileSize(const aSize: int64): string;
function mmpFormatSeconds(const seconds: integer): string;
function mmpFormatTime(const seconds: integer): string;
function mmpFormatWidthHeight(const width, height: integer): string;

function mpvFormattedBrightness(const mpv: TMPVBasePlayer): string;
function mpvFormattedContrast(const mpv: TMPVBasePlayer): string;
function mpvFormattedDuration(const mpv: TMPVBasePlayer): string;
function mpvFormattedGamma(const mpv: TMPVBasePlayer): string;
function mpvFormattedSaturation(const mpv: TMPVBasePlayer): string;
function mpvFormattedSpeed(const mpv: TMPVBasePlayer): string;
function mpvFormattedTime(const mpv: TMPVBasePlayer): string;
function mpvFormattedVol(const mpv: TMPVBasePlayer): string;

implementation

uses
  system.sysUtils;

function mmpFormatFileSize(const aSize: int64): string;
begin
 case aSize >= 1052266987 of  TRUE: try result := format('FS:  %.3f GB', [aSize / 1024 / 1024 / 1024]); except end;  // >= 0.98 of 1GB
                             FALSE: case aSize < 1024 * 1024 of  TRUE: try result := format('FS:  %d KB', [trunc(aSize / 1024)]); except end;
                                                                FALSE: try result := format('FS:  %.2f MB', [aSize / 1024 / 1024]); except end;end;end;
end;

function mmpFormatSeconds(const seconds: integer): string;
begin
  case seconds < 100 of  TRUE: result := format('%ds', [seconds]);
                        FALSE: result := format('%dm%.2ds', [seconds div 60, seconds mod 60]);
  end;
end;

function mmpFormatWidthHeight(const width, height: integer): string;
begin
  result := format('%dx%d', [width, height]);
end;

function mmpFormatTime(const seconds: integer): string;
begin
  case seconds < 60 of  TRUE: result := format('%.2d:%.2d', [0, seconds]);
                       FALSE: case seconds < 3600 of  TRUE: result := format('%.2d:%.2d', [seconds div 60, seconds mod 60]);
                                                     FALSE: result := format('%.2d:%.2d:%.2d', [seconds div 3600, (seconds mod 3600) div 60, seconds mod 3600 mod 60]); end;end;
end;

//==========

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
