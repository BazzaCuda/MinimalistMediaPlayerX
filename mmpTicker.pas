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
unit mmpTicker;

interface

uses
  system.diagnostics, system.timespan;

function tickerStart:   boolean;
function tickerStop:    boolean;
function tickerTotalMs: double;

implementation

var rStopWatch: TStopWatch;
function tickerStart: boolean;
begin
  rStopwatch := TStopwatch.startNew;
end;

function tickerStop: boolean;
begin
  rStopWatch.stop;
end;

function tickerTotalMs: double;
var vTimeSpan: TTimeSpan;
begin
  vTimeSpan := rStopWatch.elapsed;
  result := vTimeSpan.totalMilliseconds;
end;

end.
