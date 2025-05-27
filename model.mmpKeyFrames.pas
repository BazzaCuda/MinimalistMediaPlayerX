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
unit model.mmpKeyFrames;

interface

uses
  system.generics.collections;

function mmpClearKeyFrames: boolean;
function mmpDoKeyFrames(const aURL: string): boolean;
function mmpGetKeyFrames(const aURL: string): boolean;
function mmpKeyFile(const aURL: string): string;
function mmpKeyFrameProximity(const aKeyFrame: double): double;
function mmpKeyFrames: TList<double>;
function mmpMockKeyFrame(const aWidth: integer; const aDurationSS: double; const aPixelX: integer): double;
function mmpOnKeyframe(const aKeyFrame: double; const aStartSS: double; const aEndSS: double): boolean;
function mmpSetKeyFrames(const aURL: string): boolean;

implementation

uses
  system.classes, system.sysUtils,
  vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpFileUtils, mmpFuncProg, mmpShellUtils, mmpUtils,
  _debugWindow;

{$J+} const gKeyFrames: TList<double> = NIL; {$J-}

function mmpClearKeyFrames: boolean;
begin
  case gKeyFrames = NIL of FALSE: mmpKeyFrames.clear; end; // don't create it by calling mmpKeyFrames if we don't need it
end;

function mmpDoKeyFrames(const aURL: string): boolean;
begin
  result := FALSE;
  mmpClearKeyFrames; // don't reuse a previous file's keyframes
  mmpGetKeyFrames(aURL);
  result := mmpSetKeyFrames(aURL);
end;

function mmpKeyFile(const aURL: string): string;
begin
  result := changeFileExt(aURL, '.key');
end;

function mmpGetKeyFrames(const aURL: string): boolean;
begin
  result := FALSE;
  mmp.cmd(evSTOpInfo, 'keyframes...');

  case mmpCompareFileTimestamps(aURL, mmpKeyFile(aURL)) of TRUE: EXIT; end; // already got the most up to date .key file
  try
    var vFFprobe  := mmpExePath + 'ffprobe.exe';
    var vParams   := ' -v quiet -skip_frame nokey -select_streams v:0 -show_entries frame=pts_time -of default=noprint_wrappers=1:nokey=1 ';
    var vKeyFile  := ' -o ' + '"' + mmpKeyFile(aURL) + '"';
    var vInFile   := ' "' + aURL + '"';
//    var vRedirect := ' > ';
//    var vKeyFile  := '"' + mmpKeyFile(aURL) + '"';

    mmpExecAndWait(vFFProbe + vParams + vKeyFile + vInFile, rtDontWait);

//    mmpExecAndWait(vFFProbe + vParams + vInFile + vRedirect + vKeyFile, rtDontWait);

//    mmpExecAndWait(mmpExePath + 'ffprobe.exe' + ' -v quiet -skip_frame nokey -select_streams v:0 -show_entries frame=pts_time -of default=noprint_wrappers=1:nokey=1 "' +
//                  aURL + '" > "' + changeFileExt(aURL, '.key') + '"', rtDontWait);
    result := TRUE;
  finally
  end;
end;

function mmpKeyFrameProximity(const aKeyFrame: double): double;
// lowest valid return value is 0.0 when a direct match with a keyFrame is found
var
  vInsertIx:  integer;
begin
  result := -1;

  case gKeyFrames       = NIL of TRUE: EXIT; end; // mmpGetKeyFrames and mmpSetKeyFrames haven't been called
  case gKeyFrames.count = 0   of TRUE: EXIT; end;
  case aKeyFrame        = 0   of TRUE: EXIT; end;

  result := 0;
  case gKeyFrames.binarySearch(aKeyFrame, vInsertIx) of TRUE: EXIT; end; // highly unlikely to ever be true

  result := -1;
  case vInsertIx > 0 of TRUE: result := aKeyFrame - gKeyFrames[vInsertIx - 1]; end;
end;

function mmpKeyFrames: TList<double>;
begin
  case gKeyFrames = NIL of TRUE: gKeyFrames := TList<double>.create; end;
  result := gKeyFrames;
end;

function mmpMockKeyFrame(const aWidth: integer; const aDurationSS: double; const aPixelX: integer): double;
// not used
begin
  result := 0;
  case aPixelX     <= 0 of TRUE: EXIT; end;
  case aDurationSS <= 0 of TRUE: EXIT; end;
  case aWidth      <= 0 of TRUE: EXIT; end;
  result := (aPixelX * aDurationSS) / aWidth;
end;

function mmpSetKeyFrames(const aURL: string): boolean;
begin
  result := FALSE;
  var vSysMessage: string;
  var vKeyFramesFile := mmpKeyFile(aURL);
  case fileExists(vKeyFramesFile) of FALSE: EXIT; end;
  case mmpIsFileInUseExclusive(vKeyFramesFile, vSysMessage) of TRUE:  begin
                                                                        mmp.cmd(evSTOpInfo, 'keyframes...');
                                                                        EXIT; end;end; // only use the .key file when ffprobe has finished writing to it

  var vSL := TStringList.create;
  try
    vSL.loadFromFile(vKeyFramesFile);
    for var i := 0 to vSL.count - 1 do case strToFloatDef(vSL[i], 0) <> 0 of TRUE: mmpKeyFrames.add(strToFloatDef(vSL[i], -1)); end;
    mmpKeyFrames.sort; // ffprobe sometimes reports them out of sequence, especially the first two
                                                                          //    vSL.clear;
                                                                          //    for var aValue in mmpKeyFrames do vSL.add(floatToStr(aValue));
                                                                          //    vSL.saveToFile('B:\Downloads\MMP testing\sorted.key');
  finally
    vSL.free;
  end;

  mmp.cmd(evSTOpInfo, 'keyframes on');     // at last!
  mmp.cmd(evPBBackgroundColor, clFuchsia); // TALProgressBar.setPosition will reset the color on the next tick
  mmpDelay(100);
  mmp.cmd(evPBBackgroundColor, clFuchsia); // make sure it gets seen during a tick cycle
  result := TRUE;
end;

function mmpOnKeyFrame(const aKeyFrame: double; const aStartSS: double; const aEndSS: double): boolean;
var
  vInsertIx:  integer;
  vDiff:      double;
begin
  result := FALSE;

  case gKeyFrames       = NIL of TRUE: EXIT; end; // mmpGetKeyFrames and mmpSetKeyFrames haven't been called
  case gKeyFrames.count = 0   of TRUE: EXIT; end;
  case aKeyFrame        = 0   of TRUE: EXIT; end;

  result := gKeyFrames.binarySearch(aKeyFrame, vInsertIx);
  case result of TRUE: EXIT; end; // highly unlikely to ever be true

  result := vInsertIx > 0;
  case result of FALSE: EXIT; end;

  // Check keyframe to the left for tolerance match
  vDiff  := aKeyFrame - gKeyFrames[vInsertIx - 1];
  result := (vDiff >= aStartSS) and (vDiff <= aEndSS);
end;

initialization
finalization
  case gKeyFrames = NIL of FALSE: gKeyFrames.free; end;

end.
