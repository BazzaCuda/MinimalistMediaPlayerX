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
  system.sysUtils,
  system.classes,
  system.generics.collections,
  mmpAction;

type
  TKeyMinute = record
    kmKeyFrames: TList<double>; // KeyFrame timestamps as doubles
  end;

  IKeyFrameManager = interface
    function  clearKeyFrames: TVoid;
    function  init(const aFilePath: string): TVoid;
    function  proximity(const aPositionSS: integer): double;
  end;

  TKeyFrameManager = class(TInterfacedObject, IKeyFrameManager)
  strict private
    FMinutes: TDictionary<integer, TKeyMinute>; // minute Ix -> KeyFrame record
    FFilePath: string;
    FFetching: boolean;
  private
    function  getKeyFrameList(const aPositionSS: integer): TList<double>;
    function  probeKeyFrames(const aPositionSS: integer): boolean;
    function  runProbe(const aFilePath: string; const aPositionSS: integer; const aStringList: TStringList): boolean;
  public
    constructor Create;
    destructor  Destroy; override;

    // IKeyFrameManager
    function  clearKeyFrames: TVoid;
    function  init(const aFilePath: string): TVoid;
    function  proximity(const aPositionSS: integer): double;
  end;

function keyFrameManager: IKeyFrameManager;

implementation

uses
  system.ioUtils,
  vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  view.mmpFormTimeline,
  mmpCmd, mmpFileUtils, mmpUtils,
  _debugWindow;

function keyFrameManager: IKeyFrameManager;
{$J+} const gKFM: IKeyFrameManager = NIL; {$J-}
begin
  case gKFM = NIL of TRUE: gKFM := TKeyFrameManager.create; end;
  result := gKFM;
end;

function mmpKeyFile(const aFilePath: string; const aMinuteIx: integer): string;
begin
  var vPath := extractFilePath(aFilePath);
  var vFile := TPath.getFileNameWithoutExtension(aFilePath);

  vFile := format('%s%s%0.3d%s', [vFile, '-', aMinuteIx, '.key']);

  result := format('%s%s', [vPath, vFile]);
end;

{ TKeyFrameManager }

function TKeyFrameManager.clearKeyFrames: TVoid;
begin
  for var vKeyMinute in FMinutes.values do
    vKeyMinute.kmKeyFrames.free;
  FMinutes.clear;
end;

constructor TKeyFrameManager.Create;
begin
  inherited Create;
  FMinutes  := TDictionary<integer, TKeyMinute>.Create;
end;

destructor TKeyFrameManager.Destroy;
begin
  clearKeyFrames;

  FMinutes.free;
  inherited Destroy;
end;

function TKeyFrameManager.getKeyFrameList(const aPositionSS: integer): TList<double>;
var
  vMinuteIx: integer;
begin
  result := NIL;
  case FFetching of TRUE: EXIT; end;

  vMinuteIx := aPositionSS div 60;

  case FMinutes.containsKey(vMinuteIx) of FALSE: begin
                                                      FFetching := TRUE;
                                                      try
                                                        case probeKeyFrames(aPositionSS) of FALSE: EXIT; end;
                                                      finally
                                                        FFetching := FALSE;
                                                      end;end;end;

  result := FMinutes[vMinuteIx].kmKeyFrames;
end;

function TKeyFrameManager.runProbe(const aFilePath: string; const aPositionSS: integer; const aStringList: TStringList): boolean;
begin
  result := FALSE;
  mmp.cmd(evSTOpInfo, 'KeyFrames...');

  var vMinuteIx := aPositionSS div 60;
  var vStartSS  := vMinuteIx * 60;
  var vEndSS    := vStartSS + 60;

  var vKeyFile := mmpKeyFile(aFilePath, vMinuteIx);
  case fileExists(vKeyFile) of TRUE: deleteFile(vKeyFile); end;

  var vFFprobe  := ''; // mmpExePath + 'ffprobe.exe';
  var vParams   := ' -v quiet -skip_frame nokey -select_streams v:0 -show_entries frame=pts_time -of default=noprint_wrappers=1:nokey=1 ';
  var vInterval := format(' -read_intervals %d%s%d', [vStartSS, '%', vEndSS]);
  var vKeyFileO := ' -o ' + '"' + vKeyFile + '"';
  var vInFile   := ' "' + aFilePath + '"';

  TLExecAndWait(vFFProbe + vParams + vInterval + vKeyFileO + vInFile, rtFFProbe);

  case fileExists(vKeyFile) of   TRUE: aStringList.LoadFromFile(vKeyFile);
                                FALSE: EXIT; end;

  result := TRUE;
end;

function TKeyFrameManager.init(const aFilePath: string): TVoid;
begin
  FFilePath := aFilePath;
end;

function TKeyFrameManager.probeKeyFrames(const aPositionSS: integer): boolean;
var
  vKeyMinute:     TKeyMinute;
  vProcessOutput: TStringList;
  vLine:          string;
  vValue:         double;
begin
  result := FALSE;

  var vMinuteIx := aPositionSS div 60;

  vKeyMinute.kmKeyFrames := TList<double>.create;
  vProcessOutput := TStringList.create;
  try
    case runProbe(FFilePath, aPositionSS, vProcessOutput) of FALSE: EXIT; end;

    for vLine in vProcessOutput do
      case tryStrToFloat(vLine, vValue) of TRUE: vKeyMinute.kmKeyFrames.add(vValue); end;

    vKeyMinute.kmKeyFrames.sort;
    FMinutes.add(vMinuteIx, vKeyMinute);
  finally
    vProcessOutput.free;
  end;

  mmp.cmd(evPBBackgroundColor, clFuchsia); // mmpFormTimeline will reset the color (evPBBackgroundColor) on the next tick
  mmpDelay(100);
  mmp.cmd(evPBBackgroundColor, clFuchsia); // make sure it gets seen during a tick cycle

  result := TRUE;
end;

function TKeyFrameManager.proximity(const aPositionSS: integer): double;
var
  vKeyFrames: TList<double>;
  vInsertIx:  integer;
begin
  result := -1;

  var vMinuteIx  := aPositionSS div 60;

  vKeyFrames := getKeyFrameList(aPositionSS);
  case vKeyFrames = NIL of TRUE: EXIT; end;

  result := 0;
  case vKeyFrames.binarySearch(aPositionSS, vInsertIx) of TRUE: EXIT; end; // highly unlikely to ever be true

  result := -1;
  case vInsertIx > 0 of TRUE: result := aPositionSS - vKeyFrames[vInsertIx - 1]; end;
end;

end.
