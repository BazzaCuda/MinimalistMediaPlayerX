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
    FMinutes:   TDictionary<integer, TKeyMinute>; // minute Ix -> KeyFrame record
    FFilePath:  string;
    FFetching:  boolean;
    FMasterKey: TStringList;
  private
    function  appendMasterKey(const aPositionSS: integer; const aStringList: TStringList): TVoid;
    function  flashProgressBar: TVoid;
    function  getKeyFrameList(const aPositionSS: integer): TList<double>;
    function  keyFile(const aPositionSS: integer): string;
    function  loadMasterKey: TVoid;
    function  minuteIx(const aPositionSS: integer): integer;
    function  probeKeyFrames(const aPositionSS: integer): boolean;
    function  runProbe(const aFilePath: string; const aPositionSS: integer; const aStringList: TStringList): boolean;
    function  saveMasterKey: TVoid;
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
  mmpCmd, mmpConsts, mmpFileUtils, mmpUtils,
  _debugWindow;

function keyFrameManager: IKeyFrameManager;
{$J+} const gKFM: IKeyFrameManager = NIL; {$J-}
begin
  case gKFM = NIL of TRUE: gKFM := TKeyFrameManager.create; end;
  result := gKFM;
end;

{ TKeyFrameManager }

function TKeyFrameManager.clearKeyFrames: TVoid;
begin
  for var vKeyMinute in FMinutes.values do vKeyMinute.kmKeyFrames.free;
  FMinutes.clear;
end;

constructor TKeyFrameManager.Create;
begin
  inherited Create;
  FMinutes    := TDictionary<integer, TKeyMinute>.Create;
  FMasterKey  := TStringList.create;
end;

destructor TKeyFrameManager.Destroy;
begin
  clearKeyFrames;

  FMasterKey.free;
  FMinutes.free;
  inherited Destroy;
end;

function TKeyFrameManager.flashProgressBar: TVoid;
begin
  mmp.cmd(evPBBackgroundColor, clFuchsia); // mmpFormTimeline will reset the color (evPBBackgroundColor) on the next tick
  mmpDelay(100);
  mmp.cmd(evPBBackgroundColor, clFuchsia); // make sure it gets seen during a tick cycle
end;

function TKeyFrameManager.getKeyFrameList(const aPositionSS: integer): TList<double>;
begin
  result := NIL;
  case FFetching of TRUE: EXIT; end;

  var vMinuteIx := aPositionSS div 60;

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
  mmp.cmd(evSTOpInfo, 'keyframes...');

  var vStartSS  := minuteIx(aPositionSS) * 60;
  var vEndSS    := vStartSS + 60;

  var vKeyFile := keyFile(aPositionSS);
  case fileExists(vKeyFile) of TRUE: deleteFile(vKeyFile); end;

//  var vParams   := ' -v quiet -skip_frame nokey -select_streams v:0 -show_entries frame=pts_time  -of default=noprint_wrappers=1:nokey=1 ';
  var vParams   := ' -v quiet -skip_frame nokey -show_packets -select_streams v:0 -show_entries packet=pts_time,flags -of csv=print_section=0 ';

  var vInterval := format(' -read_intervals %d%s%d', [vStartSS, '%', vEndSS]);
  var vKeyFileO := ' -o ' + '"' + vKeyFile + '"';
  var vInFile   := ' "' + aFilePath + '"';

  TLExecAndWait(vParams + vInterval + vKeyFileO + vInFile, rtFFProbe);

  case fileExists(vKeyFile) of   TRUE: aStringList.LoadFromFile(vKeyFile);
                                FALSE: EXIT; end;

  deleteFile(vKeyFile);

  result := TRUE;
end;

function TKeyFrameManager.saveMasterKey: TVoid;
begin
  FMasterKey.saveToFile(changeFileExt(FFilePath, '.key'));
end;

function TKeyFrameManager.appendMasterKey(const aPositionSS: integer; const aStringList: TStringList): TVoid;
begin
  FMasterKey.add(format('[%0.3d]', [minuteIx(aPositionSS)]));
  FMasterKey.addStrings(aStringList);
  saveMasterKey;
end;

function TKeyFrameManager.init(const aFilePath: string): TVoid;
begin
  FFilePath := aFilePath;
  loadMasterKey;
end;

function TKeyFrameManager.keyFile(const aPositionSS: integer): string;
begin
  var vPath := extractFilePath(FFilePath);
  var vFile := TPath.getFileNameWithoutExtension(FFilePath);

  vFile := format('%s%s%0.3d%s', [vFile, '-', minuteIx(aPositionSS), '.key']);

  result := format('%s%s', [vPath, vFile]);
end;

function TKeyFrameManager.loadMasterKey: TVoid;
begin
  var vKeyFile := changeFileExt(FFilePath, '.key');
  case fileExists(vKeyFile) of FALSE: EXIT; end;

  case mmpCompareFileTimestamps(FFilePath, vKeyFile) of FALSE:  begin                 // the key file is out of date compared to the video file
                                                                  FMasterKey.clear;   // force getting brand new keyframe info from ffprobe
                                                                  saveMasterKey;
                                                                  EXIT; end;end;

  clearKeyFrames;
  FMasterKey.loadFromFile(vKeyFile);

  var vMinuteIx: integer;

  for var vLine in FMasterKey do
    case vLine[1] = '[' of TRUE:  begin
                                    vMinuteIx := strToIntDef(copy(vLine, 2, length(vLine) - 2), -1);

                                    var vKeyMinute: TKeyMinute;
                                    vKeyMinute.kmKeyFrames := TList<double>.create;
                                    FMinutes.add(vMinuteIx, vKeyMinute);
                                    CONTINUE; end;
                          FALSE:  begin
                                    var vValue: double;
                                    case tryStrToFloat(vLine, vValue) of TRUE: FMinutes[vMinuteIx].kmKeyFrames.add(vValue); end;end;end;

  case FMinutes.count > 0 of TRUE: flashProgressBar; end;
end;

function TKeyFrameManager.minuteIx(const aPositionSS: integer): integer;
begin
  result := aPositionSS div 60;
end;

function TKeyFrameManager.probeKeyFrames(const aPositionSS: integer): boolean;
var
  vKeyMinute:     TKeyMinute;
  vValue:         double;
begin
  result := FALSE;

      vKeyMinute.kmKeyFrames  := TList<double>.create;
  var vProcessOutput          := TStringList.create;
  var vCleanData              := TStringList.create;

  try
    case runProbe(FFilePath, aPositionSS, vProcessOutput) of FALSE: EXIT; end;

    for var vLine in vProcessOutput do
      case pos('K', vLine) = 0 of FALSE: vCleanData.add(copy(vLine, 1, pos(',', vLine) - 1)); end;

    appendMasterKey(aPositionSS, vCleanData);

    for var vLine in vCleanData do
      case tryStrToFloat(vLine, vValue) of TRUE: vKeyMinute.kmKeyFrames.add(vValue); end;

    vKeyMinute.kmKeyFrames.sort;
    FMinutes.add(minuteIx(aPositionSS), vKeyMinute);
  finally
    vCleanData.free;
    vProcessOutput.free;
  end;

  flashProgressBar;

  result := TRUE;
end;

function TKeyFrameManager.proximity(const aPositionSS: integer): double;
var
  vKeyFrames: TList<double>;
  vInsertIx:  integer;
begin
  result := -1;

  vKeyFrames := getKeyFrameList(aPositionSS);
  case vKeyFrames = NIL of TRUE: EXIT; end;

  result := 0;
  case vKeyFrames.binarySearch(aPositionSS, vInsertIx) of TRUE: EXIT; end; // highly unlikely to ever be true

  result := -1;
  case vInsertIx > 0 of TRUE: result := aPositionSS - vKeyFrames[vInsertIx - 1]; end;
end;

end.
