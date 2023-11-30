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
unit formTimeline;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Generics.Collections, Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TTimelineForm = class(TForm)
    pnlCursor: TPanel;
    lblPosition: TPanel;
    imgTrashCan: TImage;
    procedure FormCreate(Sender: TObject);
    procedure pnlCursorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlCursorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlCursorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlCursorMouseEnter(Sender: TObject);
    procedure pnlCursorMouseLeave(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    FDragging: boolean;
    FPrevAction: string;
  private
    function getCursorPos: integer;
    procedure setCursorPos(const Value: integer);
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
    property cursorPos: integer read getCursorPos write setCursorPos;
  end;

  TSegment = class(TPanel)
  strict private
    FDeleted:     boolean;
    FEndSS:       integer;
    FOldColor:    TColor;
    FSegDetails:  TLabel;
    FSegID:       TLabel;
    FSelected:    boolean;
    FStartSS:     integer;
    FTrashCan:    TImage;
  private
    function  getDuration: integer;
    procedure setSegID(const Value: string);
    function  getSegID: string;
    procedure setDisplayDetails;
    procedure setSelected(const Value: boolean);
    function  getIx: integer;
    function  getIsLast: boolean;
    function  getIsFirst: boolean;
  protected
    procedure doClick(Sender: TObject);
    procedure paint; override;
  public
    constructor create(const aStartSS: integer; const aEndSS: integer; const aDeleted: boolean = FALSE);
    property deleted:   boolean read FDeleted  write FDeleted;
    property duration:  integer read getDuration;
    property endSS:     integer read FEndSS    write FEndSS;
    property isFirst:   boolean read getIsFirst;
    property isLast:    boolean read getIsLast;
    property ix:        integer read getIx;
    property oldColor:  TColor  read FOldColor write FOldColor;
    property segID:     string  read getSegID  write setSegID;
    property selected:  boolean read FSelected write setSelected;
    property startSS:   integer read FStartSS  write FStartSS;
    property trashCan:  TImage  read FTrashCan;
  end;

  TTimeline = class(TObject)
  strict private
    FMax: integer;
    FMediaFilePath: string;
    FPosition: integer;
    FSegments: TObjectList<TSegment>;
    FSelSeg: TSegment;
    FUndoList: TObjectStack<TStringList>;
    FRedoList: TObjectStack<TStringList>;
    function getMax: integer;
    function getPosition: integer;
    procedure setMax(const Value: integer);
    procedure setPosition(const Value: integer);
    function freeSegments: boolean;
  private
    constructor create;
    destructor  destroy; override;
    function addUndo(aText: string): boolean;
    function clearFocus: boolean;
    function createInPoint(const aPosition: integer): boolean;
    function createOutPoint(const aPosition: integer): boolean;
    function cutSegment(const aSegment: TSegment; const aPosition: integer; const deleteLeft: boolean = FALSE; const deleteRight: boolean = FALSE): boolean;
    function defaultSegment: boolean;
    function delSegment(const aSegment: TSegment): boolean;
    function drawSegments: boolean;
    function filePathLOG: string;
    function filePathMMP: string;
    function filePathOUT: string;
    function filePathSEG: string;
    function getSegCount: integer;
    function getTimelineHeight: integer;
    function loadSegments(aStringList: TStringList = NIL): boolean;
    function log(aLogEntry: string): boolean;
    function mergeLeft(const aSegment: TSegment): boolean;
    function mergeRight(const aSegment: TSegment): boolean;
    function processSegments: boolean;
    function restoreSegment(const aSegment: TSegment): boolean;
    function saveSegments: string;
    function segmentAtCursor: TSegment;
  public
    function clear: boolean;
    function keyHandled(key: WORD): boolean;
    function initTimeline(aMediaFilePath: string; aMax: integer): boolean;
    function undo(const aPrevAction: string): boolean;
    property max: integer read getMax write setMax;
    property mediaFilePath: string read FMediaFilePath;
    property position: integer read getPosition write setPosition;
    function redo: boolean;
    property segCount: integer read getSegCount;
    property segments: TObjectList<TSegment> read FSegments;
    property selSeg:   TSegment read FSelSeg write FSelSeg;
    property timelineHeight: integer read getTimelineHeight;
  end;

function focusTimeline: boolean;
function showTimeline(const Pt: TPoint; const aWidth: integer; const createNew: boolean = TRUE): boolean;
function shutTimeline: boolean;
function TL: TTimeline;

implementation

uses
  progressBar, dialogs, playlist, shellAPI, commonUtils, _debugWindow;

const
  NEARLY_BLACK = clBlack + $101010;

var
  timelineForm: TTimelineForm;
  gTL: TTimeline;

function ctrlKeyDown: boolean;
begin
  result := GetKeyState(VK_CONTROL) < 0;
end;

function execAndWait(const aCmdLine: string): boolean;
var
  ExecInfo: TShellExecuteInfo;
  exitCode: cardinal;
begin
  ZeroMemory(@ExecInfo, SizeOf(ExecInfo));
  with ExecInfo do
  begin
    cbSize := SizeOf(ExecInfo);
    fMask := SEE_MASK_NOCLOSEPROCESS;
    Wnd := 0;
    lpVerb := 'open';
    lpFile := 'ffmpeg';
    lpParameters := PChar(aCmdLine);
    lpDirectory := '';
    nShow := SW_HIDE;
  end;
  result := ShellExecuteEx(@ExecInfo);
  if result then
  begin
    if ExecInfo.hProcess <> 0 then // no handle if the process was activated by DDE
    begin
      repeat
        if MsgWaitForMultipleObjects(1, ExecInfo.hProcess, FALSE, INFINITE, QS_ALLINPUT) = (WAIT_OBJECT_0 + 1) then
          application.processMessages
        else
          BREAK;
      until FALSE;
      getExitCodeProcess(execInfo.hProcess, exitCode);
      result := exitCode = 0;
      CloseHandle(ExecInfo.hProcess);
    end;
  end;
end;

function focusTimeline: boolean;
begin
  case timeLineForm = NIL of TRUE: EXIT; end;
  setForegroundWindow(timelineForm.handle); // so this window also receives keyboard keystrokes
end;

function showTimeline(const Pt: TPoint;  const aWidth: integer; const createNew: boolean = TRUE): boolean;
begin
  case (timelineForm = NIL) and createNew of TRUE: timelineForm := TTimelineForm.create(NIL); end;
  case timelineForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current timeline window. Used for repositioning the window when the main UI moves or resizes.

  timelineForm.width  := aWidth;
  timelineForm.height := 54;

  timelineForm.show;
  winAPI.Windows.setWindowPos(timelineForm.handle, HWND_TOP, Pt.X, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
end;

function shutTimeline: boolean;
begin
  case gTL          <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case timelineForm <> NIL of TRUE: begin timelineForm.free; timelineForm := NIL; end;end;
end;

function TL: TTimeline;
begin
  case gTL = NIL of TRUE: gTL := TTimeline.create; end;
  result := gTL;
end;

var nextColor: integer = 0;
function generateRandomEvenDarkerSoftColor: TColor;
// chatGPT
var
  darkerSoftColors: array of TColor;
begin
  // Define an array of even darker soft colors
  SetLength(darkerSoftColors, 6);
  darkerSoftColors[0] := RGB(80, 80, 80);   // Very Dark Gray
  darkerSoftColors[1] := RGB(70, 70, 70);   // Very Dark Silver
  darkerSoftColors[2] := RGB(60, 60, 60);   // Very Dark Platinum
  darkerSoftColors[3] := RGB(50, 50, 50);   // Very Dark Snow
  darkerSoftColors[4] := RGB(40, 40, 40);   // Very Dark Ivory
  darkerSoftColors[5] := RGB(30, 30, 30);   // Extremely Dark Gray

  result := darkerSoftColors[nextColor];
  inc(nextColor);
  case nextColor > 5 of TRUE: nextColor := 0; end;
end;

{$R *.dfm}

{ TTimelineForm }

procedure TTimelineForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := self.Handle; // normally application.handle
end;

procedure TTimelineForm.pnlCursorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := TRUE;
end;

procedure TTimelineForm.pnlCursorMouseEnter(Sender: TObject);
begin
  screen.cursor := crSizeWE;
end;

procedure TTimelineForm.pnlCursorMouseLeave(Sender: TObject);
begin
  screen.cursor := crDefault;
end;

procedure TTimelineForm.pnlCursorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FDragging then cursorPos := cursorPos + (X - pnlCursor.Width div 2);
  if FDragging then PB.setNewPosition(cursorPos);
end;

procedure TTimelineForm.pnlCursorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := FALSE;
  PB.setNewPosition(cursorPos);
end;

procedure TTimelineForm.setCursorPos(const Value: integer);
begin
  pnlCursor.left := value;
end;

procedure TTimelineForm.FormCreate(Sender: TObject);
begin
  pnlCursor.height := SELF.height;
  pnlCursor.top    := 0;
  pnlCursor.left   := -1;
end;

procedure TTimelineForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  var vSaveUndo := TL.keyHandled(key);
  case key = ord('C') of TRUE: begin TL.cutSegment(TL.segmentAtCursor, TL.position);  TL.drawSegments; end;end;
  case key = ord('R') of TRUE: begin TL.restoreSegment(TL.selSeg);                    TL.drawSegments; end;end;
  case key = ord('X') of TRUE: begin TL.delSegment(TL.selSeg);                        TL.drawSegments; end;end;
  case key = ord('I') of TRUE: begin TL.createInPoint(TL.Position);                   TL.drawSegments; end;end;
  case key = ord('O') of TRUE: begin TL.createOutPoint(TL.Position);                  TL.drawSegments; end;end;
  case key = ord('M') of TRUE: begin TL.mergeRight(TL.selSeg);                        TL.drawSegments; end;end;
  case key = ord('N') of TRUE: begin TL.mergeLeft(TL.selSeg);                         TL.drawSegments; end;end;
  case key = ord('Z') of TRUE: begin TL.undo(FPrevAction);     vSaveUndo := FALSE;    TL.drawSegments; end;end;
  case key = ord('Y') of TRUE: begin TL.redo;                  vSaveUndo := FALSE;    TL.drawSegments; end;end;
  case key = ord('P') of TRUE: TL.processSegments; end;

  var vAction := TL.saveSegments;

  case vSaveUndo AND (vAction <> FPrevAction) of TRUE:  begin
                                                          TL.addUndo(vAction);
                                                          FPrevAction := vAction; end;end;
  case TL.keyHandled(key) of TRUE: key := 0; end;
end;

procedure TTimelineForm.FormResize(Sender: TObject);
begin
  lblPosition.left := (SELF.width div 2) - (lblPosition.width div 2);
  lblPosition.top  := (SELF.height div 2) - (lblPosition.height div 2);
  TL.drawSegments;
end;

function TTimelineForm.getCursorPos: integer;
begin
  result := pnlCursor.left;
end;

{ TTimeline }

function TTimeline.addUndo(aText: string): boolean;
begin
  var vSL := TStringList.create;
  vSL.text := aText;
  FUndoList.push(vSL);

  debug('');
  debug('undo added');
  for var i := 0 to vSL.count - 1 do debug(vSL[i]);
end;

function TTimeline.clear: boolean;
begin
  freeSegments;
end;

function TTimeline.clearFocus: boolean;
begin
  for var vSegment in FSegments do vSegment.selected := FALSE;
  FSelSeg := NIL;
end;

constructor TTimeline.create;
begin
  inherited;
  FSegments             := TObjectList<TSegment>.create;
  FUndoList             := TObjectStack<TStringList>.create;
  FRedoList             := TObjectStack<TStringList>.create;
  FSegments.ownsObjects := TRUE;
  FUndoList.ownsObjects := TRUE;
  FRedoList.ownsObjects := TRUE;
end;

function TTimeline.createInPoint(const aPosition: integer): boolean;
begin
  case segCount = 1 of FALSE: EXIT; end;
  cutSegment(FSegments[0], aPosition, TRUE);
end;

function TTimeline.createOutPoint(const aPosition: integer): boolean;
begin
  case segCount = 0 of  TRUE: EXIT; end;
  case segCount = 1 of  TRUE: cutSegment(FSegments[0], aPosition, FALSE, TRUE);
                       FALSE: case segCount = 2 of TRUE: cutSegment(FSegments[1], aPosition, FALSE, TRUE); end;end;
end;

function TTimeline.cutSegment(const aSegment: TSegment; const aPosition: integer; const deleteLeft: boolean = FALSE; const deleteRight: boolean = FALSE): boolean;
begin
  case aSegment = NIL of TRUE: EXIT; end;

  var newStartSS := aPosition;

  var newSegment := TSegment.create(newStartSS, aSegment.EndSS);
  aSegment.EndSS := newStartSS - 1;


  case newSegment.endSS <= newSegment.startSS of TRUE: debugFormat('seg: %s+, start: %d, end: %d', [aSegment.getSegID, newSegment.startSS, newSegment.endSS]); end;

  case deleteLeft  of TRUE: delSegment(aSegment); end;
  case deleteRight of TRUE: delSegment(newSegment); end;

  case aSegment.isLast of  TRUE: FSegments.add(newSegment);
                          FALSE: FSegments.insert(aSegment.ix + 1, newSegment); end;
end;

function TTimeline.delSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL of TRUE: EXIT; end;
  aSegment.deleted    := TRUE;
  case aSegment.color  = NEARLY_BLACK of FALSE: aSegment.oldColor := aSegment.color; end; // in case user tries to delete an already-deleted segment
  aSegment.color      := NEARLY_BLACK;
end;

destructor TTimeline.destroy;
begin
  freeSegments;
  FSegments.free;
  FUndoList.free;
  FRedoList.free;
  inherited;
end;

function TTimeline.drawSegments: boolean;
begin
  case FMax = 0 of TRUE: begin {showMessage('drawSegments zero max');} EXIT; end;end;
  var n := 1;
  for var vSegment in FSegments do begin
    vSegment.top     := 0;
    vSegment.height  := timelineForm.height;
    vSegment.left    := trunc((vSegment.startSS / FMax) * timelineForm.width);
    vSegment.width   := trunc((vSegment.duration / FMax) * timelineForm.width);
    vSegment.caption := '';
    vSegment.segID   := intToStr(n);
    vSegment.setDisplayDetails;
    vSegment.StyleElements := [];
    VSegment.trashCan.visible := vSegment.deleted;
    case vSegment.deleted of TRUE: begin
                                      vSegment.trashCan.left := (vSegment.width  div 2) - (vSegment.trashCan.width  div 2);
                                      vSegment.trashCan.top  := (vSegment.height div 2) - (vSegment.trashCan.height div 2); end;end;
    vSegment.parent := timelineForm;
    vSegment.invalidate;
    SetWindowPos(vSegment.handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    inc(n);
  end;
  timelineForm.pnlCursor.bringToFront;
end;

function TTimeline.filePathOUT: string;
begin
  result := extractFilePath(FMediaFilePath) + CU.getFileNameWithoutExtension(FMediaFilePath) + ' [edited]' + extractFileExt(FMediaFilePath);
end;

function TTimeline.filePathLOG: string;
begin
  result := changeFileExt(FMediaFilePath, '.log');
end;

function TTimeline.filePathMMP: string;
begin
  result := changeFileExt(FMediaFilePath, '.mmp');
end;

function TTimeline.filePathSEG: string;
begin
  result := changeFileExt(FMediaFilePath, '.seg');
end;

function TTimeline.freeSegments: boolean;
begin
  FSegments.clear;
end;

function TTimeline.getMax: integer;
begin
  result := FMax;
end;

function TTimeline.getPosition: integer;
begin
  result := FPosition;
end;

function TTimeline.getSegCount: integer;
begin
  result := FSegments.count;
end;

function TTimeline.getTimelineHeight: integer;
begin
  result := timelineForm.height;
end;

function TTimeline.defaultSegment: boolean;
begin
  freeSegments;
  FSegments.add(TSegment.create(0, FMax));
  addUndo(format('0-%d,0', [FMax]));
  drawSegments;
end;

function TTimeline.initTimeline(aMediaFilePath: string; aMax: integer): boolean;
begin
  case FMediaFilePath = aMediaFilePath of TRUE: EXIT; end;
  freeSegments;
  FMediaFilePath := aMediaFilePath;
  FMax           := aMax;
  case fileExists(filePathMMP) of  TRUE: loadSegments;
                                  FALSE: defaultSegment; end;
end;

function TTimeline.keyHandled(key: WORD): boolean;
begin
  result := key in [ord('C'), ord('I'), ord('M'), ord('N'), ord('O'), ord('P'), ord('R'), ord('X'), ord('Z')];
end;

function TTimeline.loadSegments(aStringList: TStringList = NIL): boolean;
var
  vSL: TStringList;
  vStartSS: integer;
  vEndSS: integer;
  vDeleted: boolean;
  posHyphen: integer;
  posComma: integer;
begin
  freeSegments;
  vSL := TStringList.create;
  try
    case aStringList <> NIL of  TRUE: vSL.text := aStringList.text;
                               FALSE: vSL.loadFromFile(filePathMMP); end;
    debug('');
    for var i := 0 to vSL.count - 1 do begin
      case trim(vSL[i]) = '' of TRUE: CONTINUE; end;
      debug('loaded ' + vSL[i]);
      posHyphen := pos('-', vSL[i]);
      vStartSS  := strToInt(copy(vSL[i], 1, posHyphen - 1));
      posComma  := pos(',', vSL[i]);
      vEndSS    := strToInt(copy(vSL[i], posHyphen + 1, posComma - posHyphen - 1));
      vDeleted  := copy(vSL[i], posComma + 1, 1) = '1';
      FSegments.add(TSegment.create(vStartSS, vEndss, vDeleted));
    end;

  finally
    vSL.free;
  end;
  drawSegments;
  case aStringList = NIL of TRUE: debugClear; end;
end;

function TTimeline.log(aLogEntry: string): boolean;
begin
  var vLogFile := filePathLOG;
  var vLog := TStringList.create;
  try
    case fileExists(vLogFile) of TRUE: vLog.loadFromFile(vLogFile); end;
    vLog.add(aLogEntry);
    vLog.saveToFile(vLogFile);
  finally
    vLog.free;
  end;
end;

function TTimeline.mergeLeft(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL of TRUE: EXIT; end;
  case aSegment.isFirst of TRUE: EXIT; end;
  var ix := aSegment.ix;
  aSegment.startSS := FSegments[ix - 1].startSS;
  FSegments[ix - 1].color := aSegment.color;
  FSegments.delete(ix - 1);
end;

function TTimeline.mergeRight(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  var ix := aSegment.ix;
  aSegment.endSS := FSegments[ix + 1].endSS;
  FSegments[ix + 1].color := aSegment.color;
  FSegments.delete(ix + 1);
end;

function TTimeline.processSegments: boolean;
var cmdLine: string;
const
  STD_SEG_PARAMS = ' -map 0:0? -c:0 copy -map 0:1? -c:1 copy -map 0:2? -c:2 copy -map 0:3? -c:3 copy -avoid_negative_ts make_zero -map_metadata 0 -movflags +faststart -default_mode infer_no_subs -ignore_unknown';

begin
  case ctrlKeyDown of FALSE: begin
  var vSL := TStringList.create;
  try
    vSL.saveToFile(changeFileExt(FMediaFilePath, '.seg'));
    var n := 1;
    for var vSegment in FSegments do begin
      case vSegment.deleted of TRUE: CONTINUE; end;

      cmdLine := '-hide_banner';
      cmdLine := cmdLine + ' -ss "' + intToStr(vSegment.startSS) + '"';
      cmdLine := cmdLine + ' -i "' + FMediaFilePath + '"';
      cmdLine := cmdLine + ' -t "'  + intToStr(vSegment.duration) + '"';
      cmdLine := cmdLine + STD_SEG_PARAMS;
      var segFile := extractFilePath(FMediaFilePath) + CU.getFileNameWithoutExtension(FMediaFilePath) + format(' seg%.2d', [n]) + extractFileExt(FMediaFilePath);
      cmdLine := cmdLine + ' -y "' + segFile + '"';
      log(cmdLine);

      case execAndWait(cmdLine) of TRUE: vSL.add('file ''' + stringReplace(segFile, '\', '\\', [rfReplaceAll]) + ''''); end;
      inc(n);
    end;
    vSL.saveToFile(filePathSEG);
  finally
    vSL.free;
  end;end;end;

  cmdLine := '-f concat -safe 0 -i "' + changeFileExt(FMediaFilePath, '.seg') + '"';
  cmdLine := cmdLine + ' -map 0:0 -c:0 copy -disposition:0 default -map 0:1? -c:1 copy -disposition:1 default -map 0:2? -c:2 copy -disposition:2 default';
  cmdLine := cmdLine + ' -movflags "+faststart" -default_mode infer_no_subs -ignore_unknown';
  cmdLine := cmdLine + ' -y "' + filePathOUT + '"';
  log(cmdLine);

  result := execAndWait(cmdLine);
end;

function TTimeline.redo: boolean;
begin
  case ctrlKeyDown of FALSE: EXIT; end;
  case FRedoList.count = 0 of TRUE: EXIT; end;

  var vSL1 := FRedoList.peek;
  case vSL1 <> NIL of TRUE: begin loadSegments(vSL1);
                                  var vSL2 := TStringList.create;
                                  vSL2.text := vSL1.text;
                                  FUndoList.push(VSL2); end;end;
  FRedoList.pop;
end;

function TTimeline.restoreSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL of TRUE: EXIT; end;
  aSegment.deleted := FALSE;
  case aSegment.oldColor = NEARLY_BLACK of FALSE: aSegment.color := aSegment.oldColor; end;
end;

function TTimeline.saveSegments: string;
begin
  case TL.segCount = 0 of TRUE: EXIT; end;

  var vSL := TStringList.create;
  try
    for var vSegment in FSegments do
      vSL.add(format('%d-%d,%d', [vSegment.startSS, vSegment.endSS, integer(vSegment.deleted)]));

    vSL.saveToFile(filePathMMP);
    result := vSL.text;
  finally
    vSL.free;
  end;

  case (TL.segCount = 1) AND (TL.segments[0].startSS = 0) AND (TL.segments[0].endSS = TL.max) AND fileExists(filePathMMP) of TRUE: deleteFile(filePathMMP); end;
end;

function TTimeline.segmentAtCursor: TSegment;
begin
  result := NIL;
  for var vSegment in FSegments do
    case (vSegment.left <= timelineForm.cursorPos) and (vSegment.left + vSegment.width >= timelineForm.cursorPos) of TRUE: result := vSegment; end;
end;

procedure TTimeline.setMax(const Value: integer);
begin
  FMax := value;
end;

procedure TTimeline.setPosition(const Value: integer);
begin
  FPosition := value;
  case (FPosition = 0) OR (FMax = 0) of TRUE: EXIT; end;
  timelineForm.pnlCursor.left := trunc((FPosition / FMax) * timelineForm.width) - timelineForm.pnlCursor.width;
  timelineForm.lblPosition.caption  := CU.formatTime(FPosition);
end;

function TTimeline.undo(const aPrevAction: string): boolean;
begin
  case ctrlKeyDown of FALSE: EXIT; end;
  case FUndoList.count = 0 of TRUE: EXIT; end;

  var vSL1 := FUndoList.peek;
  case (vSL1 <> NIL) and (vSL1.text = aPrevAction) of TRUE: begin
                                                              var vSL2 := TStringList.create;
                                                              vSL2.text := vSL1.text;
                                                              FUndoList.pop;
                                                              FRedoList.push(vSL2);
                                                              case FUndoList.count = 0 of TRUE: EXIT; end;end;end;

  var vSL3 := FUndoList.peek;
  case vSL3 <> NIL of TRUE:  begin
                              loadSegments(vSL3);
                              var vSL4 := TStringList.create;
                              vSL4.text := vSL3.text;
                              FRedoList.push(vSL4); end;end;
  FUndoList.pop;
end;

{ TSegment }

procedure CopyPNGImage(SourceImage, DestImage: TImage);
begin
  // Check if the source image has a picture to copy
  if Assigned(SourceImage.Picture) and Assigned(SourceImage.Picture.Graphic) then
  begin
    // Clear the destination image
    DestImage.Picture := nil;

    // Assign the graphic content from the source to the destination
    DestImage.Picture.Assign(SourceImage.Picture.Graphic);
  end;
end;

procedure TSegment.doClick(Sender: TObject);
begin
  TL.clearFocus;
  TL.selSeg  := SELF;
  selected   := TRUE;
end;

constructor TSegment.create(const aStartSS: integer; const aEndSS: integer; const aDeleted: boolean = FALSE);
begin
  inherited create(NIL);
  parent            := timelineForm;
  height            := timelineForm.height;
  font.color        := clSilver;
  font.size         := 10;
  font.style        := [fsBold];
  alignment         := taLeftJustify;
  onClick           := doClick;
  doubleBuffered    := TRUE;

  startSS           := aStartSS;
  endSS             := aEndSS;
  borderStyle       := bsNone;
  bevelOuter        := bvNone;
  color             := generateRandomEvenDarkerSoftColor;
  oldColor          := color;

  FSegID            := TLabel.create(SELF);
  FSegID.parent     := SELF;
  FSegID.top        := 0;
  FSegID.left       := 4;
  FSegID.styleElements := [];

  FSegDetails := TLabel.create(SELF);
  FSegDetails.parent     := SELF;
  FSegDetails.top        := 38;
  FSegDetails.left       := 4;
  FSegDetails.styleElements := [];

  FTrashCan := TImage.create(SELF);
  FTrashCan.parent := SELF;
  FTrashCan.stretch := TRUE;
  FTrashCan.center  := TRUE;
  FTrashCan.height  := 31;
  FTrashCan.width   := 41;
  FTrashCan.visible := FALSE;
  FTrashCan.onClick := doClick;
  CopyPNGImage(timelineForm.imgTrashCan, FTrashCan);

  case aDeleted of TRUE: TL.delSegment(SELF); end;
end;

function TSegment.getDuration: integer;
begin
  result := FEndSS - FStartSS;
end;

function TSegment.getIx: integer;
begin
  result := TL.segments.indexOf(SELF);
end;

function TSegment.getIsFirst: boolean;
begin
  result := ix = 0;
end;

function TSegment.getIsLast: boolean;
begin
  result := ix = TL.segCount - 1;
end;

function TSegment.getSegID: string;
begin
  result := FSegID.caption;
end;

procedure TSegment.paint;
begin
  var rect := getClientRect;

  canvas.brush.color := color;

  canvas.fillRect(rect);

  case selected of  TRUE: Frame3D(canvas, rect, clTeal, clTeal, 1);
                   FALSE: Frame3D(canvas, rect, color, color, 1); end;
end;

procedure TSegment.setDisplayDetails;
begin
  FSegDetails.caption := format('%ds - %ds', [startSS, endSS]);
end;

procedure TSegment.setSegID(const Value: string);
begin
  FSegID.caption := value;
end;

procedure TSegment.setSelected(const Value: boolean);
begin
  FSelected := Value;
  invalidate;
end;

initialization
  timelineForm := NIL;
  gTL          := NIL;

finalization
  case gTL          <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case timelineForm <> NIL of TRUE: begin timelineForm.close; timelineForm.free; timelineForm := NIL; end;end;

end.
