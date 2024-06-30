{   MMP: Minimalist Media Player
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
unit formTimeline;

interface

uses
  winApi.messages, winApi.Windows,
  system.classes, system.generics.collections, system.sysUtils, system.variants,
  vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls,
  formProgress,
  TSegmentClass;

type
  TRunType = (rtFFMpeg, rtCMD);

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
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormResize(Sender: TObject);
    procedure lblPositionClick(Sender: TObject);
  strict private
    FDragging: boolean;
  private
    function getCursorPos: integer;
    procedure setCursorPos(const Value: integer);
    function updatePositionDisplay: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams);
    procedure exportSegments(sender: TObject);
  public
    property cursorPos: integer read getCursorPos write setCursorPos;
  end;

  TTimeline = class(TObject)
  strict private
    FLengthenCount: integer;
    FMax: integer;
    FMediaFilePath: string;
    FPosition: integer;
    FPrevAction: string;
    FShortenCount: integer;
    FUndoList: TObjectStack<TStringList>;
    FRedoList: TObjectStack<TStringList>;
    function getMax: integer;
    function getPosition: integer;
    procedure setMax(const Value: integer);
    procedure setPosition(const Value: integer);
  private
    constructor create;
    destructor  Destroy; override;
    function addUndo(const aAction: string): string;
    function cutSegment(const aSegment: TSegment; const aPosition: integer; const deleteLeft: boolean = FALSE; const deleteRight: boolean = FALSE): boolean;
    function defaultSegment: string;
    function drawSegments: boolean;
    function filePathLOG: string;
    function filePathMMP: string;
    function filePathOUT: string;
    function filePathSEG: string;
    function getSegCount: integer;
    function lengthenSegment(const aSegment: TSegment): boolean;
    function loadSegments(const aStringList: TStringList = NIL; const includeTitles: boolean = FALSE): string;
    function log(const aLogEntry: string): boolean;
    function mergeLeft(const aSegment: TSegment): boolean;
    function mergeRight(const aSegment: TSegment): boolean;
    function restoreSegment(const aSegment: TSegment): boolean;
    function saveSegments: string;
    function segFileEntry(const aSegFile: string): string;
    function segmentAtCursor: TSegment;
    function shortenSegment(const aSegment: TSegment): boolean;
    function exportFail(const aProgressForm: TProgressForm; const aSegID: string = ''): TModalResult;
    function getSegments: TObjectList<TSegment>;
    procedure onCancelButton(sender: TObject);
    function loadChapters: TStringList;
  protected
    function exportSegments: boolean;
  public
    function clear: boolean;
    function delSegment(const aSegment: TSegment): boolean;
    function initTimeline(const aMediaFilePath: string; const aMax: integer): string;
    function redo: boolean;
    function undo(const aPrevAction: string): boolean;
    function validKey(key: WORD): boolean;
    property lengthenCount: integer               read FLengthenCount write FLengthenCount;
    property max:           integer               read getMax         write setMax;
    property mediaFilePath: string                read FMediaFilePath;
    property position:      integer               read getPosition    write setPosition;
    property prevAction:    string                read FPrevAction    write FPrevAction;
    property segCount:      integer               read getSegCount;
    property segments:      TObjectList<TSegment> read getSegments;
    property shortenCount:  integer               read FShortenCount  write FShortenCount;
  end;

function focusTimeline: boolean;
function showTimeline(const Pt: TPoint; const aWidth: integer; const createNew: boolean = TRUE): boolean;
function shutTimeline: boolean;
function TL: TTimeline;

implementation

uses
  winApi.shellApi,
  vcl.dialogs,
  mmpFileUtils, mmpImageUtils, mmpMPVFormatting, mmpSingletons, mmpUtils,
  formStreamList,
  _debugWindow;

var
  timelineForm: TTimelineForm;
  gTL: TTimeline;
  gCancelled: boolean;

function debugSL(aText: string; aSL: TStringList): boolean;
begin
  EXIT;
  debug('');
  debug(aText);
  for var i := aSL.count - 1 downto 0 do debug(aSL[i]);
end;

function ctrlKeyDown: boolean;
begin
  result := GetKeyState(VK_CONTROL) < 0;
end;

function execAndWait(const aCmdLine: string; const aRunType: TRunType = rtFFMpeg): boolean;
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

    case aRunType of
      rtFFMPeg: lpFile := 'ffmpeg';
      rtCMD:    lpFile := 'cmd';
    end;

    case aRunType of
      rtFFMpeg: lpParameters := PChar(aCmdLine);
      rtCMD:    lpParameters := PChar(' /K ffmpeg ' + aCmdLine);
    end;

    lpDirectory := PWideChar(extractFilePath(ParamStr(0)));

    case aRunType of
      rtFFMpeg: nShow := SW_HIDE;
      rtCMD:    nShow := SW_SHOW;
    end;

  end;

  result := ShellExecuteEx(@ExecInfo);

  case result AND (ExecInfo.hProcess <> 0) of TRUE: begin // no handle if the process was activated by DDE
                                                      case aRunType of
                                                        rtFFMpeg: begin
                                                                    repeat
                                                                      case MsgWaitForMultipleObjects(1, ExecInfo.hProcess, FALSE, INFINITE, QS_ALLINPUT) = (WAIT_OBJECT_0 + 1) of  TRUE: mmpProcessMessages;
                                                                                                                                                                                  FALSE: BREAK; end;
                                                                    until gCancelled;
                                                                    getExitCodeProcess(execInfo.hProcess, exitCode);
                                                                    result := exitCode = 0;
                                                                  end;
                                                        rtCMD: result := TRUE;
                                                      end;
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

  TSegment.parentForm := timelineForm;

  timelineForm.width  := aWidth;
  timelineForm.height := DEFAULT_SEGMENT_HEIGHT;

  timelineForm.show;
  winAPI.Windows.setWindowPos(timelineForm.handle, HWND_TOP, Pt.X, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);

  showStreamList(point(pt.x + timelineForm.width, pt.y), aWidth, timelineForm.exportSegments, createNew);
  GV.showingTimeline := TRUE;
  GV.timelineHeight  := timelineForm.height + 10;
end;

function shutTimeline: boolean;
begin
  shutStreamList;
  case gTL          <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case timelineForm <> NIL of TRUE: begin timelineForm.free; timelineForm := NIL; end;end;
  GV.showingTimeline := FALSE;
  GV.timelineHeight  := 0;
end;

function TL: TTimeline;
begin
  case gTL = NIL of TRUE: gTL := TTimeline.create; end;
  result := gTL;
end;

{$R *.dfm}

{ TTimelineForm }

procedure TTimelineForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for this window
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := SELF.Handle; // normally application.handle
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

procedure TTimelineForm.exportSegments(sender: TObject);
begin
  TL.exportSegments;
end;

procedure TTimelineForm.FormCreate(Sender: TObject);
begin
  pnlCursor.height := SELF.height;
  pnlCursor.top    := 0;
  pnlCursor.left   := -1;
  keyPreview       := TRUE;
end;

procedure TTimelineForm.FormKeyPress(Sender: TObject; var Key: Char);
// e.g. key:char here may be x or X, but keyUp:word will always be 88
begin
  key := upCase(key);
  case key in ['L', 'S'] of FALSE: EXIT; end; // ignore irrelevant keystrokes - let main window have them

  case key in ['L'] of TRUE: begin TL.lengthenCount := TL.lengthenCount + 1; TL.lengthenSegment(TSegment.selSeg); TL.drawSegments; end;end;
  case key in ['S'] of TRUE: begin TL.shortenCount  := TL.shortenCount  + 1; TL.shortenSegment(TSegment.selSeg);  TL.drawSegments; end;end;

  var vAction := TL.saveSegments;
  TL.drawSegments;

  case (TL.lengthenCount = 1) OR (TL.shortenCount = 1) of TRUE: TL.addUndo(vAction); end;
end;

procedure TTimelineForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
// For VK_ keys we only get a formKeyDown event
var
  vOK: boolean;
  vAction: string;
begin
  case TL.validKey(key) of FALSE: EXIT; end; // ignore irrelevant keystrokes - let main window have them

  vOK := FALSE;
  vAction := '';

  case key = ord('C') of TRUE: begin vOK := TL.cutSegment(TL.segmentAtCursor, TL.position);              TL.drawSegments; end;end;
  case key = ord('R') of TRUE: begin vOK := TL.restoreSegment(TSegment.selSeg);                          TL.drawSegments; end;end;
  case key = ord('X') of TRUE: begin vOK := TL.delSegment(TSegment.selSeg);                              TL.drawSegments; end;end;
  case key = ord('I') of TRUE: begin vOK := TL.cutSegment(TL.segmentAtCursor, TL.position, TRUE);        TL.drawSegments; end;end;
  case key = ord('O') of TRUE: begin vOK := TL.cutSegment(TL.segmentAtCursor, TL.position, FALSE, TRUE); TL.drawSegments; end;end;
  case key = ord('M') of TRUE: begin vOK := TL.mergeRight(TSegment.selSeg);                              TL.drawSegments; end;end;
  case key = ord('N') of TRUE: begin vOK := TL.mergeLeft(TSegment.selSeg);                               TL.drawSegments; end;end;

  case key = ord('L') of TRUE: begin vOK := TRUE; TL.lengthenCount := 0; end;end;  // user has stopped holding down L
  case key = ord('S') of TRUE: begin vOK := TRUE; TL.shortenCount  := 0; end;end;  // user has stopped holding down S

  var vSaveUndo := vOK; // a change was made

  case ctrlKeyDown AND (key = ord('Z')) of TRUE: begin TL.undo(TL.prevAction);   vSaveUndo := FALSE;    TL.drawSegments; end;end; // Ctrl-Z
  case ctrlKeyDown AND (key = ord('Y')) of TRUE: begin TL.redo;                  vSaveUndo := FALSE;    TL.drawSegments; end;end; // Ctrl-Y

  case vSaveUndo of TRUE: begin
                            vAction := TL.saveSegments;
                            TL.drawSegments;
                            TL.addUndo(vAction); end;end;

  case TL.validKey(key) of TRUE: key := 0; end; // trap the key if we did something with it
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

procedure TTimelineForm.lblPositionClick(Sender: TObject);
begin
  case lblPosition.tag = 0 of  TRUE: lblPosition.tag := 1;
                              FALSE: lblPosition.tag := 0; end;
  updatePositionDisplay;
end;

function TTimelineForm.updatePositionDisplay: boolean;
begin
  case lblPosition.tag = 0 of  TRUE: timelineForm.lblPosition.caption  := intToStr(TL.Position) + 's';
                              FALSE: timelineForm.lblPosition.caption  := mmpFormatTime(TL.Position); end;
end;

{ TTimeline }

function TTimeline.addUndo(const aAction: string): string;
begin
  case aAction = FPrevAction of TRUE: EXIT; end;

  FPrevAction := aAction;

  var vSL := TStringList.create;
  vSL.text := aAction;
  FUndoList.push(vSL);
  result := aAction;
  debugSL('add undo', vSL);
end;

function TTimeline.clear: boolean;
begin
  segments.clear;
end;

constructor TTimeline.create;
begin
  inherited;
  FUndoList             := TObjectStack<TStringList>.create;
  FRedoList             := TObjectStack<TStringList>.create;
  FUndoList.ownsObjects := TRUE;
  FRedoList.ownsObjects := TRUE;
end;

function TTimeline.cutSegment(const aSegment: TSegment; const aPosition: integer; const deleteLeft: boolean = FALSE; const deleteRight: boolean = FALSE): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;

  var newStartSS := aPosition;

  var newSegment := TSegment.create(newStartSS, aSegment.EndSS);
  aSegment.EndSS := newStartSS - 1;

  case newSegment.endSS <= newSegment.startSS of TRUE: debugFormat('seg: %s+, start: %d, end: %d', [aSegment.SegID, newSegment.startSS, newSegment.endSS]); end;

  case ctrlKeyDown of TRUE: delSegment(aSegment); end;
  case deleteLeft  of TRUE: delSegment(aSegment); end;
  case deleteRight of TRUE: delSegment(newSegment); end;

  case aSegment.isLast of  TRUE: segments.add(newSegment);
                          FALSE: segments.insert(aSegment.ix + 1, newSegment); end;
  result := TRUE;
end;

function TTimeline.delSegment(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  result := aSegment.delete;
end;

destructor TTimeline.Destroy;
begin
  segments.clear;
  FUndoList.free;
  FRedoList.free;
  inherited;
end;

function TTimeline.drawSegments: boolean;
begin
  case FMax = 0 of TRUE: EXIT; end;
  var n := 1;
  for var vSegment in segments do begin
    vSegment.top     := 0;
    vSegment.height  := timelineForm.height;
    vSegment.left    := trunc((vSegment.startSS / FMax) * timelineForm.width);
    vSegment.width   := trunc((vSegment.duration / FMax) * timelineForm.width);
    vSegment.caption := '';
    vSegment.segID   := format('%.2d', [n]);
    vSegment.setDisplayDetails;
    vSegment.StyleElements := [];
    mmpCopyPNGImage(timelineForm.imgTrashCan, vSegment.trashCan);

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
  applySegments(TL.segments);
end;

function TTimeline.filePathOUT: string;
begin
  result := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + ' [edited]' + extractFileExt(FMediaFilePath);
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
  result := segments.count;
end;

function TTimeline.getSegments: TObjectList<TSegment>;
begin
  result := TSegment.segments;
end;

function TTimeline.defaultSegment: string;
begin
  segments.clear;
  segments.add(TSegment.create(0, FMax));
  result := format('0-%d,0', [FMax]);
end;

function TTimeLine.exportFail(const aProgressForm: TProgressForm; const aSegID: string = ''): TModalResult;
begin
  case aSegID = '' of  TRUE: aProgressForm.subHeading.caption := 'Concatenation failed';
                      FALSE: aProgressForm.subHeading.caption := format('Export of seg%s failed', [aSegID]); end;

  aProgressForm.modal := TRUE;

  aProgressForm.hide;
  result := aProgressForm.showModal;

  aProgressForm.modal := FALSE;
  aProgressForm.show; // reshow non-modally after the showModal
end;

function TTimeline.exportSegments: boolean;
const
  STD_SEG_PARAMS = ' -avoid_negative_ts make_zero -map_metadata 0 -movflags +faststart -default_mode infer_no_subs -ignore_unknown';
var
  cmdLine:    string;
  vMaps:      string;
  vID:        integer;
  vSegOneFN:  string;
begin
  result     := TRUE;
  gCancelled := FALSE;

  vSegOneFN  := '';

  var vProgressForm := TProgressForm.create(NIL);
  vProgressForm.onCancel := onCancelButton;
  var vS1 := ''; case segments.count   > 1 of  TRUE: vS1 := 's'; end; // it bugs me that so many programmers don't bother to do this! :D
  var vS2 := ''; case MI.selectedCount > 1 of  TRUE: vS2 := 's'; end;
  vProgressForm.heading.caption := format('Exporting %d segment%s (%d stream%s)', [TSegment.includedCount, vS1, MI.selectedCount, vS2]);
  vProgressForm.show;

  // export segments
  try
    case ctrlKeyDown of FALSE: begin
      var vSL := TStringList.create;
      try
        vSL.saveToFile(filePathSEG); // clear previous contents
        for var vSegment in segments do begin
          case vSegment.deleted of TRUE: CONTINUE; end;

          cmdLine := '-hide_banner';

          cmdLine := cmdLine + ' -ss "' + intToStr(vSegment.startSS) + '"';
          cmdLine := cmdLine + ' -i "' + FMediaFilePath + '"';
          cmdLine := cmdLine + ' -t "'  + intToStr(vSegment.duration) + '"';

          vMaps := '';
          for var vMediaStream in MI.mediaStreams do
            case vMediaStream.selected of TRUE: begin
                                                  case tryStrToInt(vMediaStream.ID, vID) of FALSE: vID := 0; end;
                                                  case MI.lowestID = 1 of TRUE: vID := vID - 1; end;
                                                  vMaps := vMaps + format(' -map 0:%d ', [vID]);
                                                end;end;
          vMaps := vMaps + ' -c copy';
          cmdLine := cmdLine + vMaps;
          cmdLine := cmdLine + STD_SEG_PARAMS;
          var segFile := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + ' seg' + vSegment.segID + extractFileExt(FMediaFilePath);
          case TSegment.includedCount = 1 of TRUE: vSegOneFN := segFile; end;
          cmdLine := cmdLine + ' -y "' + segFile + '"';
          log(cmdLine);

          vProgressForm.subHeading.caption := extractFileName(segFile);

          case execAndWait(cmdLine) of  TRUE: vSL.add(segFileEntry(segFile));
                                       FALSE: begin
                                                result := FALSE;
                                                case exportFail(vProgressForm, vSegment.segID) = mrYes of TRUE: begin
                                                                                                                  vSL.add(segFileEntry(segFile)); // the user will correct the export
                                                                                                                  execAndWait(cmdLine, rtCMD); end;end;
                                              end;
          end;
        end;
        vSL.saveToFile(filePathSEG);
      finally
        vSL.free;
      end;end;end;

    case result of FALSE: EXIT; end;

    case vSegOneFN <> '' of TRUE: begin renameFile(vSegOneFN, filePathOUT); EXIT; end;end;

  // concatenate exported segments
  vProgressForm.subHeading.caption := 'Joining segments';
  cmdLine := '-f concat -safe 0 -i "' + changeFileExt(FMediaFilePath, '.seg') + '"';
  for var i := 0 to MI.selectedCount - 1 do
    cmdLine := cmdLine + format(' -map 0:%d -c:%d copy -disposition:%d default', [i, i, i]);
  cmdLine := cmdLine + STD_SEG_PARAMS;
  cmdLine := cmdLine + ' -y "' + filePathOUT + '"';
  log(cmdLine);

  result := execAndWait(cmdLine);
  case result of FALSE: case exportFail(vProgressForm) = mrYes of TRUE: execAndWait(cmdLine, rtCMD); end;end;

  finally
    vProgressForm.free;
  end;
end;

function TTimeline.initTimeline(const aMediaFilePath: string; const aMax: integer): string;
begin
  case FMediaFilePath = aMediaFilePath of TRUE: EXIT; end;
  segments.clear; FUndoList.clear; FRedoList.clear;
  refreshStreamInfo(aMediaFilePath);
  FMediaFilePath := aMediaFilePath;
  FMax           := aMax;
  case fileExists(filePathMMP) of  TRUE: result := addUndo(loadSegments);
                                  FALSE: begin
                                           var vSL := loadChapters;
                                           case vSL <> NIL of  TRUE: begin result := addUndo(loadSegments(vSL, TRUE)); vSL.free; end;
                                                              FALSE:       result := addUndo(defaultSegment); end;end;end;

  drawSegments;
end;

function TTimeline.loadSegments(const aStringList: TStringList = NIL; const includeTitles: boolean = FALSE): string;
var
  vSL: TStringList;
  vStartSS: integer;
  vEndSS: integer;
  vDeleted: boolean;
  posHyphen: integer;
  posComma: integer;
begin
  segments.clear;
  vSL := TStringList.create;
  try
    case aStringList <> NIL of  TRUE: vSL.text := aStringList.text;
                               FALSE: vSL.loadFromFile(filePathMMP); end;

    for var i := 0 to vSL.count - 1 do begin
      case trim(vSL[i]) = '' of TRUE: CONTINUE; end;
      posHyphen := pos('-', vSL[i]);
      vStartSS  := strToInt(copy(vSL[i], 1, posHyphen - 1));
      posComma  := pos(',', vSL[i]);
      vEndSS    := strToInt(copy(vSL[i], posHyphen + 1, posComma - posHyphen - 1));
      vDeleted  := copy(vSL[i], posComma + 1, 1) = '1';
      var ix := segments.add(TSegment.create(vStartSS, vEndss, vDeleted));
      case includeTitles of TRUE: segments[ix].title := MI.mediaChapters[ix].chapterTitle; end;
      result    := result + format('%d-%d,%d', [vStartSS, vEndSS, integer(vDeleted)]) + #13#10;
    end;
  finally
    vSL.free;
  end;
end;

function TTimeline.lengthenSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL  of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  aSegment.endSS := aSegment.endSS + 1;
  segments[aSegment.ix + 1].startSS := segments[aSegment.ix + 1].startSS + 1;
end;

function TTimeLine.loadChapters: TStringList;
begin
  result := NIL;
  case MI.chapterCount = 0 of TRUE: EXIT; end;

  result := TStringList.create;

  for var i := 0 to MI.chapterCount - 1 do
    result.add(format('%d-%d,0', [MI.mediaChapters[i].chapterStartSS, MI.mediaChapters[i].chapterEndSS]));
end;

function TTimeline.log(const aLogEntry: string): boolean;
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
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  case aSegment.isFirst of TRUE: EXIT; end;
  var ix := aSegment.ix;
  aSegment.startSS := segments[ix - 1].startSS;
  segments[ix - 1].color := aSegment.color;
  segments.delete(ix - 1);
  result := TRUE;
end;

function TTimeline.mergeRight(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  var ix := aSegment.ix;
  aSegment.endSS := segments[ix + 1].endSS;
  segments[ix + 1].color := aSegment.color;
  segments.delete(ix + 1);
  result := TRUE;
end;

procedure TTimeline.onCancelButton(sender: TObject);
begin
  gCancelled := TRUE;
end;

function TTimeline.redo: boolean;
begin
  case FRedoList.count = 0 of TRUE: EXIT; end;

  case FRedoList.peek <> NIL of TRUE: begin
                                        debugSL('redo', FRedoList.peek);
                                        loadSegments(FRedoList.peek);
                                        saveSegments;
                                        drawSegments;
                                        FUndoList.push(FRedoList.extract);
                                      end;end;
end;

function TTimeline.restoreSegment(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  aSegment.deleted := FALSE;
  case aSegment.oldColor = NEARLY_BLACK of FALSE: aSegment.color := aSegment.oldColor; end;
  result := TRUE;
end;

function TTimeline.saveSegments: string;
begin
  case TL.segCount = 0 of TRUE: EXIT; end;

  var vSL := TStringList.create;
  try
    for var vSegment in segments do
      vSL.add(format('%d-%d,%d', [vSegment.startSS, vSegment.endSS, integer(vSegment.deleted)]));

    vSL.saveToFile(filePathMMP);
    result := vSL.text;
  finally
    vSL.free;
  end;

  case (TL.segCount = 1) AND (segments[0].startSS = 0) AND (segments[0].endSS = TL.max) AND fileExists(filePathMMP) of TRUE: deleteFile(filePathMMP); end;
end;

function TTimeline.segFileEntry(const aSegFile: string): string;
begin
  result := 'file ''' + stringReplace(aSegFile, '\', '\\', [rfReplaceAll]) + '''';
end;

function TTimeline.segmentAtCursor: TSegment;
begin
  result := NIL;
  for var vSegment in segments do
    case (timelineForm.cursorPos >= vSegment.left) and (timelineForm.cursorPos <= vSegment.left + vSegment.width) of TRUE: result := vSegment; end;
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
  timelineForm.updatePositionDisplay;
end;

function TTimeline.shortenSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL  of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  aSegment.endSS := aSegment.endSS - 1;
  segments[aSegment.ix + 1].startSS := segments[aSegment.ix + 1].startSS - 1;
end;

function TTimeline.undo(const aPrevAction: string): boolean;
// discard the most recent action and apply the subsequent actions from the stack
begin
  case FUndoList.count = 0 of TRUE: EXIT; end;

  case (FUndoList.peek <> NIL) and (FUndoList.peek.text = aPrevAction) of TRUE: begin
                                                                                  debugSL('pop top', FUndoList.peek);
                                                                                  FRedoList.push(FUndoList.extract);
                                                                                end;end;

  case FUndoList.count = 0 of TRUE: EXIT; end;

  case FUndoList.peek <> NIL of TRUE: begin
                                        debugSL('undo pop', FUndoList.peek);
                                        loadSegments(FUndoList.peek);
                                        saveSegments;
                                        drawSegments;
                                        FRedoList.push(FUndoList.extract);
                                      end;end;
end;

function TTimeline.validKey(key: WORD): boolean;
const validKeys = 'CILMNORSXYZ';
begin
  result := validKeys.contains(char(key));
end;

initialization
  timelineForm := NIL;
  gTL          := NIL;

finalization
  case gTL          <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case timelineForm <> NIL of TRUE: begin timelineForm.close; timelineForm.free; timelineForm := NIL; end;end;

end.
