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
unit view.mmpFormTimeline;

interface

uses
  winApi.messages, winApi.Windows,
  system.classes, system.generics.collections, system.sysUtils, system.variants,
  vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  view.mmpFormProgress,
  TSegmentClass;

type
  TRunType = (rtFFMpeg, rtCMD);

  TTimelineForm = class(TForm)
    pnlCursor:          TPanel;
    lblPosition:        TPanel;
    imgTrashCan:        TImage;
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
    function  getCursorPos: integer;
    procedure setCursorPos(const Value: integer);
    function  updatePositionDisplay(const aPosition: integer = -1): boolean;
  protected
    procedure createParams(var params: TCreateParams);
    procedure exportSegments(sender: TObject);
  public
    property  cursorPos: integer read getCursorPos write setCursorPos;
  end;

  TTimeline = class(TObject)
  strict private
    FCancelled:               boolean;
    FLengthenCount:           integer;
    FMax:                     integer;
    FMediaFilePath:           string;
    FPosition:                integer;
    FPrevAction:              string;
    FShortenCount:            integer;
    FUndoList:                TObjectStack<TStringList>;
    FRedoList:                TObjectStack<TStringList>;
    FSubscriber:              ISubscriber;
  private
    constructor create;
    destructor  Destroy; override;

    function    getMax:       integer;
    function    getPosition:  integer;
    function    getSegCount:  integer;
    function    getSegments:  TObjectList<TSegment>;

    procedure   setMax(const Value: integer);
    procedure   setPosition(const Value: integer);

    function    addUndo(const aAction: string): string;
    function    cutSegment(const aSegment: TSegment; const aPosition: integer; const bDeleteLeft: boolean = FALSE; const bDeleteRight: boolean = FALSE): boolean;
    function    defaultSegment: string;
    function    drawSegments: boolean;
    function    exportFail(const aProgressForm: TProgressForm; const aSegID: string = ''): TModalResult;
    function    filePathLOG: string;
    function    filePathMMP: string;
    function    filePathOUT: string;
    function    filePathSEG: string;
    function    lengthenSegment(const aSegment: TSegment): boolean;
    function    loadChapters: TStringList;
    function    loadSegments(const aStringList: TStringList = NIL; const includeTitles: boolean = FALSE): string;
    function    log(const aLogEntry: string): boolean;
    function    mergeLeft(const aSegment: TSegment): boolean;
    function    mergeRight(const aSegment: TSegment): boolean;
    function    restoreSegment(const aSegment: TSegment): boolean;
    function    saveSegments: string;
    function    segFileEntry(const aSegFile: string): string;
    function    segmentAt(const aCursorPos: integer): TSegment;
    function    shortenSegment(const aSegment: TSegment): boolean;
  protected
    function    exportSegments: boolean;
    procedure   onCancelButton(sender: TObject);
    function    onNotify(const aNotice: INotice): INotice;
  public
    function    clear:          boolean;
    function    delSegment(const aSegment: TSegment): boolean;
    function    initTimeline(const aMediaFilePath: string; const aMax: integer): string;
    function    redo:           boolean;
    function    undo(const aPrevAction: string):  boolean;
    function    validKey(key: WORD):              boolean;

    property    cancelled:      boolean               read FCancelled;
    property    lengthenCount:  integer               read FLengthenCount write FLengthenCount;
    property    max:            integer               read getMax         write setMax;
    property    mediaFilePath:  string                read FMediaFilePath;
    property    position:       integer               read getPosition    write setPosition;
    property    prevAction:     string                read FPrevAction    write FPrevAction;
    property    segCount:       integer               read getSegCount;
    property    segments:       TObjectList<TSegment> read getSegments;
    property    shortenCount:   integer               read FShortenCount  write FShortenCount;
  end;

function focusTimeline: boolean;
function showTimeline(const aPt: TPoint; const aWidth: integer; const bCreateNew: boolean = TRUE): boolean;
function shutTimeline: boolean;
function TL: TTimeline;

implementation

uses
  winApi.shellApi,
  vcl.dialogs,
  mmpFileUtils, mmpFormatting, mmpDoProcs, mmpGlobalState, mmpImageUtils, mmpKeyboardUtils, mmpUtils,
  view.mmpFormStreamList,
  model.mmpMediaInfo,
  _debugWindow;

function debugSL(aText: string; aSL: TStringList): boolean;
begin
  EXIT;
  debug('');
  debug(aText);
  for var i := aSL.count - 1 downto 0 do debug(aSL[i]);
end;

function execAndWait(const aCmdLine: string; const aRunType: TRunType = rtFFMpeg): boolean;
// rtFFMPeg: normal running - the user just gets to see progress dialogs
// rtCMD: a segment export failed, so we rerun showing the cmd prompt box so the user can view the FFMPeg error messages
var
  vExecInfo: TShellExecuteInfo;
  vExitCode: cardinal;
begin
  zeroMemory(@vExecInfo, SizeOf(vExecInfo));
  with vExecInfo do
  begin
    cbSize  := sizeOf(vExecInfo);
    fMask   := SEE_MASK_NOCLOSEPROCESS;
    Wnd     := 0;
    lpVerb  := 'open';

    case aRunType of
      rtFFMPeg: lpFile := 'ffmpeg';
      rtCMD:    lpFile := 'cmd';
    end;

    case aRunType of
      rtFFMpeg: lpParameters := pChar(aCmdLine);
      rtCMD:    lpParameters := pChar(' /K ffmpeg ' + aCmdLine);
    end;

    lpDirectory := pWideChar(mmpExePath);

    case aRunType of
      rtFFMpeg: nShow := SW_HIDE;
      rtCMD:    nShow := SW_SHOW;
    end;

  end;

  result := ShellExecuteEx(@vExecInfo);

  case result AND (vExecInfo.hProcess <> 0) of TRUE: begin // no handle if the process was activated by DDE
                                                      case aRunType of
                                                        rtFFMpeg: begin
                                                                    repeat
                                                                      case msgWaitForMultipleObjects(1, vExecInfo.hProcess, FALSE, INFINITE, QS_ALLINPUT) = (WAIT_OBJECT_0 + 1) of   TRUE: mmpProcessMessages;
                                                                                                                                                                                    FALSE: BREAK; end;
                                                                    until TL.cancelled;
                                                                    getExitCodeProcess(vExecInfo.hProcess, vExitCode);
                                                                    result := vExitCode = 0;
                                                                  end;
                                                        rtCMD: result := TRUE;
                                                      end;
                                                      closeHandle(vExecInfo.hProcess);
                                                    end;
  end;
end;

var gTimelineForm: TTimelineForm = NIL;
function focusTimeline: boolean;
begin
  case gTimeLineForm = NIL of TRUE: EXIT; end;
  setForegroundWindow(gTimelineForm.handle); // so this window also receives keyboard keystrokes
end;

function showTimeline(const aPt: TPoint; const aWidth: integer; const bCreateNew: boolean = TRUE): boolean;
begin
  case (gTimelineForm = NIL) and bCreateNew of TRUE: gTimelineForm := TTimelineForm.create(NIL); end;
  case gTimelineForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current timeline window. Used for repositioning the window when the main UI moves or resizes.

  TSegment.parentForm := gTimelineForm;

  gTimelineForm.width  := aWidth;
  gTimelineForm.height := DEFAULT_SEGMENT_HEIGHT;

  gTimelineForm.show;
  winAPI.Windows.setWindowPos(gTimelineForm.handle, HWND_TOP, aPt.X, aPt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);

  mmpShowStreamList(point(aPt.x + gTimelineForm.width, aPt.y), aWidth, gTimelineForm.exportSegments, bCreateNew);

  mmpDo(evMPKeepOpen, TRUE);
  mmpDo(evGSShowingTimeline, TRUE);
  mmpDo(evGSTimelineHeight, gTimelineForm.height + 10);
end;

var gTL: TTimeline = NIL;
function shutTimeline: boolean;
begin
  mmpShutStreamList;
  case gTL            <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case gTimelineForm  <> NIL of TRUE: begin gTimelineForm.free; gTimelineForm := NIL; end;end;
  mmpDo(evMPKeepOpen, FALSE);
  mmpDo(evGSShowingTimeline, FALSE);
  mmpDo(evGSTimelineHeight, 0);
end;

function TL: TTimeline;
begin
  case gTL = NIL of TRUE: gTL := TTimeline.create; end;
  result := gTL;
end;

{$R *.dfm}

{ TTimelineForm }

procedure TTimelineForm.createParams(var params: TCreateParams);
// no taskbar icon for this window
begin
  inherited;
  params.ExStyle    := params.ExStyle or (WS_EX_APPWINDOW);
  params.WndParent  := SELF.Handle; // normally application.handle
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
var
  vCursorPt:  TPoint;
  vRect:      TRect;
begin
  winApi.windows.getCursorPos(vCursorPt);
  getWindowRect(SELF.handle, vRect);
  case (vCursorPt.x <= vRect.location.x) or (vCursorPt.x >= vRect.bottomRight.x) of TRUE: EXIT; end;

  case FDragging of TRUE: begin
                            cursorPos := cursorPos + (X - pnlCursor.Width div 2);
                            var vNewPos := mmpDo(evPBSetNewPosition, cursorPos).integer;
                            mmpDo(evSTDisplayTime, mmpFormatTime(vNewPos) + ' / ' + mmpFormatTime(TL.max));
                            updatePositionDisplay(vNewPos);
                          end;end;

  var vSeg := TL.segmentAt(cursorPos);
  case vSeg = NIL of FALSE: vSeg.repaint; end;
end;

procedure TTimelineForm.pnlCursorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDragging := FALSE;
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

  case (TL.lengthenCount = 1) OR (TL.shortenCount = 1) of TRUE: TL.addUndo(vAction); end; // record undo starting point
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

  case key = ord('C') of TRUE: begin vOK := TL.cutSegment(TL.segmentAt(cursorPos), TL.position);              TL.drawSegments; end;end;
  case key = ord('R') of TRUE: begin vOK := TL.restoreSegment(TSegment.selSeg);                               TL.drawSegments; end;end;
  case key = ord('X') of TRUE: begin vOK := TL.delSegment(TSegment.selSeg);                                   TL.drawSegments; end;end;
  case key = ord('I') of TRUE: begin vOK := TL.cutSegment(TL.segmentAt(cursorPos), TL.position, TRUE);        TL.drawSegments; end;end;
  case key = ord('O') of TRUE: begin vOK := TL.cutSegment(TL.segmentAt(cursorPos), TL.position, FALSE, TRUE); TL.drawSegments; end;end;
  case (key = ord('M')) and NOT mmpCtrlKeyDown of TRUE: begin vOK := TL.mergeRight(TSegment.selSeg);                                   TL.drawSegments; end;end;
  case (key = ord('M')) and     mmpCtrlKeyDown of TRUE: debug('merge them all!'); end;
  case key = ord('N') of TRUE: begin vOK := TL.mergeLeft(TSegment.selSeg);                                    TL.drawSegments; end;end;

  case key = ord('L') of TRUE: begin vOK := TRUE; TL.lengthenCount := 0; end;end;  // user has stopped holding down L
  case key = ord('S') of TRUE: begin vOK := TRUE; TL.shortenCount  := 0; end;end;  // user has stopped holding down S

  var vSaveUndo := vOK; // a change was made

  case mmpCtrlKeyDown and (key = ord('Z')) of TRUE: begin TL.undo(TL.prevAction); vSaveUndo := FALSE; TL.drawSegments; end;end; // Ctrl-Z
  case mmpCtrlKeyDown and (key = ord('Y')) of TRUE: begin TL.redo;                vSaveUndo := FALSE; TL.drawSegments; end;end; // Ctrl-Y

  case vSaveUndo of TRUE: begin
                            vAction := TL.saveSegments;
                            TL.drawSegments;
                            TL.addUndo(vAction); end;end;

  case TL.validKey(key) of TRUE: key := 0; end; // trap the key if we did something with it
end;

procedure TTimelineForm.FormResize(Sender: TObject);
begin
  lblPosition.left := (SELF.width   div 2) - (lblPosition.width   div 2);
  lblPosition.top  := (SELF.height  div 2) - (lblPosition.height  div 2);
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

function TTimelineForm.updatePositionDisplay(const aPosition: integer = -1): boolean;
begin
  var vPosition := mmpIfThenElse(aPosition = -1, TL.position, aPosition);
  case lblPosition.tag = 0 of  TRUE: gTimelineForm.lblPosition.caption  := intToStr(vPosition) + 's';
                              FALSE: gTimelineForm.lblPosition.caption  := mmpFormatTime(vPosition); end;
  lblPosition.repaint;
  lblPosition.bringToFront;
end;

{ TTimeline }

function TTimeline.addUndo(const aAction: string): string;
begin
  case aAction = FPrevAction of TRUE: EXIT; end;

  FPrevAction := aAction;

  var vSL     := TStringList.create;
  vSL.text    := aAction;
  FUndoList.push(vSL);
  result      := aAction;
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
  FSubscriber           := appEvents.subscribe(newSubscriber(onNotify));
end;

function TTimeline.cutSegment(const aSegment: TSegment; const aPosition: integer; const bDeleteLeft: boolean = FALSE; const bDeleteRight: boolean = FALSE): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;

  var newStartSS := aPosition;

  var newSegment := TSegment.create(newStartSS, aSegment.EndSS);
  aSegment.EndSS := newStartSS - 1;

  case newSegment.endSS <= newSegment.startSS of TRUE: debugFormat('seg: %s+, start: %d, end: %d', [aSegment.SegID, newSegment.startSS, newSegment.endSS]); end;

  case mmpCtrlKeyDown of TRUE: delSegment(aSegment); end;
  case bDeleteLeft    of TRUE: delSegment(aSegment); end;
  case bDeleteRight   of TRUE: delSegment(newSegment); end;

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
    vSegment.height  := gTimelineForm.height;
    vSegment.left    := trunc((vSegment.startSS / FMax) * gTimelineForm.width);
    vSegment.width   := trunc((vSegment.duration / FMax) * gTimelineForm.width);
    vSegment.caption := '';
    vSegment.segID   := format('%.2d', [n]);
    vSegment.setDisplayDetails;
    vSegment.StyleElements := [];
    mmpCopyPNGImage(gTimelineForm.imgTrashCan, vSegment.trashCan);

    VSegment.trashCan.visible := vSegment.deleted;
    case vSegment.deleted of TRUE: begin
                                      vSegment.trashCan.left := (vSegment.width  div 2) - (vSegment.trashCan.width  div 2);
                                      vSegment.trashCan.top  := (vSegment.height div 2) - (vSegment.trashCan.height div 2); end;end;
    vSegment.parent := gTimelineForm;
    vSegment.invalidate;
    setWindowPos(vSegment.handle, HWND_BOTTOM, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE);
    inc(n);
  end;
  gTimelineForm.pnlCursor.bringToFront;
  mmpApplySegments(TL.segments);
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
  result      := TRUE;
  FCancelled  := FALSE;

  vSegOneFN   := '';

  var vProgressForm       := TProgressForm.create(NIL);
  vProgressForm.onCancel  := onCancelButton;
  var vS1 := ''; case segments.count   > 1 of  TRUE: vS1 := 's'; end; // it bugs me that so many programmers don't bother to do this! :D
  var vS2 := ''; case MI.selectedCount > 1 of  TRUE: vS2 := 's'; end;
  vProgressForm.heading.caption := format('Exporting %d segment%s (%d stream%s)', [TSegment.includedCount, vS1, MI.selectedCount, vS2]);
  vProgressForm.show;

  // export segments
  try
    case mmpCtrlKeyDown of FALSE: begin
      var vSL := TStringList.create;
      try
        vSL.saveToFile(filePathSEG); // clear previous contents
        for var vSegment in segments do begin
          case vSegment.deleted of TRUE: CONTINUE; end;

          cmdLine := '-hide_banner';

          cmdLine := cmdLine + ' -ss "' + intToStr(vSegment.startSS) + '"';
          cmdLine := cmdLine + ' -i "'  + FMediaFilePath + '"';
          cmdLine := cmdLine + ' -t "'  + intToStr(vSegment.duration) + '"';

          vMaps := '';
          for var vMediaStream in MI.mediaStreams do
            case vMediaStream.selected of TRUE: begin
                                                  vID := strToIntDef(vMediaStream.ID, 0);
                                                  case MI.lowestID = 1 of TRUE: vID := vID - 1; end;
                                                  vMaps := vMaps + format(' -map 0:%d ', [vID]);
                                                end;end;
          vMaps   := vMaps + ' -c copy';
          cmdLine := cmdLine + vMaps;
          cmdLine := cmdLine + STD_SEG_PARAMS;

          var segFile := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + '.seg' + vSegment.segID + extractFileExt(FMediaFilePath);
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


    // Previously, FFmpeg would have overwritten the output file, except now we don't run FFmpeg if we only have one segment
    // and the rename below would fail if we don't delete the output file or at least rename it. So we may as well delete it.
    case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE); end; // use the user's specified deleteMethod

    while fileExists(filePathOUT) do mmpDelay(1000); // give the thread time to run.

    case vSegOneFN = '' of FALSE: begin
                                    renameFile(vSegOneFN, filePathOUT);
                                    EXIT; end;end;

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
  mmpRefreshStreamInfo(aMediaFilePath);
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
  vSL:        TStringList;
  vStartSS:   integer;
  vEndSS:     integer;
  vDeleted:   boolean;
  posHyphen:  integer;
  posComma:   integer;
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
  var vLogFile  := filePathLOG;
  var vLog      := TStringList.create;
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
  var ix                  := aSegment.ix;
  aSegment.startSS        := segments[ix - 1].startSS;
  segments[ix - 1].color  := aSegment.color; // set the color...
  segments.delete(ix - 1);                   // ...then delete the segment???
  result := TRUE;
end;

function TTimeline.mergeRight(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  var ix          := aSegment.ix;
  aSegment.endSS  := segments[ix + 1].endSS;
  segments[ix + 1].color := aSegment.color; // set the color...
  segments.delete(ix + 1);                  // ...then delete the segment???
  result := TRUE;
end;

procedure TTimeline.onCancelButton(sender: TObject);
begin
  FCancelled := TRUE;
end;

function TTimeline.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evTLMax:      setMax(aNotice.integer);
    evTLPosition: setPosition(aNotice.integer);
  end;
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

function TTimeline.segmentAt(const aCursorPos: integer): TSegment;
begin
  result := NIL;
  for var vSegment in segments do
    case (aCursorPos >= vSegment.left) and (aCursorPos <= vSegment.left + vSegment.width) of TRUE: begin result := vSegment; BREAK; end;end;
end;

procedure TTimeline.setMax(const Value: integer);
begin
  FMax := value;
end;

procedure TTimeline.setPosition(const Value: integer);
begin
  FPosition := value;
  case (FPosition = 0) OR (FMax = 0) of TRUE: EXIT; end;
  gTimelineForm.pnlCursor.left := trunc((FPosition / FMax) * gTimelineForm.width) - (gTimelineForm.pnlCursor.width div 2);
  gTimelineForm.updatePositionDisplay;
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

finalization
  case gTL            <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case gTimelineForm  <> NIL of TRUE: begin gTimelineForm.close; gTimelineForm.free; gTimelineForm := NIL; end;end;

end.
