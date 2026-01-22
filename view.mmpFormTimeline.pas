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
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.Windows,
  system.classes, system.generics.collections,
  system.sysUtils, vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics,
  {$endif}
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts,
  TSegmentClass, Vcl.Imaging.pngimage;

type

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
  private
    function  getCursorPos: integer;
    procedure setCursorPos(const Value: integer);
    function  updatePositionDisplay(const aPosition: integer): boolean;
  protected
    procedure createParams(var params: TCreateParams);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    property  cursorPos: integer read getCursorPos write setCursorPos;
  end;

  TTimeline = class(TObject)
  strict private
    FCancelled:               boolean;
    FDragging:                boolean;
    FUseKeyFrames:            boolean;
    FMax:                     integer;
    FMediaFilePath:           string;
    FNewestIx:                integer;
    FPlayEdited:              boolean;
    FPositionSS:              integer;
    FPrevAction:              string;
    FProcessHandle:           THandle;
    FUndoList:                TObjectStack<TStringList>;
    FRedoList:                TObjectStack<TStringList>;
    FSubscriber:              ISubscriber;

  private
    constructor Create;
    destructor  Destroy; override;

    function    getMax:       integer;
    function    getPosition:  integer;
    function    getSegCount:  integer;
    function    getSegments:  TObjectList<TSegment>;

    procedure   setMax(const Value: integer);
    procedure   setPosition(const Value: integer);
    procedure   setPosOnly(const Value: integer);

    function    addUndo(const aAction: string): string;
    function    createDefaultSegment(const aMax: integer): string;
    function    cutSegment(const aSegment: TSegment; const aPositionSS: integer; const bDeleteLeft: boolean = FALSE; const bDeleteRight: boolean = FALSE): boolean;
    function    drawSegments(const bResetHeight: boolean = FALSE): boolean;
    function    filePathMMP:              string;
    function    lengthenSegment(const aSegment: TSegment): boolean;
    function    loadChapters: TStringList;
    function    loadSegments(const aStringList: TStringList = NIL; const includeTitles: boolean = FALSE): string;
    function    mergeLeft(const aSegment: TSegment): boolean;
    function    mergeRight(const aSegment: TSegment): boolean;
    function    restoreSegment(const aSegment: TSegment): boolean;
    function    saveSegments: string;
    function    segFileEntry(const aSegFile: string): string;
    function    segmentAtSS(const aSS: integer): TSegment;
    function    segmentTitle(const aNumber: integer): string;
    function    shortenSegment(const aSegment: TSegment): boolean;
    function    skipExcludedSegments(const aPosition: integer): integer;
    function    skipToNextSegment(const aPosition: integer): integer;
    function    toggleKeyFrames: string;
  protected
    procedure   exportSegments(sender: TObject);
    function    onNotify(const aNotice: INotice): INotice;
    procedure   onSegmentDblClick(Sender: TObject);
    procedure   onRedraw(sender: TObject);
  public
    function    clear:          boolean;
    function    delSegment(const aSegment: TSegment): boolean;
    function    initTimeline(const aMediaFilePath: string; const aMax: integer): boolean;
    function    notify(const aNotice: INotice): INotice;
    function    redo:           boolean;
    function    undo:           boolean;
    function    validKey(key: WORD; aShift: TShiftState): boolean;

    property    dragging:       boolean               read FDragging      write FDragging;
    property    max:            integer               read getMax         write setMax;
    property    mediaFilePath:  string                read FMediaFilePath;
    property    playEdited:     boolean               read FPlayEdited    write FPlayEdited;
    property    positionSS:     integer               read getPosition    write setPosition;
    property    posOnly:        integer                                   write setPosOnly;
    property    prevAction:     string                read FPrevAction    write FPrevAction;
    property    segCount:       integer               read getSegCount;
    property    segments:       TObjectList<TSegment> read getSegments;
  end;

function focusTimeline: boolean;
function showTimeline(const aPt: TPoint; const aWidth: integer; const bPlayEdited: boolean; const bMoveStreamList: boolean; const bCreateNew: boolean = TRUE): boolean;
function shutTimeline: boolean;
function TL: TTimeline;

implementation

uses
  system.math,
  bazCmd,
  mmpExporter, mmpFormatting, mmpGlobalState, mmpImageUtils, mmpFormInputBox, mmpKeyboardUtils, mmpUtils,
  view.mmpFormStreamList,
  model.mmpConfigFile, model.mmpKeyFrames, model.mmpMediaInfo,
  _debugWindow;

var gTimelineForm: TTimelineForm = NIL;
function focusTimeline: boolean;
begin
  case gTimeLineForm = NIL of TRUE: EXIT; end;
  setForegroundWindow(gTimelineForm.handle); // so this window also receives keyboard keystrokes
end;

function showTimeline(const aPt: TPoint; const aWidth: integer; const bPlayEdited: boolean; const bMoveStreamList: boolean; const bCreateNew: boolean = TRUE): boolean;
begin
  case (gTimelineForm = NIL) and bCreateNew of TRUE: gTimelineForm := TTimelineForm.create(NIL); end;
  case gTimelineForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current timeline window. Used for repositioning the window when the main UI moves or resizes.

  TSegment.parentForm := gTimelineForm;

  gTimelineForm.width  := aWidth;
  gTimelineForm.height := DEFAULT_SEGMENT_HEIGHT;

  gTimelineForm.show;
  winAPI.Windows.setWindowPos(gTimelineForm.handle, HWND_TOP, aPt.X, aPt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);

  TL.playEdited := bPlayEdited;
  TL.positionSS := mmp.cmd(evMPReqPosition).integer; // don't wait for the next evTLPosition event

  case bMoveStreamList of TRUE: mmpShowStreamList(point(aPt.x + gTimelineForm.width, aPt.y), aWidth, TL.exportSegments, bCreateNew); end;

  mmp.cmd(evMPKeepOpen, TRUE);
  mmp.cmd(evGSShowingTimeline, TRUE);
  mmp.cmd(evGSTimelineHeight, gTimelineForm.height + 10);
end;

var gTL: TTimeline = NIL;
function shutTimeline: boolean;
begin
  mmpShutStreamList;
  case gTL            <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case gTimelineForm  <> NIL of TRUE: begin gTimelineForm.free; gTimelineForm := NIL; end;end;
  mmp.cmd(evMPKeepOpen, FALSE);
  mmp.cmd(evGSTimelineHeight, 0);
  mmp.cmd(evGSShowingTimeline, FALSE);
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
  case button = mbLeft of TRUE: TL.dragging := TRUE; end;
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
{$J+} const vPrevX: integer = -1; {$J-}
var
  vCursorPt:  TPoint;
  vRect:      TRect;
begin
  winApi.windows.getCursorPos(vCursorPt);
  getWindowRect(SELF.handle, vRect);
  case (vCursorPt.x <= vRect.location.x) or (vCursorPt.x >= vRect.bottomRight.x) of TRUE: EXIT; end; // prevent dragging outside the bounds of the timeline

  case x = vPrevX of TRUE: EXIT; end; // filter out hundreds of repeating messages when the mouse is being held down but not being moved
  vPrevX := X;

  case TL.dragging of  TRUE:  begin
                                cursorPos := cursorPos + (X - pnlCursor.width div 2); // just sets pnlCursor.left
                                case cursorPos < 0 of TRUE: cursorPos := 0; end;
                                var vNewPos := mmp.cmd(evPBSetNewPosition, cursorPos).integer; // PB returns the x position converted to SS
                                mmp.cmd(evSTDisplayTime, mmpFormatTime(vNewPos) + ' / ' + mmpFormatTime(TL.max));
                                TL.positionSS := vNewPos;                               // this also sets pnlCursor.left. hmmm
//                                TL.posOnly := vNewPos; // just set TL.position with none of the side-effects
//                                updatePositionDisplay(TL.position);                   // TL.position has just done this! hmmm
                                var vSeg := TL.segmentAtSS(TL.positionSS);
                                case vSeg = NIL of FALSE: vSeg.invalidate; end; // it's really not clear which is best here, repaint or invalidate
                              end;end;

end;

procedure TTimelineForm.pnlCursorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case button = mbLeft of TRUE: TL.dragging := FALSE; end;
end;

procedure TTimelineForm.setCursorPos(const Value: integer);
begin
  pnlCursor.left := value;
end;

procedure TTimelineForm.FormCreate(Sender: TObject);
begin
  lblPosition.caption   := EMPTY;

  doubleBuffered        := TRUE;

  pnlCursor.height      := SELF.height;
  pnlCursor.top         := 0;
  pnlCursor.left        := -1;
  pnlCursor.width       := pnlCursor.width + 1;
  pnlCursor.bevelOuter  := bvNone;

  lblPosition.styleElements := [seFont];
  lblPosition.borderStyle   := bsNone;
  lblPosition.bevelOuter    := bvNone;
  lblPosition.color         := TL_DEFAULT_COLOR;
  lblPosition.controlStyle  := lblPosition.controlStyle + [csOpaque];
  lblPosition.visible       := TRUE;
end;

procedure TTimelineForm.FormKeyPress(Sender: TObject; var Key: Char);
// e.g. key:char here may be x or X, but keyUp:word will always be 88
begin
  case mmpShiftKeyDown of TRUE: EXIT; end; // EXPERIMENTAL
  key := upCase(key);
  case key in ['L', 'S'] of FALSE: EXIT; end; // ignore irrelevant keystrokes - let main window have them

  case key in ['L'] of TRUE: TL.lengthenSegment(TSegment.selSeg); end;
  case key in ['S'] of TRUE: TL.shortenSegment(TSegment.selSeg);  end;

  TL.drawSegments;
end;

procedure TTimelineForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
// For VK_ keys we only get a formKeyDown event
begin
  case (key = 67) and GS.cleanup    of TRUE: begin mmp.cmd(evGSCleanup, FALSE);     key := 0; EXIT; end;end; // block the spurious C-up after Ctrl-Shift-C
  case (key = 82) and GS.renameFile of TRUE: begin mmp.cmd(evGSRenameFile, FALSE);  key := 0; EXIT; end;end; // block the spurious R-up after Ctrl-R
  case TL.validKey(key, shift) of FALSE: EXIT; end; // ignore irrelevant keystrokes - let main window have them

  var vSaveUndo := FALSE;

  case key of
    ord('C'): vSaveUndo := TL.cutSegment(TL.segmentAtSS(TL.positionSS), TL.positionSS, TL.segmentAtSS(TL.positionSS).deleted, TL.segmentAtSS(TL.positionSS).deleted);

    ord('I'): vSaveUndo := TL.cutSegment(TL.segmentAtSS(TL.positionSS), TL.positionSS, TRUE);
    ord('O'): vSaveUndo := TL.cutSegment(TL.segmentAtSS(TL.positionSS), TL.positionSS, FALSE, TRUE);

    ord('M'): vSaveUndo := TL.mergeRight(TSegment.selSeg);
    ord('N'): vSaveUndo := TL.mergeLeft(TSegment.selSeg);

    ord('R'): vSaveUndo := TL.restoreSegment(TSegment.selSeg);
    ord('X'): vSaveUndo := TL.delSegment(TSegment.selSeg);

    ord('L'): vSaveUndo := TRUE; // user has stopped holding down L; save the Timeline
    ord('S'): case mmpShiftKeyDown of  TRUE: TL.skipToNextSegment(TL.positionSS);
                                      FALSE: vSaveUndo := TRUE; end; // user has stopped holding down S; save the Timeline
  end;

  case vSaveUndo of TRUE: TL.addUndo(TL.saveSegments); end; // a change was made

  case key of
    ord('Z'): case mmpCtrlKeyDown of TRUE: TL.undo; end; // Ctrl-Z Undo
    ord('Y'): case mmpCtrlKeyDown of TRUE: TL.redo; end; // Ctrl-Y Redo
  end;

  TL.drawSegments;
  updatePositionDisplay(TL.positionSS);

  case key = ord('F') of TRUE: mmp.cmd(evSTOpInfo, TL.toggleKeyFrames); end;

  case TL.validKey(key, shift) of TRUE: key := 0; end; // trap the key if we did something with it
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
  updatePositionDisplay(TL.positionSS);
end;

function TTimelineForm.updatePositionDisplay(const aPosition: integer): boolean;
begin
  case lblPosition.tag = 0 of  TRUE: gTimelineForm.lblPosition.caption  := intToStr(aPosition) + 's';
                              FALSE: gTimelineForm.lblPosition.caption  := mmpFormatTime(aPosition); end;

  var vSelSeg := TL.segmentAtSS(aPosition);
  case vSelSeg = NIL of FALSE: case vSelSeg.deleted of   TRUE: lblPosition.color := clBlack;
                                                        FALSE: lblPosition.color := TL_DEFAULT_COLOR; end;end;

  lblPosition.invalidate;
  lblPosition.bringToFront;
end;

procedure TTimelineForm.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // don't call inherited
  message.result := 1; // prevent flicker when dragging the Timeline cursor over a panel
end;

{ TTimeline }

function TTimeline.addUndo(const aAction: string): string;
begin
  while FRedoList.count > 0 do FUndoList.push(FRedoList.extract); // !!!!!!

  FPrevAction := aAction;

  var vSL     := TStringList.create;
  vSL.text    := aAction;
  FUndoList.push(vSL);
  result      := aAction;
end;

function TTimeline.clear: boolean;
begin
  segments.clear;
end;

constructor TTimeline.Create;
begin
  inherited;
  FUndoList             := TObjectStack<TStringList>.create;
  FRedoList             := TObjectStack<TStringList>.create;
  FUndoList.ownsObjects := TRUE;
  FRedoList.ownsObjects := TRUE;
  FSubscriber           := appEvents.subscribe(newSubscriber(onNotify));
end;

function TTimeline.createDefaultSegment(const aMax: integer): string;
begin
  var vIx := segments.add(TSegment.create(0, aMax, onRedraw, onSegmentDblClick));
  case CF.asBoolean[CONF_CHAPTERS_SHOW] of TRUE: segments[vIx].title := 'Default Segment'; end;
  result := format('0-%d,0=Default Segment', [aMax]);
end;

function TTimeline.cutSegment(const aSegment: TSegment; const aPositionSS: integer; const bDeleteLeft: boolean = FALSE; const bDeleteRight: boolean = FALSE): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;

  var newStartSS := aPositionSS;
  case aSegment.endSS < newStartSS of TRUE: EXIT; end; // guard against "rounding" errors

  var newSegment := TSegment.create(newStartSS, aSegment.EndSS, onRedraw, onSegmentDblClick); // this will be the righthand segment of the two. The old segment will finish 1 second to the left of it.
  aSegment.EndSS := system.math.max(newStartSS - 1, 1); // don't allow endSS = 0 when startSS = 1

  case bDeleteLeft    of TRUE: delSegment(aSegment); end;
  case bDeleteRight   of TRUE: delSegment(newSegment); end;

  case aSegment.isLast of  TRUE:  FNewestIx := segments.add(newSegment);
                          FALSE:  begin
                                    segments.insert(aSegment.ix + 1, newSegment);
                                    FNewestIx := aSegment.ix + 1; end;end;

  case newSegment.isLast of TRUE: newSegment.endSS := FMax; end; // issue with losing the correct duration when cutting the final segment

  case CF.asBoolean[CONF_CHAPTERS_SHOW] of TRUE: newSegment.title := segmentTitle(FNewestIx + 1); end;

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
  try
    segments.clear;
  finally
  end;
  FUndoList.free;
  FRedoList.free;
  inherited;
end;

function TTimeline.drawSegments(const bResetHeight: boolean = FALSE): boolean;
begin
  case GS.openingURL of TRUE: EXIT; end;
  case FMax = 0 of TRUE: EXIT; end;

  var n := 1;
  try
    for var i := 0 to segments.count - 1 do begin
      var vSegment := segments[i];
      vSegment.top     := 0;
      vSegment.height  := gTimelineForm.height;
      case vSegment.ix = 0 of  TRUE: vSegment.left := 0;
                              FALSE: vSegment.left := trunc((vSegment.startSS / FMax) * gTimelineForm.width); end;
      vSegment.width   := trunc((vSegment.duration / FMax) * gTimelineForm.width);
      case vSegment.isLast of TRUE: vSegment.width := gTimelineForm.width - vSegment.left; end;

      vSegment.caption := EMPTY;
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
    case segCount > 1 of TRUE: segments[0].width := segments[1].left - 1; end;
  finally
  end;

  gTimelineForm.pnlCursor.bringToFront;
  mmpApplySegments(segments, FMax, bResetHeight);
  mmpScrollTo(FNewestIx);
end;

function TTimeline.filePathMMP: string;
begin
  result := changeFileExt(FMediaFilePath, '.mmp');
end;

function TTimeline.getMax: integer;
begin
  result := FMax;
end;

function TTimeline.getPosition: integer;
begin
  result := FPositionSS;
end;

function TTimeline.getSegCount: integer;
begin
  result := segments.count;
end;

function TTimeline.getSegments: TObjectList<TSegment>;
begin
  result := TSegment.segments;
end;

procedure TTimeline.exportSegments(sender: TObject);
begin
  case mmpCtrlKeyDown and mmpShiftKeyDown of   TRUE: mmpNewExporter(FMediaFilePath, GS.mediaType).copySourceFile;
                                              FALSE: mmpNewExporter(FMediaFilePath, GS.mediaType).exportEdits; end;
end;

function TTimeline.initTimeline(const aMediaFilePath: string; const aMax: integer): boolean;
begin
  result := FALSE;
  case FMediaFilePath = aMediaFilePath of TRUE: EXIT; end;

  // {$if BazDebugWindow} debugFormat('initTimeLine: %d', [aMax]); {$endif}

  segments.clear;

  FUndoList.clear; FRedoList.clear;
  mmpRefreshStreamInfo(aMediaFilePath);
  FMediaFilePath := aMediaFilePath;
  FMax           := aMax;
  addUndo(createDefaultSegment(aMax)); // always give the user a vanilla Ctrl-Z starting point
  case fileExists(filePathMMP) of  TRUE: addUndo(loadSegments);
                                  FALSE: begin
                                           var  vSL := loadChapters;
                                           case vSL  = NIL of FALSE: begin addUndo(loadSegments(vSL, TRUE)); vSL.free; end;end;end;end;

  positionSS := mmp.cmd(evMPReqPosition).integer; // don't wait for the next evTLPosition event

  drawSegments(TRUE);

  case (GS.mediaType = mtVideo) of TRUE:  begin
                                            case FUseKeyFrames of FALSE: FUseKeyFrames := CF.asBoolean[CONF_KEYFRAMES]; end;
                                            case FUseKeyFrames of  TRUE: keyFrameManager.init(aMediaFilePath); end;end;end;

  result := TRUE;
end;

function TTimeline.lengthenSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL  of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  aSegment.endSS := aSegment.endSS + 1;
  segments[aSegment.ix + 1].startSS := segments[aSegment.ix + 1].startSS + 1;
end;

function TTimeline.loadSegments(const aStringList: TStringList = NIL; const includeTitles: boolean = FALSE): string;
var
  vSL:        TStringList;
  vStartSS:   integer;
  vEndSS:     integer;
  vDeleted:   boolean;
  vTitle:     string;
  posHyphen:  integer;
  posComma:   integer;
  posEquals:  integer;
begin
  result := EMPTY;
  segments.clear;
  vSL := TStringList.create;
  try
    case aStringList <> NIL of  TRUE: vSL.text := aStringList.text;
                               FALSE: vSL.loadFromFile(filePathMMP); end;

    for var i := 0 to vSL.count - 1 do begin
      case trim(vSL[i]) = EMPTY of TRUE: CONTINUE; end;
      posHyphen := pos('-', vSL[i]);
      vStartSS  := strToInt(copy(vSL[i], 1, posHyphen - 1));
      posComma  := pos(',', vSL[i]);
      vEndSS    := strToInt(copy(vSL[i], posHyphen + 1, posComma - posHyphen - 1));
      vDeleted  := copy(vSL[i], posComma + 1, 1) = '1';

      posEquals := pos('=', vSL[i]);
      case posEquals > 0 of  TRUE: vTitle := copy(vSL[i], posEquals + 1, length(vSL[i]));
                            FALSE: vTitle := EMPTY; end;

      var ix := segments.add(TSegment.create(vStartSS, vEndss, onRedraw, onSegmentDblClick, vDeleted));
      case CF.asBoolean[CONF_CHAPTERS_SHOW] of TRUE: segments[ix].title := vTitle; end;          // regardless of includeTitles, assume we're reading a .mmp file
      case includeTitles of TRUE: segments[ix].title := MI.mediaChapters[ix].chapterTitle; end;  // includeTitles says we're reading chapters from the media file

      case CF.asBoolean[CONF_CHAPTERS_SHOW] and (segments[ix].title = EMPTY) of TRUE: segments[ix].title := segmentTitle(ix + 1); end;

//      result := result + format('%d-%d,%d', [vStartSS, vEndSS, integer(vDeleted)]) + #13#10;
      result := result + format('%d-%d,%d=%s', [vStartSS, vEndSS, integer(vDeleted), segments[ix].title]) + #13#10;
    end;
  finally
    vSL.free;
  end;
end;

function TTimeLine.loadChapters: TStringList;
begin
  result := NIL;
  case MI.chapterCount = 0 of TRUE: EXIT; end;

  result := TStringList.create;

  for var i := 0 to MI.chapterCount - 1 do
    result.add(format('%d-%d,0=%s', [MI.mediaChapters[i].chapterStartSS, MI.mediaChapters[i].chapterEndSS, MI.mediaChapters[i].chapterTitle]));
end;

function TTimeline.mergeLeft(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL   of TRUE: EXIT; end;
  case aSegment.isFirst of TRUE: EXIT; end;
  var ix                  := aSegment.ix;
  aSegment.startSS        := segments[ix - 1].startSS;
  segments.delete(ix - 1);
  result := TRUE;
end;

function TTimeline.mergeRight(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL   of TRUE: EXIT; end;
  case aSegment.isLast  of TRUE: EXIT; end;
  var ix          := aSegment.ix;
  aSegment.endSS  := segments[ix + 1].endSS;
  segments.delete(ix + 1);
  result := TRUE;
end;

function TTimeline.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TTimeline.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case GS.openingURL of TRUE: EXIT; end;
  case aNotice = NIL of TRUE: EXIT; end;
  case GS.showingTimeline of FALSE: EXIT; end;

  case aNotice.event of
    evTickTimer:          mmp.cmd(evPBBackgroundColor, PB_DEFAULT_BACKGROUND_COLOR); // gets set in model.mmpKeyFrames.probeKeyFrames
  end;

  case aNotice.event of
    evTLMax:              setMax(aNotice.integer);
    evTLPosition:         case FDragging of FALSE: setPosition(aNotice.integer); end;
    evTLRename:           FMediaFilePath := aNotice.text; // mmpVM renames the mmp file
  end;
end;

procedure TTimeline.onRedraw(sender: TObject);
begin
  TL.addUndo(TL.saveSegments);
//  TL.saveSegments;
  TL.drawSegments;
  gTimelineForm.updatePositionDisplay(TL.positionSS);
end;

procedure TTimeline.onSegmentDblClick(Sender: TObject);
begin
  var vSegment := Sender as TSegment;
  var vS := mmpInputBoxForm(vSegment.title);
  case vS = vSegment.title of FALSE: vSegment.newTitle := vS; end;
end;

function TTimeline.redo: boolean;
// discard the most recent action [by moving it straight to the undo list] and apply the subsequent actions from the stack
begin
  case FRedoList.count = 0 of TRUE: EXIT; end;

  // move the most recent action [reflected in the Timeline] straight to the undo list, and then redo _to_ the action before that.
  // It's not a major problem, but without this the user's first Ctrl-[Y] won't appear to do anything.
  while (FRedoList.count > 0) and (FRedoList.peek.text = FPrevAction) do FUndoList.push(FRedoList.extract);

  // action the top item on the stack then move it to the undo list
  case FRedoList.count > 0 of TRUE:   begin
                                        loadSegments(FRedoList.peek);
                                        FPrevAction := saveSegments; // the previous action is what is currently reflected in the Timeline
                                        drawSegments;
                                        FUndoList.push(FRedoList.extract); end;end;
end;

function TTimeline.restoreSegment(const aSegment: TSegment): boolean;
begin
  result := FALSE;
  case aSegment = NIL of TRUE: EXIT; end;
  aSegment.restore;

//  aSegment.deleted := FALSE;
//  case aSegment.oldColor = NEARLY_BLACK of FALSE: aSegment.color := aSegment.oldColor; end;
  result := TRUE;
end;

function TTimeline.saveSegments: string;
begin
  case segCount = 0 of TRUE: EXIT; end;

  var vSLInternal := TStringList.create;
  var vSLExternal := TStringList.create;

  try
    for var vSegment in segments do begin
      vSLInternal.add(format('%d-%d,%d',    [vSegment.startSS, vSegment.endSS, integer(vSegment.deleted)                ]));
      vSLExternal.add(format('%d-%d,%d=%s', [vSegment.startSS, vSegment.endSS, integer(vSegment.deleted), vSegment.title])); end;

    vSLExternal.saveToFile(filePathMMP);
    result := vSLExternal.text;
  finally
    vSLExternal.free;
    vSLInternal.free;
  end;

  case (segCount = 1) AND (segments[0].startSS = 0) AND (segments[0].endSS = FMax) AND fileExists(filePathMMP) of TRUE: deleteFile(filePathMMP); end;
end;

function TTimeline.segFileEntry(const aSegFile: string): string;
begin
  result := 'file ''' + stringReplace(aSegFile, '\', '\\', [rfReplaceAll]) + '''';
end;

function TTimeline.segmentAtSS(const aSS: integer): TSegment;
begin
  result := NIL;
  for var vSegment in segments do
    case (aSS >= vSegment.startSS) and (aSS <= vSegment.endSS) of TRUE: begin result := vSegment; BREAK; end;end;
end;

function TTimeline.segmentTitle(const aNumber: integer): string;
begin
  result := format('Segment %.2d', [aNumber]);
end;

procedure TTimeline.setMax(const Value: integer);
begin
  FMax := value;
end;

procedure TTimeline.setPosition(const Value: integer);
begin
  case GS.showingTimeline of FALSE: EXIT; end;

  FPositionSS := skipExcludedSegments(value); // will do a seek if needed
  // {$if BazDebugWindow} debugFormat('FPositionSS: %d, FMax: %d', [FPositionSS, FMax]); {$endif}

//  var vPositionSS := mmp.cmd(evMPReqPrecisePos).double;

  case FPositionSS = 0 of  TRUE:  gTimelineForm.pnlCursor.left := 0;
                          FALSE:  case FMax = 0 of FALSE: gTimelineForm.pnlCursor.left := trunc((FPositionSS / FMax) * gTimelineForm.width) - (gTimelineForm.pnlCursor.width div 2); end;end;
  mmpProcessMessages;

  gTimelineForm.updatePositionDisplay(FPositionSS);

  var vColor    := clWhite;

  case FUseKeyFrames
    of TRUE:  begin
                var vProximity := keyFrameManager.proximity(FPositionSS);
                // debugFormat('pos: %d = %g', [FPositionSS, vProximity]);
                case (vProximity >= 0.5) and (vProximity <= 1.0)  of TRUE: vColor := clYellow;  end;
                case  vProximity <  0.5                           of TRUE: vColor := clFuchsia; end;
                case  vProximity <  0.0                           of TRUE: vColor := clWhite;   end;end;end; // override clFuchsia if -1 was returned

  gTimeLineForm.pnlCursor.color := vColor;

  gTimelineForm.pnlCursor.bringToFront;
  gTimelineForm.pnlCursor.invalidate;
  mmpProcessMessages;
end;

procedure TTimeline.setPosOnly(const Value: integer);
begin
  FPositionSS := value;
end;

function TTimeline.shortenSegment(const aSegment: TSegment): boolean;
begin
  case aSegment = NIL  of TRUE: EXIT; end;
  case aSegment.isLast of TRUE: EXIT; end;
  aSegment.endSS := aSegment.endSS - 1;
  segments[aSegment.ix + 1].startSS := segments[aSegment.ix + 1].startSS - 1;
end;

function TTimeline.skipExcludedSegments(const aPosition: integer): integer;
begin
  result {newPos} := aPosition;
  case GS.skipExcluded of FALSE: EXIT; end;

  while result < FMax do  begin
                            var vSeg := segmentAtSS(result);
                            case vSeg = NIL   of   TRUE: BREAK; end;
                            case vSeg.deleted of  FALSE: BREAK; end;
                            result := vSeg.endSS + 1; end;

  case result = aPosition of FALSE: mmp.cmd(evMPSeek, result); end;
end;

function TTimeline.skipToNextSegment(const aPosition: integer): integer;
begin
  result {newPos} := aPosition;

  var  vSeg := segmentAtSS(result);
  case vSeg = NIL   of   TRUE: EXIT; end;

  case mmpCtrlKeyDown of   TRUE: case vSeg.isFirst of  TRUE: result := vSeg.startSS;
                                                      FALSE: case (aPosition - vSeg.startSS) > 3 of  TRUE: result := vSeg.startSS;
                                                                                                    FALSE: result := segments[vSeg.ix - 1].startSS; end;end;
                          FALSE: case vSeg.isLast  of  TRUE: result := vSeg.endSS;
                                                      FALSE: result := segments[vSeg.ix + 1].startSS; end;end;

  case result < 0 of TRUE: result := 0; end;

  case result = aPosition of FALSE: mmp.cmd(evMPSeek, result); end;
end;

function TTimeline.toggleKeyFrames: string;
begin
  result := 'not applicable to audio';
  case GS.mediaType <> mtVideo of TRUE: EXIT; end;

  FUseKeyFrames := NOT FUseKeyFrames;

  case FUseKeyFrames of FALSE:  begin
                                  result := 'keyframes off';
                                  EXIT; end;end;

  result := 'keyframes on';
  keyFrameManager.init(FMediaFilePath);
end;

function TTimeline.undo: boolean;
// discard the most recent action [by moving it straight to the redo list] and apply the subsequent actions from the stack
begin
  // move the most recent action [reflected in the Timeline] straight to the undo list, and then undo _to_ the action before that.
  // It's not a major problem, but without this the user's first Ctrl-[Z] won't appear to do anything.
  while (FUndoList.count > 1) and (FUndoList.peek.text = FPrevAction) do FRedoList.push(FUndoList.extract); // don't remove the default undo segment

  // action the top item on the stack then move it to the redo list
  case FUndoList.count > 0 of TRUE:   begin
                                        case FUndoList.peek.text <> FPrevAction of TRUE: loadSegments(FUndoList.peek); end; // prevent random colors on an otherwise unchanging Timeline
                                        FPrevAction := saveSegments; // the previous action is what is currently reflected in the Timeline
                                        drawSegments;
                                        case FUndoList.count > 1 of TRUE: FRedoList.push(FUndoList.extract); end; // don't let the user move the default undo segment to the redo list!!
                                      end;end;
end;

function TTimeline.validKey(key: WORD; aShift: TShiftState): boolean;
const
  validKeys1 = 'CFIOMNRXLS';
  validKeys2 = 'YZ';
begin
  var vCtrlKeyDown  := ssCtrl   in aShift;
  var vShiftKeyDown := ssShift  in aShift;

  result := (validKeys1.contains(char(key)) and NOT (vCtrlKeyDown or vShiftKeyDown)) or (validKeys2.contains(char(key)) and vCtrlKeyDown);
  result := result or ((char(key) = 'S') and vShiftKeyDown); // EXPERIMENTAL
end;

initialization

finalization
  case gTL            <> NIL of TRUE: begin gTL.free; gTL := NIL; end;end;
  case gTimelineForm  <> NIL of TRUE: begin gTimelineForm.close; gTimelineForm.free; gTimelineForm := NIL; end;end;

end.
