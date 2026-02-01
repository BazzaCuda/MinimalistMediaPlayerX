{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit TSegmentClass;

interface

uses
  winApi.windows, winApi.messages,
  system.classes, system.generics.collections,
  vcl.controls, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpAction;

const
  NEARLY_BLACK = clBlack + $101010;
  DEFAULT_SEGMENT_HEIGHT = 54;

type
  TSegment = class(TPanel)
  strict private
    FDeleted:     boolean;
    FEndSS:       integer;
    FOldColor:    TColor;
    FOnDblClick:  TNotifyEvent;
    FRedraw:      TNotifyEvent;
    FSegDetails:  TLabel;
    FSegID:       TLabel;
    FSelected:    boolean;
    FStartSS:     integer;
    FTitle:       TLabel;
    FTrashCan:    TImage;
  private
    function      getDuration:  integer;
    function      getIsFirst:   boolean;
    function      getIsLast:    boolean;
    function      getIx:        integer;
    function      getSegID:     string;
    function      getTitle:     string;
    procedure     setSegID(const Value: string);
    procedure     setSelected(const Value: boolean);
    procedure     setNewTitle(const aValue: string);
    procedure     setTitle(const Value: string);

    class var     FParent:      TWinControl;
    class var     FSelSeg:      TSegment;
    class var     FSegments:    TObjectList<TSegment>;

    class destructor  freeSegments;
    class function    getSegments:      TObjectList<TSegment>;  static;
    class function    getIncludedCount: integer;                static;

  protected
    procedure doClick(Sender: TObject);
    procedure doDblClick(Sender: TObject);
    procedure doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  public
    constructor Create(const aStartSS: integer; const aEndSS: integer; const onRedraw: TNotifyEvent = NIL; const onDoubleClick: TNotifyEvent = NIL; const bDeleted: boolean = FALSE); reintroduce;
    function    delete: TVoid;
    function    restore: TVoid;
    procedure   setDisplayDetails;
    function    setAsSelSeg: TVoid;
    property    deleted:   boolean read FDeleted      write FDeleted;
    property    duration:  integer read getDuration;
    property    endSS:     integer read FEndSS        write FEndSS;
    property    isFirst:   boolean read getIsFirst;
    property    isLast:    boolean read getIsLast;
    property    ix:        integer read getIx;
    property    newTitle:  string                     write setNewTitle;
    property    oldColor:  TColor  read FOldColor     write FOldColor;
    property    onDoubleClick: TNotifyEvent read FOnDblclick write FOnDblClick;
    property    redraw:    TNotifyEvent read FRedraw  write FRedraw;
    property    segID:     string  read getSegID      write setSegID;
    property    selected:  boolean read FSelected     write setSelected;
    property    startSS:   integer read FStartSS      write FStartSS;
    property    title:     string  read getTitle      write setTitle;
    property    trashCan:  TImage  read FTrashCan;

    class function clearFocus:    TVoid; static;
    class property includedCount: integer               read getIncludedCount;
    class property parentForm:    TWinControl                                   write FParent;
    class property segments:      TObjectList<TSegment> read getSegments; // technique copied from system.messaging.TMessageManager
    class property selSeg:        TSegment              read FSelSeg            write FSelSeg;
  end;

implementation

uses
  system.sysUtils,
  mmpConsts,
  _debugWindow;

function generateRandomEvenDarkerSoftColor: TColor; // from a suggestion by chatGPT
{$J+} const nextColor:        integer = 0;      {$J-}
{$J+} var   darkerSoftColors: array of TColor;  {$J-}

  function fillSilvers: TVoid;
  begin
//    darkerSoftColors[0] := RGB(80, 80, 80);   // Very Dark Gray
//    darkerSoftColors[1] := RGB(70, 70, 70);   // Very Dark Silver
//    darkerSoftColors[2] := RGB(60, 60, 60);   // Very Dark Platinum
//    darkerSoftColors[3] := RGB(50, 50, 50);   // Very Dark Snow
//    darkerSoftColors[4] := RGB(40, 40, 40);   // Very Dark Ivory
//  //  darkerSoftColors[5] := RGB(30, 30, 30);   // Extremely Dark Gray
//  // EXIT;

    darkerSoftColors[0] := RGB(28, 28, 28);   // Dark Neutral Gray 1     // best so far. like the original 5 but not as stark
    darkerSoftColors[1] := RGB(36, 36, 36);   // Dark Neutral Gray 2
    darkerSoftColors[2] := RGB(44, 44, 44);   // Dark Neutral Gray 3
    darkerSoftColors[3] := RGB(52, 52, 52);   // Dark Neutral Gray 4
    darkerSoftColors[4] := RGB(60, 60, 60);   // Dark Neutral Gray 5
    darkerSoftColors[5] := RGB(32, 32, 32);   // Dark Neutral Gray 6
    darkerSoftColors[6] := RGB(40, 40, 40);   // Dark Neutral Gray 7
    darkerSoftColors[7] := RGB(48, 48, 48);   // Dark Neutral Gray 8
    darkerSoftColors[8] := RGB(56, 56, 56);   // Dark Neutral Gray 9
    darkerSoftColors[9] := RGB(64, 64, 64);   // Dark Neutral Gray 10
  end;
begin
  // Define an array of even darker soft colors
  case length(darkerSoftColors) = 0 of TRUE:  begin
                                                setLength(darkerSoftColors, 10);
                                                fillSilvers; // whoever _he_ is!
                                              end;end;

  result := darkerSoftColors[nextColor];
  inc(nextColor);
  case nextColor > 9 of TRUE: nextColor := 0; end;
end;

{ TSegment }

class function TSegment.clearFocus: TVoid;
begin
  for var i := 0 to FSegments.count - 1 do FSegments[i].selected := FALSE;
  FSelSeg := NIL;
end;

class function TSegment.getIncludedCount: integer;
begin
  result := 0;
  for var vSegment in FSegments do
    case vSegment.deleted of FALSE: inc(result); end;
end;

class function TSegment.getSegments: TObjectList<TSegment>;
begin
  case FSegments = NIL of TRUE: begin
                                  FSegments := TObjectList<TSegment>.create;
                                  FSegments.ownsObjects := TRUE; end;end;
  result := FSegments;
end;

class destructor TSegment.freeSegments;
begin
  freeAndNil(FSegments);
end;

constructor TSegment.Create(const aStartSS: integer; const aEndSS: integer; const onRedraw: TNotifyEvent = NIL; const onDoubleClick: TNotifyEvent = NIL; const bDeleted: boolean = FALSE);
begin
  inherited Create(NIL);
  parent            := FParent;
  height            := DEFAULT_SEGMENT_HEIGHT;
  font.color        := DARK_MODE_SILVER;
  font.size         := 10;
  font.style        := [fsBold];
  alignment         := taLeftJustify;
  onClick           := doClick;
  onDblClick        := doDblClick;
  onMouseUp         := doMouseUp;
  FRedraw           := onRedraw;
  FOnDblClick       := onDoubleClick;

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
  FSegID.onClick    := doClick;
  FSegID.OnDblClick := doDblClick;
  FSegID.OnMouseUp  := doMouseUp;
  FSegID.font.color := COLOR_SEGMENT_ID;

  FTitle := TLabel.create(SELF);
  FTitle.parent         := SELF;
  FTitle.top            := 18;
  FTitle.left           := 4;
  FTitle.styleElements  := [];
  FTitle.onClick        := doClick;
  FTitle.onDblClick     := doDblClick;
  FTitle.OnMouseUp      := doMouseUp;
  FTitle.font.color     := COLOR_SEGMENT_TITLE;

  FSegDetails := TLabel.create(SELF);
  FSegDetails.parent        := SELF;
  FSegDetails.top           := 38;
  FSegDetails.left          := 4;
  FSegDetails.styleElements := [];
  FSegDetails.onClick       := doClick;
  FSegDetails.onDblClick    := doDblClick;
  FSegDetails.OnMouseUp     := doMouseUp;

  FTrashCan := TImage.create(SELF);
  FTrashCan.parent      := SELF;
  FTrashCan.stretch     := TRUE;
  FTrashCan.center      := TRUE;
  FTrashCan.height      := 31;
  FTrashCan.width       := 41;
  FTrashCan.visible     := FALSE;
  FTrashCan.onClick     := doClick;
  FTrashCan.onDblClick  := doDblClick;
  FTrashCan.OnMouseUp   := doMouseUp;

  case bDeleted of TRUE: SELF.delete; end;
end;

function TSegment.delete: TVoid;
begin
  deleted    := TRUE;
  case color  = NEARLY_BLACK of FALSE: oldColor := color; end; // in case user tries to delete an already-deleted segment
  color      := NEARLY_BLACK;
end;

procedure TSegment.doClick(Sender: TObject);
begin
  setAsSelSeg;
end;

procedure TSegment.doDblClick(Sender: TObject);
begin
  case assigned(FOnDblClick) of TRUE: FOnDblClick(SELF); end;
end;

procedure TSegment.doMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case button = mbRight of FALSE: EXIT; end;
  case FDeleted of   TRUE: restore;
                    FALSE: delete; end;
  case assigned(FRedraw) of TRUE: FRedraw(SELF); end;
end;

function TSegment.getDuration: integer;
begin
  case FStartSS = FEndSS of TRUE: begin result := 1; EXIT; end;end; // prevent zero duration segments

  result := (FEndSS - FStartSS);

  case FStartSS = 0 of   TRUE: EXIT;
                        FALSE: result := result + 1; end;
end;

function TSegment.getIsFirst: boolean;
begin
  result := ix = 0;
end;

function TSegment.getIsLast: boolean;
begin
  result := ix = FSegments.count - 1;
end;

function TSegment.getIx: integer;
begin
  result := FSegments.indexOf(SELF);
end;

function TSegment.getSegID: string;
begin
  result := FSegID.caption;
end;

function TSegment.getTitle: string;
begin
  result := FTitle.caption;
end;

procedure TSegment.Paint;
begin
  var rect := getClientRect;

  canvas.brush.color := color;

  canvas.fillRect(rect);

  case isLast and (segments.count > 1) of TRUE: rect.right := rect.right - 4; end; // no idea! :D

  case selected of  TRUE: Frame3D(canvas, rect, clTeal, clTeal, 1);
                   FALSE: Frame3D(canvas, rect, color, color, 1); end;
end;

function TSegment.restore: TVoid;
begin
  deleted := FALSE;
  case oldColor = NEARLY_BLACK of FALSE: color := oldColor; end;
end;

procedure TSegment.setSegID(const Value: string);
begin
  FSegID.caption := value;
end;

function TSegment.setAsSelSeg: TVoid;
begin
  clearFocus;
  FSelSeg    := SELF;
  selected   := TRUE;
end;

procedure TSegment.setDisplayDetails;
begin
  FSegDetails.caption := format('%ds - %ds', [startSS, endSS]);
end;

procedure TSegment.setNewTitle(const aValue: string);
begin
  FTitle.caption := aValue;
  case assigned(FRedraw) of TRUE: FRedraw(SELF); end;
end;

procedure TSegment.setSelected(const Value: boolean);
begin
  FSelected := Value;
  invalidate;
end;

procedure TSegment.setTitle(const Value: string);
begin
  FTitle.caption := Value;
end;

procedure TSegment.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  // don't call inherited
  message.result := 1; // prevent flicker when dragging the Timeline cursor over the panel
end;

end.
