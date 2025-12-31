{
  ALProgressBar v1.03

  (C)2001 Andrew Leigh
  http://www.alphalink.com.au/~leigh/components

  Description:
    TALProgressBar is an enhanced progress bar control. It allows you to tile a
    bitmap on the bar, add various color blending effects and change the
    progress direction.

  History:
    v1.0  06-Nov-1999 Initial release.
    v1.01 30-Dec-2000 Fixed floating point division error when Min and Max
                      values are the same while trying to draw the bitmap.
    v1.02 29-Jan-2001 Added Percentage property for displaying the position text
                      in an absolute percentage value.
    v1.03 02-Sep-2001 Fixed problem with background color not being persisted
                      if the color is black.
    v1.04 Jul-2023    Added Hint.
    v1.05 Jul-2024    Stripped out everything not being used in order to track down problem with bar suddenly not being drawn
}
unit ALProgressBar;

interface

uses
  winApi.windows,
  system.classes, system.generics.collections,
  vcl.controls, vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  TShowHintEvent = procedure(var Message: TCMHintShow) of object; // BAZ

  TALProgressBar = class(TCustomControl)
  strict private
    FBackgroundColor: TColor;
    FKeyFrames:       TList<double>;
    FPosition:        integer;
    FMin:             integer;
    FMax:             integer;
    FBarColor:        TColor;
    FOnHintShow:      TShowHintEvent; // BAZ
  private
    procedure   adjustBarLength;
    procedure   plotKeyFrames;
    procedure   setBackgroundColor(const aValue: TColor);
    procedure   setBarColor(const aValue: TColor);
    procedure   setKeyFrames(const aValue: string);
    procedure   setMax(const aValue: integer);
    procedure   setPosition(const aValue: integer);
  protected
    procedure   CMHintShow(var message: TCMHintShow); message CM_HINTSHOW;
    procedure   Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    backgroundColor:  TColor          read FBackgroundColor       write setBackgroundColor;
    property    barColor:         TColor          read FBarColor              write setBarColor;
    property    keyFrames:        string                                      write setKeyFrames;
    property    max:              integer         read FMax                   write setMax;
    property    onHintShow:       TShowHintEvent  read FOnHintShow            write FOnHintShow; // BAZ
    property    onMouseDown;
    property    onMouseMove;
    property    onMouseUp;
    property    position:         integer          read FPosition              write setPosition;
  end;

implementation

uses
  system.math, system.sysUtils,
  mmpConsts, mmpCmd,
  _debugWindow;

const
  BAR_HEIGHT = 10;

{ TALProgressBar }

procedure TALProgressBar.CMHintShow(var message: TCMHintShow); // BAZ
begin
  case assigned(FOnHintShow) of TRUE: FOnHintShow(message); end;
end;

constructor TALProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  height            := BAR_HEIGHT;
  doubleBuffered    := TRUE;

  FBackgroundColor  := clBlack + 1; // just enough to be different from the clBlack transparent color.

  FBarColor         := PB_DEFAULT_COLOR;
  FPosition         := 0;
  FMin              := 0;
  FMax              := 100;

  cursor            := crHandPoint;

  FKeyFrames        := TList<double>.create;
end;

destructor TALProgressBar.Destroy;
begin
  case FKeyFrames = NIL of FALSE: FKeyFrames.free; end;
  inherited;
end;

procedure TALProgressBar.Paint;
begin
  inherited;

  adjustBarLength;
//  plotKeyFrames;
end;

procedure TALProgressBar.adjustBarLength;
begin
  canvas.brush.color := FBackgroundColor;
  canvas.fillRect(rect(0, 0, SELF.width, SELF.height));

  case FMax = 0 of TRUE: EXIT; end; // prevent division by zero

//  var vPosition := mmp.cmd(evMPReqPrecisePos).double;

  var vBarLength := mmp.use(FPosition > 0, ceil((FPosition / FMax) * SELF.width), 0);

  canvas.brush.color := FBarColor;
  canvas.fillRect(rect(0, 0, vBarLength, BAR_HEIGHT));
end;

procedure TALProgressBar.plotKeyFrames;
const WIN_SIZE = 60;
var
  vVisibleStartSS: double;
  vVisibleEndSS:   double;

  procedure doCalc;
  begin
    vVisibleStartSS := system.math.max(0.0, min(FPosition - (WIN_SIZE div 2), FMax - WIN_SIZE));
    vVisibleEndSS   := vVisibleStartSS + WIN_SIZE;
    vVisibleEndSS   := min(FMax, vVisibleEndSS);
  end;

begin
  var vPixelsPerSS:double := SELF.width / WIN_SIZE; // our n-second window
  doCalc;

  canvas.brush.color := clTeal;
  for var i := 0 to FKeyFrames.count - 1 do begin
    case FKeyFrames[i] = -1 of TRUE: CONTINUE; end;
    var vKeyFrame := round(FKeyFrames[i] * vPixelsPerSS);
    case (FKeyFrames[i] > vVisibleStartSS) and (FKeyFrames[i] < vVisibleEndSS)
        of TRUE: canvas.fillRect(rect(vKeyFrame, 0, vKeyFrame + 2, BAR_HEIGHT)); end; // 2 is the width of the vertical cursor
  end;
end;

procedure TALProgressBar.setBackgroundColor(const aValue: TColor);
begin
  case FBackgroundColor = aValue of FALSE:  begin
                                              FBackgroundColor := aValue;
                                              invalidate; end;end;
end;

procedure TALProgressBar.setPosition(const aValue: integer);
begin
  case FPosition = aValue of FALSE: begin
                                      FPosition := aValue;
                                      invalidate; end;end;
end;

procedure TALProgressBar.setMax(const aValue: Integer);
begin
  case aValue > 0 of TRUE: FMax := aValue; end;
  invalidate;
end;

procedure TALProgressBar.setBarColor(const aValue: TColor);
begin
  FBarColor := aValue;
  invalidate;
end;

procedure TALProgressBar.setKeyFrames(const aValue: string);
begin
  FKeyFrames.clear;
  var vSL := TStringlist.create;
  try
    vSL.text := aValue;
    for var i := 0 to vSL.count - 1 do FKeyFrames.add(strToFloatDef(vSL[i], -1));
  finally
    vSL.free;
  end;
end;

end.
