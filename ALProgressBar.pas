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
}

unit ALProgressBar;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TProgressDirection = (pdLeftToRight, pdRightToLeft, pdBottomToTop, pdTopToBottom);
  TBarColorStyle = (cs1Color, cs2Colors, cs3Colors);

  TALProgressBar = class(TGraphicControl)
  private
    MainBitmap: TBitmap;
    fBorderColor1: TColor;
    fBorderColor2: TColor;
    fBackgroundColor: TColor;
    fPosition: Integer;
    fBarBitmap: TBitmap;
    fMin: Integer;
    fMax: Integer;
    fShowBorder: Boolean;
    fDirection: TProgressDirection;
    fShowPosText: Boolean;
    fPosTextSuffix: String;
    fPosTextPrefix: String;
    fBarColorStyle: TBarColorStyle;
    fBarColor1: TColor;
    fBarColor2: TColor;
    fBarColor3: TColor;
    RegenerateBitmap: Boolean;
    TiledBarBitmap: TBitmap;
    fPercentage: Boolean;
    procedure PaintBorder;
    procedure PaintPosText;
    procedure SetBorderColor1(const Value: TColor);
    procedure SetBorderColor2(const Value: TColor);
    procedure SetBarBitmap(const Value: TBitmap);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetPosition(const Value: Integer);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetShowBorder(const Value: Boolean);
    procedure PaintBar(RegenerateBitmap: Boolean);
    procedure TileBitmap(TiledBitmap: TBitmap; var DestBitmap: TBitmap);
    procedure SetDirection(const Value: TProgressDirection);
    procedure SetBarColor1(const Value: TColor);
    procedure SetBarColor2(const Value: TColor);
    procedure SetBarColor3(const Value: TColor);
    procedure SetShowPosText(const Value: Boolean);
    procedure SetPosTextSuffix(const Value: String);
    procedure CMFontChanged(var Message :TMessage); message CM_FontChanged;
    procedure SetPosTextPrefix(const Value: String);
    function CalcColorIndex(StartColor, EndColor: TColor; Steps, ColorIndex: Integer): TColor;
    procedure SetBarColorStyle(const Value: TBarColorStyle);
    procedure DrawColorBlending;
    procedure SetPercentage(const Value: Boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackgroundColor: TColor        read fBackgroundColor       write SetBackgroundColor      default clBtnFace;
    property BarBitmap: TBitmap             read fBarBitmap             write SetBarBitmap;
    property BarColor1: TColor              read fBarColor1             write SetBarColor1            default clGreen;
    property BarColor2: TColor              read fBarColor2             write SetBarColor2            default clYellow;
    property BarColor3: TColor              read fBarColor3             write SetBarColor3            default clRed;
    property BarColorStyle: TBarColorStyle  read fBarColorStyle         write SetBarColorStyle        default cs3Colors;
    property BorderColor1: TColor           read fBorderColor1          write SetBorderColor1         default clBtnShadow;
    property BorderColor2: TColor           read fBorderColor2          write SetBorderColor2         default clBtnHighlight;
    property Direction: TProgressDirection  read fDirection             write SetDirection            default pdLeftToRight;
    property Max: Integer                   read fMax                   write SetMax                  default 100;
    property Min: Integer                   read fMin                   write SetMin                  default 0;
    property Percentage: Boolean            read fPercentage            write SetPercentage           default False;
    property Position: Integer              read fPosition              write SetPosition             default 0;
    property PosTextPrefix: String          read fPosTextPrefix         write SetPosTextPrefix;
    property PosTextSuffix: String          read fPosTextSuffix         write SetPosTextSuffix;
    property ShowBorder: Boolean            read fShowBorder            write SetShowBorder           default True;
    property ShowPosText: Boolean           read fShowPosText           write SetShowPosText          default True;
    property Align;
    property Font;
    property ParentShowHint;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

uses
  _debugWindow;

procedure Register;
begin
  RegisterComponents('ALComps', [TALProgressBar]);
end;

{ TALProgressBar }

constructor TALProgressBar.Create(AOwner: TComponent);
begin
  inherited;

  Width := 200;
  Height := 20;

  fBorderColor1 := clBtnShadow;
  fBorderColor2 := clBtnHighLight;
  fBackgroundColor := clBtnFace;
  fBarBitmap := TBitmap.Create;
  fBarColor1 := clGreen;
  fBarColor2 := clYellow;
  fBarColor3 := clRed;
  fBarColorStyle := cs3Colors;
  fPosition := 0;
  fMin := 0;
  fMax := 100;
  fShowBorder := True;
  fDirection := pdLeftToRight;
  fShowPosText := True;
  fPosTextSuffix := '';
  fPosTextPrefix := '';
  fPercentage := False;
  
  RegenerateBitmap := True;

  MainBitmap := TBitmap.Create;
  TiledBarBitmap := TBitmap.Create;

  cursor := crHandPoint; // BAZ
end;

destructor TALProgressBar.Destroy;
begin
  MainBitmap.Free;
  fBarBitmap.Free;
  TiledBarBitmap.Free;

  inherited;
end;

procedure TALProgressBar.Paint;
begin
  inherited;

  MainBitmap.Width := Width;
  MainBitmap.Height := Height;

  if not(csReading in ComponentState) then
    PaintBar(RegenerateBitmap);

  if fShowBorder then
    PaintBorder;

  if fShowPosText then
    PaintPosText;

  Canvas.Draw(0, 0, MainBitmap);
end;

procedure TALProgressBar.PaintBar(RegenerateBitmap: Boolean);
var
  BarLength, BarPixelLength, EmptySpaceLength: Integer;
  AreaTop, AreaBottom, AreaLeft, AreaRight: Integer;
begin
  if (fBarBitmap <> nil) and not fBarBitmap.Empty then
  begin
    if RegenerateBitmap then
    begin
      TiledBarBitmap.Height := Height;
      TiledBarBitmap.Width := Width;
      TileBitmap(fBarBitmap, TiledBarBitmap);
    end;
    MainBitmap.Canvas.Draw(0, 0, TiledBarBitmap);
  end
  else if fBarColorStyle = cs1Color then
  begin
    MainBitmap.Canvas.Brush.Color := fBarColor1;
    MainBitmap.Canvas.FillRect(Rect(0, 0, Width, Height));
  end
  else if fBarColorStyle in [cs2Colors, cs3Colors] then
  begin
    if RegenerateBitmap then
    begin
      TiledBarBitmap.Height := Height;
      TiledBarBitmap.Width := Width;
      DrawColorBlending;
    end;
    MainBitmap.Canvas.Draw(0, 0, TiledBarBitmap);
  end;

  if (fDirection = pdLeftToRight) or (fDirection = pdRightToLeft) then
  begin
    if fShowBorder then
      BarPixelLength := Width - 2
    else
      BarPixelLength := Width;
  end
  else
  begin
    if fShowBorder then
      BarPixelLength := Height - 2
    else
      BarPixelLength := Height;
  end;

  if fShowBorder then
  begin
    AreaTop := 1;
    AreaLeft := 1;
    AreaBottom := Height - 1;
    AreaRight := Width - 1;
  end
  else
  begin
    AreaTop := 0;
    AreaLeft := 0;
    AreaBottom := Height;
    AreaRight := Width;
  end;

  if (fPosition > Min) and (fMax-fMin <> 0) then
    BarLength := Round(((fPosition-fMin) / Abs(fMax-fMin)) * BarPixelLength)
  else
    BarLength := 0;
  EmptySpaceLength := BarPixelLength - BarLength;

  MainBitmap.Canvas.Brush.Color := fBackgroundColor;
  if fDirection = pdLeftToRight then
    MainBitmap.Canvas.FillRect(Rect(AreaRight-EmptySpaceLength, AreaTop, AreaRight, AreaBottom))
  else if fDirection = pdRightToLeft then
    MainBitmap.Canvas.FillRect(Rect(AreaLeft, AreaTop, AreaLeft+EmptySpaceLength, AreaBottom))
  else if fDirection = pdTopToBottom then
    MainBitmap.Canvas.FillRect(Rect(AreaLeft, AreaBottom-EmptySpaceLength, AreaRight, AreaBottom))
  else if fDirection = pdBottomToTop then
    MainBitmap.Canvas.FillRect(Rect(AreaLeft, AreaTop, AreaRight, AreaTop+EmptySpaceLength));
end;

procedure TALProgressBar.DrawColorBlending;
var
  IndexCount, MaxWidth, MaxHeight, StartPoint: Integer;
  FirstColor, SecondColor: TColor;
begin
  if fBarColorStyle = cs2Colors then
  begin
    MaxWidth := TiledBarBitmap.Width;
    MaxHeight := TiledBarBitmap.Height;
  end
  else
  begin
    MaxWidth := TiledBarBitmap.Width div 2;
    MaxHeight := TiledBarBitmap.Height div 2;
  end;

  StartPoint := 1;
  if fDirection in [pdLeftToRight, pdRightToLeft] then
  begin
    if fDirection = pdLeftToRight then
    begin
      FirstColor := fBarColor1;
      SecondColor := fBarColor2;
    end
    else
    begin
      if fBarColorStyle = cs2Colors then
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end
      else
      begin
        FirstColor := fBarColor3;
        SecondColor := fBarColor2;
      end;
    end;
    for IndexCount := StartPoint to MaxWidth do
    begin
      TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor, MaxWidth, IndexCount);
      TiledBarBitmap.Canvas.MoveTo(IndexCount-1, 0);
      TiledBarBitmap.Canvas.LineTo(IndexCount-1, TiledBarBitmap.Height);
    end;
    if fBarColorStyle = cs3Colors then
    begin
      if fDirection = pdLeftToRight then
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor3;
      end
      else
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end;
      for IndexCount := MaxWidth+1 to TiledBarBitmap.Width do
      begin
        TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor, TiledBarBitmap.Width-MaxWidth, IndexCount-MaxWidth);
        TiledBarBitmap.Canvas.MoveTo(IndexCount-1, 0);
        TiledBarBitmap.Canvas.LineTo(IndexCount-1, TiledBarBitmap.Height);
      end;
    end;
  end
  else {if fDirection in [pdTopToBottom, pdBottomToTop] then}
  begin
    if fDirection = pdTopToBottom then
    begin
      FirstColor := fBarColor1;
      SecondColor := fBarColor2;
    end
    else
    begin
      if fBarColorStyle = cs2Colors then
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end
      else
      begin
        FirstColor := fBarColor3;
        SecondColor := fBarColor2;
      end;
    end;
    for IndexCount := StartPoint to MaxHeight do
    begin
      TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor, MaxHeight, IndexCount);
      TiledBarBitmap.Canvas.MoveTo(0, IndexCount-1);
      TiledBarBitmap.Canvas.LineTo(TiledBarBitmap.Width, IndexCount-1);
    end;
    if fBarColorStyle = cs3Colors then
    begin
      if fDirection = pdTopToBottom then
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor3;
      end
      else
      begin
        FirstColor := fBarColor2;
        SecondColor := fBarColor1;
      end;
      for IndexCount := MaxHeight+1 to TiledBarBitmap.Height do
      begin
        TiledBarBitmap.Canvas.Pen.Color := CalcColorIndex(FirstColor, SecondColor, TiledBarBitmap.Height-MaxHeight, IndexCount-MaxHeight);
        TiledBarBitmap.Canvas.MoveTo(0, IndexCount-1);
        TiledBarBitmap.Canvas.LineTo(TiledBarBitmap.Width, IndexCount-1);
      end;
    end;
  end
end;

procedure TALProgressBar.TileBitmap(TiledBitmap: TBitmap; var DestBitmap: TBitmap);
var
  NoOfImagesX, NoOfImagesY, ix, iy, XPos, YPos: Integer;
begin
  NoOfImagesX := (Width div TiledBitmap.Width) + 1;
  NoOfImagesY := (Height div TiledBitmap.Height) + 1;
  XPos := 0;
  YPos := 0;
  for iy := 1 to NoOfImagesY do
  begin
    for ix := 1 to NoOfImagesX do
    begin
      DestBitmap.Canvas.Draw(XPos, YPos, TiledBitmap);
      XPos := XPos + TiledBitmap.Width;
    end;
    YPos := YPos + TiledBitmap.Height;
    XPos := 0;
  end;
end;

procedure TALProgressBar.PaintPosText;
var
  Text: String;
  TextPosX, TextPosY: Integer;
begin
  if not fPercentage then
    Text := fPosTextPrefix + IntToStr(fPosition) + fPosTextSuffix
  else
    Text := fPosTextPrefix + IntToStr(Round((fPosition / fMax) * 100)) + fPosTextSuffix;

  TextPosX := (Width div 2) - (MainBitmap.Canvas.TextWidth(Text) div 2);
  TextPosY := (Height div 2) - (MainBitmap.Canvas.TextHeight(Text) div 2);

  MainBitmap.Canvas.Brush.Style := bsClear;
  MainBitmap.Canvas.TextOut(TextPosX, TextPosY, Text);
end;

procedure TALProgressBar.PaintBorder;
begin
  with MainBitmap.Canvas do
  begin
    Pen.Color := fBorderColor1;
    MoveTo(Width-1, 0);
    LineTo(0, 0);
    LineTo(0, Height);
    Pen.Color := fBorderColor2;
    MoveTo(1, Height-1);
    LineTo(Width-1, Height-1);
    LineTo(Width-1, 0);
  end;
end;

function TALProgressBar.CalcColorIndex(StartColor, EndColor: TColor; Steps, ColorIndex: Integer): TColor;
var
  BeginRGBValue: Array[0..2] of Byte;
  RGBDifference: Array[0..2] of Integer;
  Red, Green, Blue: Byte;
  NumColors: Integer;
begin
  if (ColorIndex < 1) or (ColorIndex > Steps) then
    raise ERangeError.Create('ColorIndex can''t be less than 1 or greater than ' + IntToStr(Steps));
  NumColors := Steps;
  Dec(ColorIndex);
  BeginRGBValue[0] := GetRValue(ColorToRGB(StartColor));
  BeginRGBValue[1] := GetGValue(ColorToRGB(StartColor));
  BeginRGBValue[2] := GetBValue(ColorToRGB(StartColor));
  RGBDifference[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGBValue[2];

  // Calculate the bands color
  Red := BeginRGBValue[0] + MulDiv(ColorIndex, RGBDifference[0], NumColors - 1);
  Green := BeginRGBValue[1] + MulDiv(ColorIndex, RGBDifference[1], NumColors - 1);
  Blue := BeginRGBValue[2] + MulDiv(ColorIndex, RGBDifference[2], NumColors - 1);
  Result := RGB(Red, Green, Blue);
end;

procedure TALProgressBar.SetBarBitmap(const Value: TBitmap);
begin
  fBarBitmap.Assign(Value);
  Paint;
end;

procedure TALProgressBar.SetBackgroundColor(const Value: TColor);
begin
  fBackgroundColor := Value;
  Paint;
end;

procedure TALProgressBar.SetBorderColor1(const Value: TColor);
begin
  fBorderColor1 := Value;
  Paint;
end;

procedure TALProgressBar.SetBorderColor2(const Value: TColor);
begin
  fBorderColor2 := Value;
  Paint;
end;

procedure TALProgressBar.SetPosition(const Value: Integer);
begin
  if (fPosition <> Value) and (Value <= fMax) and (Value >= fMin) then
  begin
    fPosition := Value;
    RegenerateBitmap := False;
    Paint;
    RegenerateBitmap := True;
  end;
end;

procedure TALProgressBar.SetMax(const Value: Integer);
begin
  fMax := Value;
  Paint;
end;

procedure TALProgressBar.SetMin(const Value: Integer);
begin
  fMin := Value;
  Paint;
end;

procedure TALProgressBar.SetShowBorder(const Value: Boolean);
begin
  fShowBorder := Value;
  Paint;
end;

procedure TALProgressBar.SetDirection(const Value: TProgressDirection);
begin
  fDirection := Value;
  Paint;
end;

procedure TALProgressBar.SetBarColor1(const Value: TColor);
begin
  fBarColor1 := Value;
  Paint;
end;

procedure TALProgressBar.SetBarColor2(const Value: TColor);
begin
  fBarColor2 := Value;
  Paint;
end;

procedure TALProgressBar.SetBarColor3(const Value: TColor);
begin
  fBarColor3 := Value;
  Paint;
end;

procedure TALProgressBar.SetShowPosText(const Value: Boolean);
begin
  fShowPosText := Value;
  Paint;
end;

procedure TALProgressBar.SetPosTextSuffix(const Value: String);
begin
  fPosTextSuffix := Value;
  Paint;
end;

procedure TALProgressBar.SetPosTextPrefix(const Value: String);
begin
  fPosTextPrefix := Value;
  Paint;
end;

procedure TALProgressBar.CMFontChanged(var Message: TMessage);
begin
  MainBitmap.Canvas.Font.Assign(Self.Font);
  Paint;
end;

procedure TALProgressBar.SetBarColorStyle(const Value: TBarColorStyle);
begin
  fBarColorStyle := Value;
  Paint;
end;

procedure TALProgressBar.SetPercentage(const Value: Boolean);
begin
  fPercentage := Value;
  Paint;
end;

end.
