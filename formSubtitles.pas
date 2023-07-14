unit formSubtitles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.extCtrls, vcl.stdCtrls, MMFTimedTextNotifyClass;

type
  TSubtitlesForm = class(TForm)
  private
    FSubtitle: TLabel;
    FVideoPanel: TPanel;

    ParentRect: TRect;        // rect of the parent window
    ParentPosition: TPoint;   // parent window position

    constructor create;
    procedure WMSize(var message: TWMSize); message WM_SIZE;
    procedure WMTimedTextNotify(var message: TMessage); message WM_TIMEDTEXTNOTIFY;
    function getHWND: HWND;
  public
    destructor Destroy; override;
    function initSubtitles(aVideoPanel: TPanel): boolean;
    property HWND: HWND read getHWND;
  end;

function ST: TSubtitlesForm;

implementation

uses
  mediaPlayer;

var
  gST: TSubtitlesForm;

function ST: TSubtitlesForm;
begin
  case gST = NIL of TRUE: gST := TSubtitlesForm.create; end;
  result := gST;
end;

{$R *.dfm}

{ TSubtitlesForm }

constructor TSubtitlesForm.create;
begin
  inherited create(NIL);
  FSubtitle := TLabel.create(NIL);
end;

destructor TSubtitlesForm.Destroy;
begin
  case FSubtitle <> NIL of TRUE: FSubtitle.free; end;
  inherited;
end;

function TSubtitlesForm.getHWND: HWND;
begin
  result := SELF.HANDLE;
end;

function TSubtitlesForm.initSubtitles(aVideoPanel: TPanel): boolean;
begin
  FVideoPanel := aVideoPanel;

  SELF.parent                 := aVideoPanel;
  SELF.align                  := alBottom;
  SELF.height := 200;
  SELF.styleElements          := []; // don't allow any theme alterations
  SELF.borderStyle            := bsNone;
  SELF.color                  := clBlack;
  SELF.ctl3D                  := FALSE;
  SELF.doubleBuffered         := TRUE;
  SELF.margins.bottom         := 6;
  SELF.oldCreateOrder         := TRUE;
  SELF.formStyle              := fsStayOnTop; // Keep the form always on top
  SELF.borderIcons            := [];
  SELF.alphaBlend             := True;
  SELF.alphaBlendValue        := 255;
  SELF.transparentColorValue  := clBlack;
  SELF.transparentColor       := TRUE;

  FSubtitle.parent            := SELF;
  FSubtitle.align             := alClient;
  FSubtitle.alignment         := taCenter;
  FSubtitle.alignWithMargins  := TRUE;
  FSubtitle.color             := clBlack;
  FSubtitle.font.color        := clWhite;
  FSubtitle.font.size         := 14;
  FSubtitle.font.style        := [fsBold];
  FSubtitle.layout            := tlBottom;
  FSubtitle.margins.Bottom    := 6;
  FSubtitle.parentColor       := FALSE;
  FSubtitle.parentCustomHint  := FALSE;
  FSubtitle.parentFont        := FALSE;
  FSubtitle.ParentShowHint    := FALSE;
  FSubtitle.showAccelChar     := FALSE;
  FSubtitle.showHint          := FALSE;
  FSubtitle.transparent       := TRUE;
  FSubtitle.wordWrap          := TRUE;
  FSubtitle.caption           := '';

  SELF.show;
end;

procedure TSubtitlesForm.WMSize(var message: TWMSize);
begin
//  case FSubtitle = NIL of TRUE: EXIT; end;
//  FSubtitle.caption := '';
//  FSubtitle.caption := format('w: %d, h: %d', [FSubtitle.width, FSubtitle.height]);
end;

procedure TSubtitlesForm.WMTimedTextNotify(var message: TMessage);
begin
  FSubtitle.caption := MP.subTitle;
end;

initialization
  gST := NIL;

finalization
  case gST <> NIL of TRUE: gST.free; end;

end.
