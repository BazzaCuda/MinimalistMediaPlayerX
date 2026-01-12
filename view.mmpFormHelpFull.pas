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
unit view.mmpFormHelpFull;

interface

uses
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics, vcl.stdCtrls,
  {$endif}
  MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazAction,
  mmpConsts, HTMLUn2, HtmlView, Vcl.TitleBarCtrls, Vcl.Buttons, Vcl.ToolWin;

type
  IHelpFullForm = interface
    function closeForm: TVoid;
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm:  TVoid;
  end;

  THelpFullForm = class(TForm, IHelpFullForm)
    pageControl: TPageControl;
    titleBar: TTitleBarPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  strict private
    FHelpType: THelpType;
  private
  protected
    function  createTabs(const aHelpType: THelpType): TVoid;
    function  fontAdjust(const aAdjustment: integer): TVoid;

    procedure WMNCMouseMove(var msg: TWMNCMouseMove); message WM_MOUSEMOVE;
    procedure WMSizing(var msg: TMessage);            message WM_SIZING;
  public
    function closeForm: TVoid;
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm: TVoid;
  end;

function mmpHelpFull(const aHelpType: THelpType = htMain; const bOnTop: boolean = FALSE): TVoid;

implementation

uses
  bazCmd, bazVCL,
  mmpGlobalState, mmpMarkDownUtils, mmpUtils,
  view.mmpThemeUtils,
  _debugWindow;

{$R *.dfm}

type
  TMarkdownRec = record
    helpType: THelpType;
    caption:  string;
    resource: string;
  end;

const
    MARKDOWN_RESOURCES: array[0..6] of TMarkDownRec =
    (
      (helpType: htBoth;    caption: 'Adjust Image';      resource: 'resource_mdAdjustImage'),
      (helpType: htBoth;    caption: 'Screenshots';       resource: 'resource_mdScreenshots'),
      (helpType: htMain;    caption: 'biggles';           resource: 'resource_mdHelp3'),
      (helpType: htImages;  caption: 'flies';             resource: 'resource_mdImages1'),
      (helpType: htImages;  caption: 'undone';            resource: 'resource_mdImages2'),
      (helpType: htImages;  caption: 'on';                resource: 'resource_mdImages3'),
      (helpType: htMain;    caption: 'Editing';           resource: 'resource_mdEditing')
    );

var gHelpFullForm: IHelpFullForm = NIL;

function mmpHelpFull(const aHelpType: THelpType = htMain; const bOnTop: boolean = FALSE): TVoid;
begin
  case gHelpFullForm = NIL of  TRUE:  gHelpFullForm := THelpFullForm.create(app);
                              FALSE:  begin
                                        gHelpFullForm.closeForm;
                                        gHelpFullForm := NIL;
                                        mmp.cmd(evGSHelpFull, FALSE);
                                        EXIT; end;end;

  mmp.cmd(evGSHelpFull, TRUE);
  gHelpFullForm.init(aHelpType);

  setForegroundWindow(gHelpFullForm.getHandle); // the order of these two is important
  case bOnTop of TRUE: setWindowPos(gHelpFullForm.getHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE); end;

  gHelpFullForm.showForm;
end;

function THelpFullForm.closeForm: TVoid;
begin
  close;
end;

function THelpFullForm.createTabs(const aHelpType: THelpType): TVoid;
  function initLabel(const aLabel: TLabel): TVoid;
  begin
    aLabel.align          := alBottom;
    aLabel.alignment      := taCenter;
    aLabel.caption        := 'hit [Escape] to close this window';
    aLabel.font.color     := clTeal;
    aLabel.font.name      := 'Segoe UI';
    aLabel.font.style     := [fsItalic];
    aLabel.parentFont     := FALSE;
    alabel.styleElements  := [seClient, seBorder];
  end;

  function initMarkDownViewer(const aMarkDownViewer: TMarkdownViewer): TVoid;
  begin
    aMarkdownViewer.align   := alClient;
    aMarkdownViewer.margins.setBounds(0, 0, 0, 0); // let the markdownviewers fill the client area
  end;

begin
  while pageControl.pageCount > 0 do pageControl.pages[0].free;

  for var vIx := low(MARKDOWN_RESOURCES) to high(MARKDOWN_RESOURCES) do
  begin
    case MARKDOWN_RESOURCES[vIx].HelpType in [aHelpType, htBoth] of FALSE: CONTINUE; end;
    var vTabSheet := TTabSheet.Create(pageControl);
    vTabSheet.pageControl       := pageControl;
    vTabSheet.caption           := MARKDOWN_RESOURCES[vIx].caption;

    var vLabel                  := TLabel.Create(vTabSheet);
    vLabel.parent               := vTabSheet;
    initLabel(vLabel);

    var vMarkDownViewer         := TMarkDownViewer.Create(vTabSheet);
    vMarkDownViewer.parent      := vTabSheet;
    initMarkDownViewer(vMarkDownViewer);


    mmpInitMarkdownViewer(vMarkDownViewer);
    vMarkDownViewer.defFontSize := 10;

    mmpLoadMarkDownFromResource(vMarkdownViewer, MARKDOWN_RESOURCES[vIx].resource);
  end;
end;

function THelpFullForm.fontAdjust(const aAdjustment: integer): TVoid;
begin
  for var i := 0 to pageControl.pageCount - 1 do
  begin
    var vTab := pageControl.pages[i];
    for var j := 0 to vTab.controlCount - 1 do
      case vTab.controls[j] is TMarkdownViewer of TRUE: begin
                                                          TMarkdownViewer(vTab.controls[j]).defFontSize := TMarkdownViewer(vTab.controls[j]).defFontSize + aAdjustment;
                                                          TMarkdownViewer(vTab.controls[j]).refreshViewer(TRUE, TRUE, TRUE); end;end;
  end;
end;

procedure THelpFullForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//
end;

procedure THelpFullForm.FormCreate(Sender: TObject);
begin
  SELF.margins.setBounds(0, 0, 0, 0);         // let the markdownviewers fill the client area
  pageControl.margins.setBounds(0, 0, 0, 0);  // ditto
  borderStyle   := bsSizeable;
  keyPreview    := FALSE; // EXPERIMENTAL TRUE;
  borderIcons   := [biSystemMenu];

  with customTitleBar do begin
    backgroundColor               := DARK_MODE_LIGHT; // this and inactiveBackgroundColor are the wrong way around
    buttonBackgroundColor         := DARK_MODE_DARK;
    buttonForegroundColor         := DARK_MODE_DARK;
    buttonHoverForegroundColor    := DARK_MODE_DARK;
    buttonHoverBackgroundColor    := DARK_MODE_DARK;
    buttonInactiveForegroundColor := DARK_MODE_DARK;
    buttonInactiveBackgroundColor := DARK_MODE_DARK;
    buttonPressedForegroundColor  := DARK_MODE_DARK;
    buttonPressedBackgroundColor  := DARK_MODE_DARK;
    foregroundColor               := DARK_MODE_SILVER;
    inactiveBackgroundColor       := DARK_MODE_DARK; // this and backgroundColor are the wrong way around
    inactiveForegroundColor       := DARK_MODE_SILVER;
  end;

  speedButton1.caption := 'A' + #$2191; // Unicode hex escape
  speedButton2.caption := 'A' + #$2193; // Unicode hex escape
end;

procedure THelpFullForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  case key = VK_ESCAPE of TRUE: close; end;
end;

function THelpFullForm.getHandle: HWND;
begin
  result := SELF.HANDLE;
end;

function THelpFullForm.init(const aHelpType: THelpType): TVoid;
begin
  case aHelpType = FHelpType of FALSE: createTabs(aHelpType); end;
  FHelpType := aHelpType;
  case FHelpType of
    htMain:   caption := 'MMP Help - Main Media Window';
    htImages: caption := 'MMP Help - Image & Thumbnail Browser';
  end;
end;

function THelpFullForm.showForm: TVoid;
begin
  show;
end;

procedure THelpFullForm.SpeedButton1Click(Sender: TObject);
begin
  fontAdjust(1);
end;

procedure THelpFullForm.SpeedButton2Click(Sender: TObject);
begin
  fontAdjust(-1);
end;

procedure THelpFullForm.WMNCMouseMove(var Msg: TWMNCMouseMove);
begin
//  {$if BazDebugWindow} debug('WMMouseMove'); {$endif}
  inherited;

  case Msg.HitTest of
    HTLEFT, HTRIGHT,
    HTTOPLEFT, HTTOPRIGHT,
    HTBOTTOMLEFT, HTBOTTOMRIGHT:
      SetCursor(Screen.Cursors[crVSplit]); // vertical resize cursor
    // HTTOP, HTBOTTOM → leave normal
  end;
end;

procedure THelpFullForm.WMSizing(var msg: TMessage);
begin
//  {$if BazDebugWindow} debug('WMSizing'); {$endif}
  // Msg.LPARAM points to the proposed window RECT
  var vRect := PRect(msg.LPARAM);

  case msg.WPARAM of
    WMSZ_LEFT, WMSZ_RIGHT, WMSZ_TOPLEFT, WMSZ_TOPRIGHT, WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT: vRect.right := vRect.left + width; // lock width
    // WMSZ_TOP, WMSZ_BOTTOM → vertical resizing allowed
  end;

   SetCursor(Screen.Cursors[crVSplit]); // vertical resize cursor

  msg.result := 0; // Windows expects 0 unless you want to override default processing
end;

end.
