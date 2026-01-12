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
  mmpConsts, HTMLUn2, HtmlView;

type
  IHelpFullForm = interface
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm:  TVoid;
  end;

  THelpFullForm = class(TForm, IHelpFullForm)
    pageControl: TPageControl;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  strict private
    FHelpType: THelpType;
  protected
//    procedure createParams(var Params: TCreateParams); override;
    function  createTabs(const aHelpType: THelpType): TVoid;
    procedure WMNCMouseMove(var msg: TWMNCMouseMove); message WM_MOUSEMOVE;
    procedure NCHitTest(var msg: TMessage);           message WM_NCHITTEST;
    procedure WMSizing(var msg: TMessage);            message WM_SIZING;
  public
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm: TVoid;
  end;

function mmpHelpFull(const aHelpType: THelpType): TVoid;

implementation

uses
  bazCmd, bazVCL,
  mmpGlobalState, mmpMarkDownUtils, mmpUtils,
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
      (helpType: htMain;    caption: 'not';               resource: 'resource_mdHelp2'),
      (helpType: htMain;    caption: 'biggles';           resource: 'resource_mdHelp3'),
      (helpType: htImages;  caption: 'flies';             resource: 'resource_mdImages1'),
      (helpType: htImages;  caption: 'undone';            resource: 'resource_mdImages2'),
      (helpType: htImages;  caption: 'on';                resource: 'resource_mdImages3'),
      (helpType: htBoth;    caption: 'Editing';           resource: 'resource_mdEditing')
    );

var gHelpFullForm: IHelpFullForm = NIL;

function mmpHelpFull(const aHelpType: THelpType): TVoid;
begin
  mmp.cmd(evGSHelpFull, TRUE);
  case gHelpFullForm = NIL of TRUE: gHelpFullForm := THelpFullForm.create(app); end;
  gHelpFullForm.init(aHelpType);
  setForegroundWindow(gHelpFullForm.getHandle); // the order of these two is important
  setWindowPos(gHelpFullForm.getHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
  gHelpFullForm.showForm;
end;

//procedure THelpFullForm.createParams(var Params: TCreateParams);
//begin
//  inherited;
////  Params.Style := Params.Style or WS_CAPTION or WS_SYSMENU or WS_THICKFRAME;
//  // Remove left/right border styles so horizontal resize is blocked
//  // We keep WS_THICKFRAME because Windows requires it for vertical resizing
//end;

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
    aMarkdownViewer.margins.setBounds(0, 0, 0, 0);
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
    vMarkDownViewer.defFontSize := 9;

    mmpLoadMarkDownFromResource(vMarkdownViewer, MARKDOWN_RESOURCES[vIx].resource);
  end;
end;

procedure THelpFullForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mmp.cmd(evGSHelpFull, FALSE);
  gHelpFullForm := NIL;
end;

procedure THelpFullForm.FormCreate(Sender: TObject);
begin
//  styleElements := [];
  borderStyle   := bsSizeable;;
  keyPreview    := TRUE;
  borderIcons   := [biSystemMenu];
end;

procedure THelpFullForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: close; end;
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

procedure THelpFullForm.NCHitTest(var msg: TMessage);
begin
  {$if BazDebugWindow} debug('NCHitTest'); {$endif}
end;

function THelpFullForm.showForm: TVoid;
begin
  show;
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
