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
  mmpConsts, HTMLUn2, HtmlView, Vcl.TitleBarCtrls, Vcl.Buttons, Vcl.ToolWin,
  CommCtrl, Vcl.ExtCtrls;

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
    lbTabCaptions: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure lbTabCaptionsClick(Sender: TObject);
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
    MARKDOWN_RESOURCES: array[0..30] of TMarkDownRec =
    (
      (helpType: htBoth;    caption: 'Intro';             resource: 'resource_mdIntro'), // shift-\ config dialog vs .conf file mentions, ctrl-H vs ctrl-shift-H, Ctrl-W Wiki / ESC vs Ctrl-Shift-H, all controls still available, stay on top for IATB
      (helpType: htBoth;    caption: 'Adjust Image';      resource: 'resource_mdAdjustImage'),
      (helpType: htBoth;    caption: 'Auto-Center';       resource: 'resource_mdAutoCenter'),  // auto-center and H vs M, G, Ctrl-G
      (helpType: htBoth;    caption: 'Auto Update';       resource: 'resource_mdAutoUpdate'),  // Auto Updates / About Box
      (helpType: htMain;    caption: 'Bookmarks';         resource: 'resource_mdBookmarks'),
      (helpType: htMain;    caption: 'Captions';          resource: ''),  // Captions / on-screen display / # redisplay
      (helpType: htMain;    caption: 'Editing';           resource: 'resource_mdEditing'),
      (helpType: htMain;    caption: 'Editing 2';         resource: ''),  // revisit Editing
      (helpType: htBoth;    caption: 'External Apps';     resource: 'resource_mdExternalApps'),  // External Apps
      (helpType: htBoth;    caption: 'File Control';      resource: 'resource_mdFileControl'),
      (helpType: htMain;    caption: 'Freeze Frame';      resource: ''),  // Freeze Frame
      (helpType: htBoth;    caption: 'Image Browser';     resource: ''),  // Launching the Image & Thumbnail Browser (including from the Main Media Window)
      (helpType: htBoth;    caption: 'Keyframes';         resource: ''),  // Keyframes
      (helpType: htMain;    caption: 'Multi-Window';      resource: ''),  // multi-window control, Ctrl-N numlock
      (helpType: htMain;    caption: 'Notification Area'; resource: ''),  // Notification area
      (helpType: htBoth;    caption: 'Panning';           resource: 'resource_mdPanning'),
      (helpType: htMain;    caption: 'Playback';          resource: ''),  // Playback stop/start/restart/loop next/previous chapter /  etc
      (helpType: htMain;    caption: 'Playlist';          resource: ''),  // Playlist - navigation (main vs browser) / shuffle mode / playlist filter / next folder on end/empty / playlist form
      (helpType: htBoth;    caption: 'Resets';            resource: ''),  // Resets summary
      (helpType: htBoth;    caption: 'Rotation';          resource: ''),  // Rotation
      (helpType: htBoth;    caption: 'Screenshots';       resource: 'resource_mdScreenshots'),
      (helpType: htBoth;    caption: 'Slideshows';        resource: ''),  // slideshows
      (helpType: htMain;    caption: 'Speed';             resource: ''),  // Speed
      (helpType: htMain;    caption: 'Subtitles';         resource: ''),  // Subtitles
      (helpType: htMain;    caption: 'Tabbing';           resource: 'resource_mdTabbing'),
      (helpType: htIATB;    caption: 'Thumbnails';        resource: ''),  // Thumbnails - increase/decrease size, Browser status bar
      (helpType: htIATB;    caption: 'User-Defined Folders';      resource: ''),  // Browser user-folders
      (helpType: htMain;    caption: 'Volume';            resource: 'resource_mdVolume'),
      (helpType: htMain;    caption: 'Window Control';    resource: ''),  // Window Control x 2 - Main Window and Browser (resizing)
      (helpType: htIATB;    caption: 'Window Control';    resource: ''),  // Window Control x 2 - Main Window and Browser (resizing)
      (helpType: htBoth;    caption: 'Zoom';              resource: '')   // Zoooom - main (keyboard only) vs Browser (keyboard and mouse wheel)
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
    aLabel.caption        := 'Use all controls as normal - hit [Escape] to close this window';
    aLabel.font.color     := clTeal;
    aLabel.font.name      := 'Segoe UI';
    aLabel.font.style     := [fsItalic, fsBold];
    aLabel.parentFont     := FALSE;
    alabel.styleElements  := [seClient, seBorder];
  end;

  function initMarkDownViewer(const aMarkDownViewer: TMarkdownViewer): TVoid;
  begin
    aMarkdownViewer.align   := alClient;
    aMarkdownViewer.margins.setBounds(0, 0, 0, 0); // "Let the markdownViewers fill the client area" - Paddy McGuinness
  end;

begin
  while pageControl.pageCount > 0 do pageControl.pages[0].free;

  for var vIx := low(MARKDOWN_RESOURCES) to high(MARKDOWN_RESOURCES) do
  begin
    case MARKDOWN_RESOURCES[vIx].HelpType in [aHelpType, htBoth] of FALSE: CONTINUE; end;
    var vTabSheet := TTabSheet.Create(pageControl);
    vTabSheet.pageControl       := pageControl;
    vTabSheet.caption           := '';
    lbTabCaptions.items.add(MARKDOWN_RESOURCES[vIx].caption);

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
  SELF.width := SELF.width + lbTabCaptions.width;
  SELF.margins.setBounds(0, 0, 0, 0);         // let the markdownviewers fill the client area

  { no tab captions}
  sendMessage(pageControl.HANDLE, TCM_SETITEMSIZE, 0, makeLParam(0, 0));
  pageControl.tabHeight := 1;
  pageControl.tabWidth  := 1;
  pageControl.tabStop   := FALSE; // no ctrl-tab navigation through the tabs

  pageControl.margins.setBounds(0, 0, 0, 0);
  borderStyle   := bsSizeable;
  keyPreview    := FALSE; // EXPERIMENTAL
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

  panel1.bevelInner  := bvLowered;
  panel1.bevelOuter  := bvLowered;
//  panel1.borderStyle := bsNone;

  lbTabCaptions.bevelInner  := bvNone;
  lbTabCaptions.bevelOuter  := bvNone;
  lbTabCaptions.borderStyle := bsNone;
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
    htMain: caption := 'MMP Help - Main Media Window';
    htIATB: caption := 'MMP Help - Image & Thumbnail Browser';
  end;
end;

procedure THelpFullForm.lbTabCaptionsClick(Sender: TObject);
begin
  pageControl.activePageIndex := lbTabCaptions.itemIndex;
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

procedure THelpFullForm.WMNCMouseMove(var msg: TWMNCMouseMove);
begin
  inherited;

// this seems to have zero effect on non-client cursors

//  case msg.hitTest of HTLEFT, HTRIGHT, HTTOPLEFT, HTTOPRIGHT, HTBOTTOMLEFT, HTBOTTOMRIGHT: setCursor(screen.Cursors[crNone]);   // vertical resize cursor
//                                                                          HTTOP, HTBOTTOM: setCursor(screen.cursors[crVSplit]); // leave normal
//  end;
end;

procedure THelpFullForm.WMSizing(var msg: TMessage);
begin
  // msg.LPARAM points to the proposed window RECT
  var vRect := PRect(msg.LPARAM);

  case msg.WPARAM of
    WMSZ_LEFT, WMSZ_RIGHT, WMSZ_TOPLEFT, WMSZ_TOPRIGHT, WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT: vRect.right := vRect.left + width; // lock width
                                                                    WMSZ_TOP, WMSZ_BOTTOM: ;                                  // vertical resizing allowed
  end;

//  setCursor(screen.cursors[crVSplit]); // vertical resize cursor

  msg.result := 0; // Windows expects 0 unless you want to override default processing
end;

end.
