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
  HTMLUn2, HtmlView, Vcl.TitleBarCtrls, Vcl.Buttons, Vcl.ToolWin,
  MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazAction,
  mmpConsts,
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
    sbUp: TSpeedButton;
    sbDown: TSpeedButton;
    lbTabCaptions: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    sbLeft: TSpeedButton;
    sbRight: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbUpClick(Sender: TObject);
    procedure sbDownClick(Sender: TObject);
    procedure lbTabCaptionsClick(Sender: TObject);
    procedure pageControlChange(Sender: TObject);
    procedure sbLeftClick(Sender: TObject);
    procedure sbRightClick(Sender: TObject);
  strict private
    FHelpType:  THelpType;
    FHistory:   array of integer;
    FHIstIx:    integer;
  private
    function  changePage(const aIx: integer; const bAddHistory: boolean): TVoid;
    function  historyAdd(const aIx: integer): integer;
    function  historyMove(bForwards: boolean): integer;
    procedure onHotSpotClick(sender: TObject; const SRC: string; var handled: boolean);
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
  system.math,
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
    MARKDOWN_RESOURCES: array[0..35] of TMarkDownRec =
    (
      (helpType: htBoth;    caption: 'Intro';                     resource: 'resource_mdIntro'),
      (helpType: htBoth;    caption: 'Adjust Image';              resource: 'resource_mdAdjustImage'),
      (helpType: htBoth;    caption: 'Aspect Ratio';              resource: 'resource_mdAspectRatio'),
      (helpType: htMain;    caption: 'Audio Streams';             resource: 'resource_mdAudioStreams'),
      (helpType: htBoth;    caption: 'Auto-Center';               resource: 'resource_mdAutoCenter'),
      (helpType: htBoth;    caption: 'Auto Update';               resource: 'resource_mdAutoUpdate'),
      (helpType: htMain;    caption: 'Bookmarks';                 resource: 'resource_mdBookmarks'),
      (helpType: htMain;    caption: 'Captions';                  resource: 'resource_mdCaptions'),
      (helpType: htMain;    caption: 'Editing Audio & Video';     resource: 'resource_mdEditing'),
      (helpType: htMain;    caption: 'Editing Audio';             resource: 'resource_mdEditingAudio'),
      (helpType: htMain;    caption: 'Editing Chapters';          resource: 'resource_mdEditingChapters'),
      (helpType: htMain;    caption: 'Editing Troubleshooting';   resource: 'resource_mdEditingTroubleshooting'),
      (helpType: htBoth;    caption: 'External Apps';             resource: 'resource_mdExternalApps'),
      (helpType: htBoth;    caption: 'File Control';              resource: 'resource_mdFileControl'),
      (helpType: htMain;    caption: 'Freeze Frame';              resource: 'resource_mdFreezeFrame'),
      (helpType: htBoth;    caption: 'Image Browser';             resource: 'resource_mdImageBrowser'),
      (helpType: htMain;    caption: 'Keyframes';                 resource: 'resource_mdKeyframes'),
      (helpType: htNone;    caption: 'Mouse';                     resource: 'resource_mdMouse'),
      (helpType: htMain;    caption: 'Multi-View Control';        resource: 'resource_mdMultiWindow'),
      (helpType: htBoth;    caption: 'Notification Area';         resource: 'resource_mdNotificationArea'),
      (helpType: htBoth;    caption: 'Panning';                   resource: 'resource_mdPanning'),
      (helpType: htMain;    caption: 'Playback';                  resource: 'resource_mdPlayback'),
      (helpType: htBoth;    caption: 'Playlist';                  resource: 'resource_mdPlaylist'),
      (helpType: htBoth;    caption: 'Resets';                    resource: 'resource_mdResets'),
      (helpType: htBoth;    caption: 'Rotation';                  resource: 'resource_mdRotation'),
      (helpType: htBoth;    caption: 'Screenshots';               resource: 'resource_mdScreenshots'),
      (helpType: htBoth;    caption: 'Slideshows';                resource: 'resource_mdSlideshows'),
      (helpType: htMain;    caption: 'Speed';                     resource: 'resource_mdSpeed'),
      (helpType: htMain;    caption: 'Subtitles';                 resource: 'resource_mdSubtitles'),
      (helpType: htMain;    caption: 'Tabbing';                   resource: 'resource_mdTabbing'),
      (helpType: htIATB;    caption: 'Thumbnails';                resource: 'resource_mdThumbnails'),
      (helpType: htIATB;    caption: 'User-Defined Folders';      resource: 'resource_mdUserFolders'),
      (helpType: htMain;    caption: 'Volume';                    resource: 'resource_mdVolume'),
      (helpType: htMain;    caption: 'Window Control';            resource: 'resource_mdWindowControlMain'),
      (helpType: htIATB;    caption: 'Window Control';            resource: 'resource_mdWindowControlIATB'),
      (helpType: htBoth;    caption: 'Zoom';                      resource: 'resource_mdZoom')
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
  case bOnTop of TRUE: setWindowPos(gHelpFullForm.getHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE); end; // vital because of focusThumbs in mmpVM

  gHelpFullForm.showForm;
end;

function THelpFullForm.changePage(const aIx: integer; const bAddHistory: boolean): TVoid;

  function getMarkdownViewer(const aTabSheet: TTabSheet): TMarkdownViewer;
  begin
    result := NIL;
    for var i := 0 to aTabSheet.controlCount - 1 do
      case aTabSheet.controls[i] is TMarkdownViewer of TRUE:  begin
                                                                result := TMarkdownViewer(aTabSheet.controls[i]);
                                                                BREAK; end;end;

  end;

  function getResourceIx(const aHelpType: THelpType; const aTargetIx: Integer): integer;
  begin
    result        := -1;
    var vTargetIx := -1;
    for var i := 0 to HIGH(MARKDOWN_RESOURCES) do
      case MARKDOWN_RESOURCES[i].helpType in [aHelpType, htBoth] of TRUE: begin
                                                                            inc(vTargetIx);
                                                                            case vTargetIx = aTargetIx of TRUE: begin
                                                                                                                  result := i;
                                                                                                                  BREAK; end;end;end;end;
  end;

begin
  case aIx = -1 of TRUE: EXIT; end;
  pageControl.activePageIndex := aIx;

  var  vMarkdownViewer := getMarkdownViewer(pageControl.pages[aIx]);
  case vMarkdownViewer = NIL of TRUE: EXIT; end;

  var  vResourceIx := getResourceIx(FHelpType, aIx);
  case vMarkdownViewer.lines.count = 0 of TRUE: mmpLoadMarkDownFromResource(vMarkdownViewer, MARKDOWN_RESOURCES[vResourceIx].resource); end;

  case lbTabCaptions.itemIndex = pageControl.activePageIndex of FALSE: lbTabCaptions.itemIndex := aIx; end;
  case bAddHistory of TRUE: historyAdd(aIx); end;
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
    vTabSheet.caption           := MARKDOWN_RESOURCES[vIx].caption;
    lbTabCaptions.items.add(MARKDOWN_RESOURCES[vIx].caption);

    var vLabel                  := TLabel.Create(vTabSheet);
    vLabel.parent               := vTabSheet;
    initLabel(vLabel);

    var vMarkDownViewer         := TMarkDownViewer.Create(vTabSheet);
    vMarkDownViewer.parent      := vTabSheet;
    initMarkDownViewer(vMarkDownViewer);

    mmpInitMarkdownViewer(vMarkDownViewer);
    vMarkDownViewer.defFontSize     := 10;
    vMarkDownViewer.OnHotSpotClick  := onHotSpotClick;
    vMarkDownViewer.DefHotSpotColor := clAqua;
    vMarkDownViewer.htOptions := vMarkDownViewer.htOptions + [htOverLinksActive];
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
  SELF.margins.setBounds(0, 0, 0, 0);         // "Let the markdownviewers fill the client area" - Paddy McGuinness

  { no tab captions}
//  sendMessage(pageControl.HANDLE, TCM_SETITEMSIZE, 0, makeLParam(0, 0));
//  pageControl.tabHeight := 1;
//  pageControl.tabWidth  := 1;
//  pageControl.tabStop   := FALSE; // no ctrl-tab navigation through the tabs

  pageControl.margins.setBounds(0, 0, 0, 0);
  borderStyle   := bsSizeable;
  keyPreview    := FALSE;
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

  sbUp.caption    := 'A' + #$2191; // Unicode hex escape
  sbDown.caption  := 'A' + #$2193; // Unicode hex escape
  sbLeft.caption  := #$25C0; // #$2190;
  sbRight.caption := #$25B6; // #$2192;

  sbLeft.enabled  := FALSE;
  sbRight.enabled := FALSE;

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

function THelpFullForm.historyAdd(const aIx: integer): integer;
begin
  setLength(FHistory, length(FHistory) + 1);
  FHistory[high(FHistory)]  := aIx;
  FHistIx                   := high(FHistory);

  result                    := FHistory[FHistIx];

  sbLeft.enabled            := FHistIx > 0;
  sbRight.enabled           := FALSE;
end;

function THelpFullForm.historyMove(bForwards: boolean): integer;
begin
  case bForwards of  TRUE: FHistIx := min(FHistIx + 1, high(FHistory));
                    FALSE: FHistIx := max(FHistIx - 1, 0); end;

  result          := FHistory[FHistIx];

  sbLeft.enabled  := FHistIx > 0;
  sbRight.enabled := FHistIx < high(FHistory);
end;

procedure THelpFullForm.onHotSpotClick(sender: TObject; const SRC: string; var handled: boolean);
begin
  case src.contains('.com') of TRUE: EXIT; end;
  handled := TRUE;

  var vSrc := src.replace('_', ' ');
  case vSrc.contains('ConfigDialog:') of   TRUE: mmp.cmd(evVMConfig, vSrc.split([':'])[1]);
                                          FALSE: changePage(lbTabCaptions.items.indexOf(vSrc), TRUE); end;
end;

function THelpFullForm.init(const aHelpType: THelpType): TVoid;
begin
  FHelpType := aHelpType;

  case FHelpType of
    htMain: caption := 'MMP Help - Main Media Window';
    htIATB: caption := 'MMP Help - Image && Thumbnail Browser'; end;

  createTabs(aHelpType);

  FHistIx := -1;
  changePage(0, TRUE);
end;

procedure THelpFullForm.lbTabCaptionsClick(Sender: TObject);
begin
  changePage(lbTabCaptions.itemIndex, TRUE);
end;

procedure THelpFullForm.pageControlChange(Sender: TObject);
begin
  // when the user clicks a tab or uses Ctrl-Tab to cycle through tabs
  changePage(pageControl.activePageIndex, TRUE);
end;

function THelpFullForm.showForm: TVoid;
begin
  show;
end;

procedure THelpFullForm.sbUpClick(Sender: TObject);
begin
  fontAdjust(1);
end;

procedure THelpFullForm.sbDownClick(Sender: TObject);
begin
  fontAdjust(-1);
end;

procedure THelpFullForm.sbLeftClick(Sender: TObject);
begin
  changePage(historyMove(FALSE), FALSE);
end;

procedure THelpFullForm.sbRightClick(Sender: TObject);
begin
  changePage(historyMove(TRUE), FALSE);
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
