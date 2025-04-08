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
unit view.mmpFormStreamList;

interface

uses
  winApi.messages, winapi.Windows,
  system.classes, system.generics.collections, system.imageList, system.sysUtils, system.variants,
  vcl.buttons, vcl.comCtrls, vcl.controlList, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.imgList, vcl.stdCtrls,
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  TSegmentClass;

type
  TStreamListForm = class(TForm)
    backPanel: TPanel;
    pageControl: TPageControl;
    tsSegments: TTabSheet;
    tsStreams: TTabSheet;
    clSegments: TControlList;
    clStreams: TControlList;
    Label1: TLabel;
    Label2: TLabel;
    lblSegDetails: TLabel;
    lblDuration: TLabel;
    Shape1: TShape;
    lblSegID: TLabel;
    imgTrashCan: TImage;
    Shape2: TShape;
    lblStream: TLabel;
    imgIcon: TImage;
    imageList: TImageList;
    lblStreamID: TLabel;
    pnlButtons: TPanel;
    btnExport: TBitBtn;
    tsOptions: TTabSheet;
    lblTitle: TLabel;
    md: TMarkdownViewer;
    procedure formCreate(Sender: TObject);
    procedure clSegmentsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
    procedure clSegmentsItemClick(Sender: TObject);
    procedure clStreamsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
    procedure clStreamsItemClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnExportMouseEnter(Sender: TObject);
    procedure btnExportMouseLeave(Sender: TObject);
    procedure btnExportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure pageControlChange(Sender: TObject);
  private
    FOnExport:  TNotifyEvent;
    FSegments:  TObjectList<TSegment>;
    function    getStreamInfo(const aMediaFilePath: string): integer;
    function    updateExportButton(aEnabled: boolean): boolean;
    function    updateStreamsCaption: boolean;
  protected
    function    applySegments(const aSegments: TObjectList<TSegment>): boolean;
    procedure   createParams(var Params: TCreateParams);
    procedure   WMNCHitTest       (var msg: TWMNCHitTest);  message WM_NCHITTEST;
    procedure   WMSizing          (var msg: TMessage);      message WM_SIZING;
    procedure   WMSize            (var msg: TWMSize);       message WM_SIZE;
  public
    property onExport: TNotifyEvent read FOnExport write FOnExport;
  end;

function mmpApplySegments(const aSegments: TObjectList<TSegment>): boolean;
function mmpRefreshStreamInfo(const aMediaFilePath: string): boolean;
function mmpShowStreamList(const aPt: TPoint; const aHeight: integer; aExportEvent: TNotifyEvent; const bCreateNew: boolean = TRUE): boolean;
function mmpShutStreamList: boolean;

implementation

uses
  system.math,
  mmpConsts, mmpFormatting, mmpDoProcs, mmpGlobalState, mmpKeyboardUtils, mmpMarkDownUtils,
  view.mmpFormTimeline, view.mmpThemeUtils,
  model.mmpMediaInfo,
  _debugWindow;

const
  DEFAULT_WIDTH  = 460;
  DEFAULT_HEIGHT = 400;

var gStreamListForm: TStreamListForm = NIL;

function mmpApplySegments(const aSegments: TObjectList<TSegment>): boolean;
begin
  gStreamListForm.applySegments(aSegments);
end;

function mmpRefreshStreamInfo(const aMediaFilePath: string): boolean;
begin
  gStreamListForm.getStreamInfo(aMediaFilePath);
  gStreamListForm.updateExportButton(MI.selectedCount > 0);
end;

function mmpShowStreamList(const aPt: TPoint; const aHeight: integer; aExportEvent: TNotifyEvent; const bCreateNew: boolean = TRUE): boolean;
begin
  case (gStreamListForm = NIL) and bCreateNew of TRUE: gStreamListForm := TStreamListForm.create(NIL); end;
  case gStreamListForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current StreamList window. Used for repositioning the window when the main UI moves or resizes.

  screen.cursor := crDefault;

  case GS.showingStreamList of FALSE: gStreamListForm.pageControl.pages[0].show; end; // first time only
  gStreamListForm.show;
  gStreamListForm.onExport := aExportEvent;

  // When the main window is resized or moved, force a WM_SIZE message to be generated without actually changing the size of the form
  winApi.windows.setWindowPos(gStreamListForm.handle, HWND_TOP, aPt.X, aPt.Y - gStreamListForm.height, gStreamListForm.width, gStreamListForm.height + 1, SWP_SHOWWINDOW); // + SWP_NOSIZE);
  winApi.windows.setWindowPos(gStreamListForm.handle, HWND_TOP, aPt.X, aPt.Y - gStreamListForm.height, gStreamListForm.width, gStreamListForm.height - 1, SWP_SHOWWINDOW); // + SWP_NOSIZE);
//  gStreamListForm.backPanel.invalidate; // force align := alClient

  mmpDo(evGSShowingStreamlist, TRUE);
  mmpDo(evGSWidthStreamlist, gStreamListForm.width);
end;

function mmpShutStreamList: boolean;
begin
  case gStreamListForm <> NIL of TRUE: begin gStreamListForm.close; gStreamListForm.free; gStreamListForm := NIL; end;end;
  mmpDo(evGSShowingStreamlist, FALSE);
  mmpDo(evGSWidthStreamlist, 0);
end;

{$R *.dfm}

{ TStreamListForm }

function TStreamListForm.applySegments(const aSegments: TObjectList<TSegment>): boolean;
begin
  FSegments := aSegments;
  clSegments.itemCount := 0;
  clSegments.itemCount := aSegments.count;
end;

procedure TStreamListForm.btnExportClick(Sender: TObject);
begin
  case assigned(FOnExport) of TRUE: FOnExport(NIL); end;
  focusTimeline;
end;

procedure TStreamListForm.btnExportMouseEnter(Sender: TObject);
begin
  case mmpCtrlKeyDown of   TRUE: btnExport.caption := 'Join';
                          FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.btnExportMouseLeave(Sender: TObject);
begin
  btnExport.caption := 'Export';
end;

procedure TStreamListForm.btnExportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case mmpCtrlKeyDown of   TRUE: btnExport.caption := 'Join';
                          FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.clSegmentsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
begin
  lblSegID.caption        := FSegments[aIndex].segID; //   format('%.2d', [aIndex + 1]);
  lblSegDetails.caption   := format('%ds - %ds', [FSegments[aIndex].startSS, FSegments[aIndex].EndSS]);
  lblDuration.caption     := format('Duration: %d secs (%s)', [FSegments[aIndex].EndSS - FSegments[aIndex].startSS, mmpFormatSeconds(FSegments[aIndex].EndSS - FSegments[aIndex].startSS)]);
  lblTitle.caption        := FSegments[aIndex].title;
  shape1.brush.color      := FSegments[aIndex].color;
  shape1.brush.style      := bsSolid;
  shape1.pen.color        := $7E7E7E;
  shape1.margins.top      := 1;
  shape1.margins.bottom   := 1;
  shape1.alignWithMargins := TRUE;
  imgTrashCan.visible     := FSegments[aIndex].deleted;
  imgTrashCan.left        := clSegments.width - imgTrashCan.width - 8;
end;

procedure TStreamListForm.clSegmentsItemClick(Sender: TObject);
begin
  TL.segments[clSegments.ItemIndex].setAsSelSeg;
  focusTimeline;
end;

procedure TStreamListForm.clStreamsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
begin
  imgIcon.picture.bitmap:= NIL;
  case MI.mediaStreams[aIndex].selected of  TRUE: imageList.getBitmap(MI.mediaStreams[aIndex].iconIx, imgIcon.picture.bitmap);
                                           FALSE: imageList.getBitmap(MI.mediaStreams[aIndex].iconIx + 1, imgIcon.picture.bitmap); end;

  case MI.mediaStreams[aIndex].selected of  TRUE: begin lblStream.font.color := DARK_MODE_SILVER; lblStream.font.style := [fsBold]; end;
                                           FALSE: begin lblStream.font.color := DARK_MODE_DKGRAY; lblStream.font.style := [fsItalic]; end;end;

  lblStreamID.caption := MI.mediaStreams[aIndex].ID;

  lblStream.caption := 'format: '      + MI.mediaStreams[aIndex].format
                     + '  duration: '  + MI.mediaStreams[aIndex].duration
                     + '  bitrate: '   + MI.mediaStreams[aIndex].bitRate + #13#10
                     + 'title: '       + MI.mediaStreams[aIndex].title
                     + '  language: '  + MI.mediaStreams[aIndex].language + #13#10
                     + 'info: '        + MI.mediaStreams[aIndex].info;
end;

procedure TStreamListForm.clStreamsItemClick(Sender: TObject);
begin
  MI.mediaStreams[clStreams.itemIndex].selected := NOT MI.mediaStreams[clStreams.itemIndex].selected;
  clStreams.itemIndex := -1; // otherwise, TControlList won't let you click the same item twice in succession!
  updateStreamsCaption;
  updateExportButton(MI.selectedCount > 0);
  focusTimeline;
end;

procedure TStreamListForm.createParams(var params: TCreateParams);
// no taskbar icon for this window
begin
  inherited;
  params.ExStyle    := params.ExStyle or (WS_EX_APPWINDOW);
  params.WndParent  := SELF.Handle; // normally application.handle
end;

procedure TStreamListForm.formCreate(Sender: TObject);
begin
  clSegments.borderStyle       := bsNone;
  clSegments.styleElements     := []; // don't allow any theme alterations
  clSegments.color                              := DARK_MODE_LIGHT;
  clSegments.itemSelectionOptions.focusedColor  := DARK_MODE_LIGHT;
  clSegments.itemSelectionOptions.hotColor      := DARK_MODE_LIGHT;
  clSegments.itemSelectionOptions.selectedColor := DARK_MODE_LIGHT;
  clStreams.borderStyle        := bsNone;
  clStreams.styleElements      := []; // don't allow any theme alterations
  clStreams.color                               := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.focusedColor   := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.hotColor       := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.selectedColor  := DARK_MODE_LIGHT;

  SELF.width  := DEFAULT_WIDTH;
  SELF.height := DEFAULT_HEIGHT;

  pageControl.tabWidth := 0; // tab widths are controlled by the width of the captions
  tsSegments.caption := '        Segments        ';
  tsStreams.caption  := '        Streams        ';
  tsOptions.caption  := '         Help          ';

  btnExport.left := (pnlButtons.width div 2) - (btnExport.width div 2);

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;

  styleElements     := []; // don't allow any theme alterations
  mmpSetGlassFrame(SELF);
  mmpSetCustomTitleBar(SELF, 2); // give the user a more substantial top edge for resizing
  font.color        := DARK_MODE_SILVER;

  md.align := alClient;
  initMarkDownViewer(md);

  loadMarkDownFromResource(md, 'Resource_mdEditing');

  clSegments.itemCount := 0;
  clStreams.itemCount  := 0;
end;

procedure TStreamListForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  key := 0;
  focusTimeline;
end;

function TStreamListForm.getStreamInfo(const aMediaFilePath: string): integer;
begin
  result := -1;

  clStreams.itemCount  := 0;

  MI.getMediaInfo(aMediaFilePath, mtVideo);
  updateStreamsCaption;
  clStreams.itemCount := MI.mediaStreams.count;
end;

procedure TStreamListForm.pageControlChange(Sender: TObject);
begin
  focusTimeline;
end;

function TStreamListForm.updateExportButton(aEnabled: boolean): boolean;
begin
  btnExport.enabled := aEnabled;
end;

function TStreamListForm.updateStreamsCaption: boolean;
begin
  tsStreams.caption := format('          Streams %d/%d          ', [MI.selectedCount, MI.streamCount]);
end;

procedure TStreamListForm.WMNCHitTest(var msg: TWMNCHitTest);
begin
  // Prevent the cursor from changing when hovering over the side edges or bottom edge
//  case (msg.result = HTTOP) of FALSE: msg.result := HTTOP; end;
  msg.result := HTTOP;
end;

procedure TStreamListForm.WMSize(var msg: TWMSize); // called when setWindowPos is called without SWP_NOMOVE
begin
  case msg.height > GS.mainForm.height  of TRUE: setWindowPos(SELF.handle, HWND_TOP, 0, 0, SELF.width, GS.mainForm.height,  SWP_NOMOVE); end;
  case msg.height < DEFAULT_HEIGHT      of TRUE: setWindowPos(SELF.handle, HWND_TOP, 0, 0, SELF.width, DEFAULT_HEIGHT,      SWP_NOMOVE); end;
  backPanel.height := SELF.height; // force align := alClient
end;

procedure TStreamListForm.WMSizing(var msg: TMessage); // called when the user is manually resizing the form
// restricts the horizontal resizing by modifying the right edge of the resizing rectangle to ensure that the window's width remains constant.
// The user can control the height of a video - the app controls the width.
begin
  inherited;
  // Prevent horizontal resizing by adjusting the rectangle's left and right edges
  var vNewRect := PRect(msg.LParam);
//  newRect^.right := newRect^.left + width; // redundant: WMNCHitTest prevents width resizing
  case vNewRect^.top < GS.mainForm.top of TRUE: vNewRect^.top := GS.mainForm.top; end; // don't use height
  case vNewRect^.top > (SELF.top + SELF.height) - DEFAULT_HEIGHT of TRUE: vNewRect^.top := (SELF.top + SELF.height) - DEFAULT_HEIGHT; end;
  backPanel.height := vNewRect^.height; // force align := alClient
end;

initialization

finalization
  case gStreamListForm <> NIL of TRUE: begin gStreamListForm.close; gStreamListForm.free; gStreamListForm := NIL; end;end;

end.
