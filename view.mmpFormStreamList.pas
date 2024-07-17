{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
  private
    FOnExport:  TNotifyEvent;
    FSegments:  TObjectList<TSegment>;
    function    getStreamInfo(const aMediaFilePath: string): integer;
    function    updateExportButton(aEnabled: boolean): boolean;
    function    updateStreamsCaption: boolean;
  protected
    function    applySegments(const aSegments: TObjectList<TSegment>): boolean;
    procedure   createParams(var Params: TCreateParams);
  public
    property onExport: TNotifyEvent read FOnExport write FOnExport;
  end;

function mmpApplySegments(const aSegments: TObjectList<TSegment>): boolean;
function mmpRefreshStreamInfo(const aMediaFilePath: string): boolean;
function mmpShowStreamList(const aPt: TPoint; const aHeight: integer; aExportEvent: TNotifyEvent; const bCreateNew: boolean = TRUE): boolean;
function mmpShutStreamList: boolean;

implementation

uses
  mmpConsts, mmpFormatting, mmpGlobalState, mmpKeyboardUtils, mmpMarkDownUtils,
  view.mmpFormTimeline,
  model.mmpMediaInfo,
  _debugWindow;

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

  winApi.windows.setWindowPos(gStreamListForm.handle, HWND_TOP, aPt.X, aPt.Y - gStreamListForm.height, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  notifyApp(newNotice(evGSShowingStreamlist, TRUE));
  notifyApp(newNotice(evGSWidthStreamlist, gStreamListForm.width));
end;

function mmpShutStreamList: boolean;
begin
  case gStreamListForm <> NIL of TRUE: begin gStreamListForm.close; gStreamListForm.free; gStreamListForm := NIL; end;end;
  notifyApp(newNotice(evGSShowingStreamlist, FALSE));
  notifyApp(newNotice(evGSWidthStreamlist, 0));
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
  clStreams.color                              := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.focusedColor  := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.hotColor      := DARK_MODE_LIGHT;
  clStreams.itemSelectionOptions.selectedColor := DARK_MODE_LIGHT;

  SELF.width  := 460;
  SELF.height := 400;

  pageControl.tabWidth := 0; // tab widths are controlled by the width of the captions
  tsSegments.caption := '        Segments        ';
  tsStreams.caption  := '        Streams        ';
  tsOptions.caption  := '         Help          ';

  btnExport.left := (pnlButtons.width div 2) - (btnExport.width div 2);

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  font.color        := DARK_MODE_SILVER;

  md.align := alClient;
  initMarkDownViewer(md);

  loadMarkDownFromResource(md, 'Resource_mdEditing');

  clSegments.itemCount := 0;
  clStreams.itemCount  := 0;
end;

function TStreamListForm.getStreamInfo(const aMediaFilePath: string): integer;
begin
  result := -1;

  clStreams.itemCount  := 0;

  MI.getMediaInfo(aMediaFilePath, mtVideo);
  updateStreamsCaption;
  clStreams.itemCount := MI.mediaStreams.count;
end;

function TStreamListForm.updateExportButton(aEnabled: boolean): boolean;
begin
  btnExport.enabled := aEnabled;
end;

function TStreamListForm.updateStreamsCaption: boolean;
begin
  tsStreams.caption := format('          Streams %d/%d          ', [MI.selectedCount, MI.streamCount]);
end;

initialization

finalization
  case gStreamListForm <> NIL of TRUE: begin gStreamListForm.close; gStreamListForm.free; gStreamListForm := NIL; end;end;

end.
