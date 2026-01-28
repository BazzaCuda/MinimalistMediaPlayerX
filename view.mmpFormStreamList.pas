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
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winapi.Windows,
  system.classes, system.generics.collections, system.imageList, system.sysUtils, system.variants,
  vcl.buttons, vcl.comCtrls, vcl.controlList, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.imgList, vcl.stdCtrls,
  {$endif}
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts,
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
    tsHelp: TTabSheet;
    lblTitle: TLabel;
    md: TMarkdownViewer;
    lblSegments: TLabel;
    lblExportSS: TLabel;
    lblTotalSS: TLabel;
    lblExport: TLabel;
    lblTotal: TLabel;
    tsOptions: TTabSheet;
    chbChaptersShow: TCheckBox;
    chbChaptersAudioWrite: TCheckBox;
    chbChaptersVideoWrite: TCheckBox;
    chbKeyframes: TCheckBox;
    chbPlayEdited: TCheckBox;
    Label59: TLabel;
    chbExportJoinOnly: TCheckBox;
    Label3: TLabel;
    Bevel1: TBevel;
    shpBracket1: TShape;
    shpBracket2: TShape;
    lblConfigLink1: TLabel;
    lblConfig1: TLabel;
    lblConfig2: TLabel;
    lblConfig3: TLabel;
    lblConfigLink2: TLabel;
    lblConfigLink3: TLabel;
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
    procedure btnExportKeyPress(Sender: TObject; var Key: Char);
    procedure pageControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnExportKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnExportKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure clSegmentsItemDblClick(Sender: TObject);
    procedure clSegmentsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chbChaptersShowClick(Sender: TObject);
    procedure chbChaptersAudioWriteClick(Sender: TObject);
    procedure chbChaptersVideoWriteClick(Sender: TObject);
    procedure chbKeyframesClick(Sender: TObject);
    procedure chbPlayEditedClick(Sender: TObject);
    procedure chbExportJoinOnlyClick(Sender: TObject);
    procedure lblConfigLink1Click(Sender: TObject);
  private
    FMediaType:         TMediaType;
    FOnExport:          TNotifyEvent;
    FOnRedraw:          TNotifyEvent;
    FOnToggleKeyframes: TNotifyEvent;
    function    getStreamInfo(const aMediaFilePath: string): integer;
    function    updateExportButton(aEnabled: boolean): boolean;
    function    updateStreamsCaption: boolean;
  protected
    function    applySegments(const aSegments: TObjectList<TSegment>; const aMax: integer; const bResetHeight: boolean = FALSE): boolean;
    function    updateTotals(const aSegments: TObjectList<TSegment>; const aMax: integer): boolean;
    procedure   createParams(var Params: TCreateParams);
    function    scrollTo(const aIx: integer): boolean;
    procedure   WMNCHitTest       (var msg: TWMNCHitTest);  message WM_NCHITTEST;
    procedure   WMSizing          (var msg: TMessage);      message WM_SIZING;
    procedure   WMSize            (var msg: TWMSize);       message WM_SIZE;
  public
    property onExport:          TNotifyEvent read FOnExport write FOnExport;
    property onRedraw:          TNotifyEvent read FOnRedraw write FOnRedraw;
    property onToggleKeyframes: TNotifyEvent read FOnToggleKeyframes write FOnToggleKeyframes;
  end;

function mmpApplySegments(const aSegments: TObjectList<TSegment>; const aMax: integer; const bResetHeight: boolean = FALSE): boolean;
function mmpRefreshStreamInfo(const aMediaFilePath: string): boolean;
function mmpShowStreamList(const aPt: TPoint; const aHeight: integer; const aRedrawEvent: TNotifyEvent; const aExportEvent: TNotifyEvent; const aToggleKeyframesEvent: TNotifyEvent; const bCreateNew: boolean = TRUE): boolean;
function mmpShutStreamList: boolean;
function mmpScrollTo(const aIx: integer): boolean;

implementation

uses
  system.math,
  bazCmd,
  mmpFormatting, mmpFormInputBox,  mmpGlobalState, mmpKeyboardUtils, mmpMarkDownUtils,
  view.mmpFormTimeline, view.mmpThemeUtils,
  model.mmpConfigFile, model.mmpMediaInfo,
  _debugWindow;

const
  DEFAULT_WIDTH  = 460;
  DEFAULT_HEIGHT = 400;

var gStreamListForm: TStreamListForm = NIL;

function mmpApplySegments(const aSegments: TObjectList<TSegment>; const aMax: integer; const bResetHeight: boolean = FALSE): boolean;
begin
  gStreamListForm.applySegments(aSegments, aMax, bResetHeight);
end;

function mmpRefreshStreamInfo(const aMediaFilePath: string): boolean;
begin
  gStreamListForm.getStreamInfo(aMediaFilePath);
  gStreamListForm.updateExportButton(MI.selectedCount > 0);
end;

function mmpShowStreamList(const aPt: TPoint; const aHeight: integer; const aRedrawEvent: TNotifyEvent; const aExportEvent: TNotifyEvent; const aToggleKeyframesEvent: TNotifyEvent; const bCreateNew: boolean = TRUE): boolean;
begin
  case (gStreamListForm = NIL) and bCreateNew of TRUE: begin gStreamListForm := TStreamListForm.create(NIL); end;end;
  case gStreamListForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current StreamList window. Used for repositioning the window when the main UI moves or resizes.

  screen.cursor := crDefault;

  case GS.showingStreamList of FALSE: gStreamListForm.pageControl.pages[0].show; end; // first time only
  gStreamListForm.show;
  gStreamListForm.onExport          := aExportEvent;
  gStreamListForm.onRedraw          := aRedrawEvent;
  gStreamListForm.onToggleKeyframes := aToggleKeyframesEvent;

  // When the main window is resized or moved, force a WM_SIZE message to be generated without actually changing the size of the form
  winApi.windows.setWindowPos(gStreamListForm.handle, HWND_TOP, aPt.X, aPt.Y - gStreamListForm.height, gStreamListForm.width, gStreamListForm.height + 1, SWP_SHOWWINDOW);
  winApi.windows.setWindowPos(gStreamListForm.handle, HWND_TOP, aPt.X, aPt.Y - gStreamListForm.height, gStreamListForm.width, gStreamListForm.height - 1, SWP_SHOWWINDOW);

  mmp.cmd(evGSShowingStreamlist, TRUE);
  mmp.cmd(evGSWidthStreamlist, gStreamListForm.width);

  focusTimeline;
end;

function mmpShutStreamList: boolean;
begin
  case gStreamListForm <> NIL of TRUE: begin gStreamListForm.close; gStreamListForm.free; gStreamListForm := NIL; end;end;
  mmp.cmd(evGSShowingStreamlist, FALSE);
  mmp.cmd(evGSWidthStreamlist, 0);
end;

function mmpScrollTo(const aIx: integer): boolean;
begin
  gStreamListForm.scrollTo(aIx);
end;

{$R *.dfm}

{ TStreamListForm }

function TStreamListForm.applySegments(const aSegments: TObjectList<TSegment>; const aMax: integer; const bResetHeight: boolean = FALSE): boolean;
begin
//  {$if BazDebugWindow} debugFormat('applySegments: %d', [aMax]); {$endif}

  clSegments.itemCount := 0;
  clSegments.itemCount := aSegments.count;

  updateTotals(aSegments, aMax);

  FMediaType := GS.mediaType;
  case bResetHeight of TRUE: SELF.height := DEFAULT_HEIGHT; end;
  case FMediaType = mtVideo of FALSE: EXIT; end; // don't break the audio editor

  while (clSegments.height < (clSegments.itemHeight * clSegments.itemCount)) // keep resizing the window while there's enough height left,
  and (SELF.top > (GS.mainForm.top + clSegments.itemHeight)) do begin        // to accommodate as many of the segments as possible (SELF. for clarity only)
                                                                  SELF.height := SELF.height + clSegments.itemHeight;
                                                                  winApi.windows.setWindowPos(HANDLE, HWND_TOP, SELF.left, SELF.top - clSegments.itemHeight, 0, 0, SWP_NOSIZE); // Y - itemHeight
                                                                end;

  focusTimeline;
end;

procedure TStreamListForm.btnExportClick(Sender: TObject);
begin
  case assigned(FOnExport) of TRUE: FOnExport(NIL); end;
// EXPERIMENTAL - if the export succeeds and the user wants the edited file run, it will be sent behind Timeline
// commenting this out doesn't seem to have any detremental effect because all the other keystrokes are already
// calling focusTimeline
//   focusTimeline;
end;

procedure TStreamListForm.btnExportKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  key := 0;
  focusTimeline;
end;

procedure TStreamListForm.btnExportKeyPress(Sender: TObject; var Key: Char);
begin
  key := #0;
  focusTimeline;
end;

procedure TStreamListForm.btnExportKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  key := 0;
  focusTimeline;
end;

procedure TStreamListForm.btnExportMouseEnter(Sender: TObject);
begin
  case mmpCtrlKeyDown of   TRUE: case mmpShiftKeyDown of   TRUE: btnExport.caption := 'Copy';
                                                          FALSE: btnExport.caption := 'Join'; end;
                          FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.btnExportMouseLeave(Sender: TObject);
begin
  btnExport.caption := 'Export';
end;

procedure TStreamListForm.btnExportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case mmpCtrlKeyDown of   TRUE: case mmpShiftKeyDown of   TRUE: btnExport.caption := 'Copy';
                                                          FALSE: btnExport.caption := 'Join'; end;
                          FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.chbChaptersAudioWriteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE] := chbChaptersAudioWrite.checked;
end;

procedure TStreamListForm.chbChaptersShowClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_SHOW] := chbChaptersShow.checked;
end;

procedure TStreamListForm.chbChaptersVideoWriteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE] := chbChaptersVideoWrite.checked;
end;

procedure TStreamListForm.chbExportJoinOnlyClick(Sender: TObject);
begin
  CF.asBoolean[CONF_EXPORT_JOIN_ONLY] := chbExportJoinOnly.checked;
end;

procedure TStreamListForm.chbKeyframesClick(Sender: TObject);
begin
  CF.asBoolean[CONF_KEYFRAMES] := chbKeyframes.checked;
  case assigned(FOnToggleKeyframes) of TRUE: FOnToggleKeyframes(SELF); end;
end;

procedure TStreamListForm.chbPlayEditedClick(Sender: TObject);
begin
  CF.asBoolean[CONF_PLAY_EDITED] := chbPlayEdited.checked;
end;

procedure TStreamListForm.clSegmentsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
begin
  var vSegments := TL.segments;
  case (vSegments.count = 0) or (aIndex > vSegments.count - 1) of TRUE: EXIT; end;
  lblSegID.caption        := vSegments[aIndex].segID;
  lblSegID.font.color     := COLOR_SEGMENT_ID; // Soft gold
  lblSegID.styleElements  := [seBorder];
  lblSegDetails.caption   := format('%ds - %ds', [vSegments[aIndex].startSS, vSegments[aIndex].EndSS]);
  lblDuration.caption     := format('Duration: %d secs (%s)', [vSegments[aIndex].duration, mmpFormatSeconds(vSegments[aIndex].duration)]);
  lblTitle.caption        := vSegments[aIndex].title;
  lblTitle.font.color     := COLOR_SEGMENT_TITLE;
  lblTitle.styleElements  := [seBorder, seClient];
  shape1.brush.color      := vSegments[aIndex].color;
  shape1.brush.style      := bsSolid;
  shape1.pen.color        := $7E7E7E;
  shape1.margins.top      := 1;
  shape1.margins.bottom   := 1;
  shape1.alignWithMargins := TRUE;
  imgTrashCan.visible     := vSegments[aIndex].deleted;
  imgTrashCan.left        := clSegments.width - imgTrashCan.width - 8;
end;

procedure TStreamListForm.clSegmentsItemClick(Sender: TObject);
begin
  TL.segments[clSegments.itemIndex].setAsSelSeg;
  focusTimeline;
end;

procedure TStreamListForm.clSegmentsItemDblClick(Sender: TObject);
begin
  var vS := mmpInputBoxForm(TL.segments[clSegments.itemIndex].title);
  case vS = TL.segments[clSegments.itemIndex].title of FALSE: TL.segments[clSegments.itemIndex].newTitle := vS; end;
end;

procedure TStreamListForm.clSegmentsMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case button = mbRight of TRUE:  begin
                                    var vIx := clSegments.hotItemIndex;
                                    case TL.segments[vIx].deleted of   TRUE: TL.segments[vIx].restore;
                                                                      FALSE: TL.segments[vIx].delete; end;
                                    case assigned(FOnRedraw) of TRUE: FOnRedraw(SELF); end;
                                    focusTimeline; end;end;
end;

procedure TStreamListForm.clStreamsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
begin
  imgIcon.picture.bitmap:= NIL;
  case MI.mediaStreams[aIndex].selected of  TRUE: imageList.getBitmap(MI.mediaStreams[aIndex].iconIx, imgIcon.picture.bitmap);
                                           FALSE: imageList.getBitmap(MI.mediaStreams[aIndex].iconIx + 1, imgIcon.picture.bitmap); end;

  case MI.mediaStreams[aIndex].selected of  TRUE: begin lblStream.font.color := DARK_MODE_SILVER; lblStream.font.style := [fsBold]; end;
                                           FALSE: begin lblStream.font.color := DARK_MODE_DKGRAY; lblStream.font.style := [fsItalic]; end;end;

  lblStreamID.font.color    := COLOR_SEGMENT_ID;
  lblStreamID.StyleElements := [seBorder, seClient];

  lblStreamID.caption := MI.mediaStreams[aIndex].ID;

  case MI.mediaStreams[aIndex].streamType = 'Image' of   TRUE:  lblStream.caption := 'format: '      + MI.mediaStreams[aIndex].format
                                                                                   + ' '             + MI.mediaStreams[aIndex].duration
                                                                                   + ' '             + MI.mediaStreams[aIndex].bitRate + #13#10
                                                                                   + 'title: '       + MI.mediaStreams[aIndex].title
                                                                                   + ''              + MI.mediaStreams[aIndex].language + #13#10
                                                                                   + 'info: '        + MI.mediaStreams[aIndex].info;
                                                        FALSE:  lblStream.caption := 'format: '      + MI.mediaStreams[aIndex].format
                                                                                   + '  duration: '  + MI.mediaStreams[aIndex].duration
                                                                                   + '  bitrate: '   + MI.mediaStreams[aIndex].bitRate + #13#10
                                                                                   + 'title: '       + MI.mediaStreams[aIndex].title
                                                                                   + '  language: '  + MI.mediaStreams[aIndex].language + #13#10
                                                                                   + 'info: '        + MI.mediaStreams[aIndex].info; end;
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

  FMediaType := GS.mediaType;

  SELF.width  := DEFAULT_WIDTH;
  SELF.height := DEFAULT_HEIGHT;

  pageControl.tabWidth  := 0; // tab widths are controlled by the width of the captions
  tsSegments.caption    := '      Segments      ';
  tsStreams.caption     := '      Streams      ';
  tsOptions.caption     := 'Options';
  tsHelp.caption        := '       Help        ';


  btnExport.left := (pnlButtons.width div 2) - (btnExport.width div 2);

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;

  styleElements     := []; // don't allow any theme alterations
  mmpSetGlassFrame(SELF);
  mmpSetCustomTitleBar(SELF, 2); // give the user a more substantial top edge for resizing
  font.color        := DARK_MODE_SILVER;

  md.align := alClient;
  mmpInitMarkDownViewer(md);

  mmpLoadMarkDownFromResource(md, 'resource_mdEditor');

  clSegments.itemCount  := 0;
  clStreams.itemCount   := 0;

  lblExportSS.caption   := EMPTY;
  lblTotalSS.caption    := EMPTY;
  lblExportSS.left      := pnlButtons.width - lblExportSS.width - 6;
  lblTotalSS.Left       := lblExportSS.left;
  lblExport.left        := lblExportSS.left - lblExport.width - 2;
  lblTotal.left         := lblExport.left;

  chbPlayEdited.checked         := CF.asBoolean[CONF_PLAY_EDITED];
  chbKeyframes.checked          := CF.asBoolean[CONF_KEYFRAMES];
  chbChaptersShow.checked       := CF.asBoolean[CONF_CHAPTERS_SHOW];
  chbChaptersAudioWrite.checked := CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE];
  chbChaptersVideoWrite.checked := CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE];
  chbExportJoinOnly.checked     := CF.asBoolean[CONF_EXPORT_JOIN_ONLY];

  shpBracket1.pen.color := $252525;
  shpBracket2.pen.color := $252525;

  lblConfig1.styleElements      := [seBorder, seClient];
  lblConfig1.font.color         := DARK_MODE_DARK;
  lblConfig2.styleElements      := [seBorder, seClient];
  lblConfig2.font.color         := DARK_MODE_DARK;
  lblConfig3.styleElements      := [seBorder, seClient];
  lblConfig3.font.color         := DARK_MODE_DARK;

  lblConfigLink1.styleElements  := [seBorder, seClient];
  lblConfigLink1.font.Color     := clTeal;
  lblConfigLink2.styleElements  := [seBorder, seClient];
  lblConfigLink2.font.Color     := clTeal;
  lblConfigLink3.styleElements  := [seBorder, seClient];
  lblConfigLink3.font.Color     := clTeal;
end;

procedure TStreamListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  key := 0;
  focusTimeline;
end;

procedure TStreamListForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  key := #0;
  focusTimeline;
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

  MI.getMediaInfo(newNotice(aMediaFilePath, mtVideo));
  updateStreamsCaption;
  clStreams.itemCount := MI.mediaStreams.count;
end;

procedure TStreamListForm.lblConfigLink1Click(Sender: TObject);
begin
  mmp.cmd(evVMConfig, TLabel(Sender).caption);
end;

procedure TStreamListForm.pageControlChange(Sender: TObject);
begin
  lblSegments.visible := pageControl.activePage = tsSegments;
  focusTimeline;
end;

procedure TStreamListForm.pageControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  focusTimeline;
end;

function TStreamListForm.scrollTo(const aIx: integer): boolean;
begin
  case clSegments.itemCount = 0 of TRUE: EXIT; end;
  clSegments.itemIndex := aIx;
end;

function TStreamListForm.updateExportButton(aEnabled: boolean): boolean;
begin
  btnExport.enabled := aEnabled;
end;

function TStreamListForm.updateStreamsCaption: boolean;
begin
  tsStreams.caption := format('          Streams %d/%d          ', [MI.selectedCount, MI.avsStreamCount]);
end;

function TStreamListForm.updateTotals(const aSegments: TObjectList<TSegment>; const aMax: integer): boolean;
  function sumExportSS: integer;
  begin
    result := 0;
    case aSegments.count = 0 of TRUE: EXIT; end;
    for var vSegment in aSegments do case vSegment.deleted of FALSE: result := result + vSegment.duration; end;
  end;
begin
  var exportSS := sumExportSS;
  lblExportSS.caption := format('%d secs (%s)', [exportSS, mmpFormatSeconds(exportSS)]);
  lblTotalSS.caption  := format('%d secs (%s)', [aMax, mmpFormatSeconds(aMax)]);
end;

procedure TStreamListForm.WMNCHitTest(var msg: TWMNCHitTest);
begin
  // Prevent the cursor from changing when hovering over the side edges or bottom edge
  // Prevent horizontal resizing by only allowing the top edge to be dragged
  msg.result := HTTOP;
end;

procedure TStreamListForm.WMSize(var msg: TWMSize); // called when setWindowPos is called without SWP_NOSIZE
begin
  case FMediaType = mtVideo of FALSE: EXIT; end; // the following code would break the audio editor
  case msg.height > GS.mainForm.height  of TRUE: setWindowPos(SELF.handle, HWND_TOP, 0, 0, SELF.width, GS.mainForm.height,  SWP_NOMOVE); end;
  case msg.height < DEFAULT_HEIGHT      of TRUE: setWindowPos(SELF.handle, HWND_TOP, 0, 0, SELF.width, DEFAULT_HEIGHT,      SWP_NOMOVE); end;
  backPanel.height := SELF.height; // force align := alClient
end;

procedure TStreamListForm.WMSizing(var msg: TMessage); // called when the user is manually resizing the form
// restricts the horizontal resizing by modifying the right edge of the resizing rectangle to ensure that the window's width remains constant.
// The user can control the height of a video - the app controls the width.
begin
  inherited;

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
