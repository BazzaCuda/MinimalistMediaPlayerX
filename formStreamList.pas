{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit formStreamList;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ControlList, generics.collections, TSegmentClass,
  System.ImageList, Vcl.ImgList, Vcl.Buttons;

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
    tsLog: TTabSheet;
    lblTitle: TLabel;
    procedure formCreate(Sender: TObject);
    procedure formClose(Sender: TObject; var Action: TCloseAction);
    procedure clSegmentsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
    procedure clStreamsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
    procedure clStreamsItemClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure clSegmentsItemClick(Sender: TObject);
    procedure btnExportMouseEnter(Sender: TObject);
    procedure btnExportMouseLeave(Sender: TObject);
    procedure btnExportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    FOnExport: TNotifyEvent;
    FSegments: TObjectList<TSegment>;
    function getStreamInfo(const aMediaFilePath: string): integer;
    function updateExportButton(aEnabled: boolean): boolean;
    function updateStreamsCaption: boolean;
  protected
    function applySegments(const aSegments: TObjectList<TSegment>): boolean;
    procedure createParams(var Params: TCreateParams);
  public
    property onExport: TNotifyEvent read FOnExport write FOnExport;
  end;

function applySegments(const aSegments: TObjectList<TSegment>): boolean;
function refreshStreamInfo(const aMediaFilePath: string): boolean;
function showStreamList(const Pt: TPoint; const aHeight: integer; aExportEvent: TNotifyEvent; const createNew: boolean = TRUE): boolean;
function shutStreamList: boolean;

implementation

uses consts, TCommonUtilsClass, TMediaInfoClass, system.generics.defaults, formTimeline, _debugWindow;

var
  streamListForm: TStreamListForm;

function ctrlKeyDown: boolean;
begin
  result := GetKeyState(VK_CONTROL) < 0;
end;

function refreshStreamInfo(const aMediaFilePath: string): boolean;
begin
  MI.lowestID := streamListForm.getStreamInfo(aMediaFilePath);
end;

function showStreamList(const Pt: TPoint; const aHeight: integer; aExportEvent: TNotifyEvent; const createNew: boolean = TRUE): boolean;
begin
  case (streamListForm = NIL) and createNew of TRUE: streamListForm := TStreamListForm.create(NIL); end;
  case streamListForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current StreamList window. Used for repositioning the window when the main UI moves or resizes.

  screen.cursor := crDefault;

  streamListForm.pageControl.pages[0].show;
  streamListForm.show;
  streamListForm.onExport := aExportEvent;

  winAPI.Windows.setWindowPos(streamListForm.handle, HWND_TOP, Pt.X, Pt.Y - streamListForm.height, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
end;

function shutStreamList: boolean;
begin
  case streamListForm <> NIL of TRUE: begin streamListForm.close; streamListForm.free; streamListForm := NIL; end;end;
  streamListForm := NIL;
end;

function applySegments(const aSegments: TObjectList<TSegment>): boolean;
begin
  streamListForm.applySegments(aSegments);
end;

{$R *.dfm}

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
  case ctrlKeyDown of  TRUE: btnExport.caption := 'Join';
                      FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.btnExportMouseLeave(Sender: TObject);
begin
  btnExport.caption := 'Export';
end;

procedure TStreamListForm.btnExportMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case ctrlKeyDown of  TRUE: btnExport.caption := 'Join';
                      FALSE: btnExport.caption := 'Export'; end;
end;

procedure TStreamListForm.clSegmentsBeforeDrawItem(aIndex: Integer; aCanvas: TCanvas; aRect: TRect; aState: TOwnerDrawState);
begin
  lblSegID.caption        := FSegments[aIndex].segID; //   format('%.2d', [aIndex + 1]);
  lblSegDetails.caption   := format('%ds - %ds', [FSegments[aIndex].startSS, FSegments[aIndex].EndSS]);
  lblDuration.caption     := format('Duration: %d secs (%s)', [FSegments[aIndex].EndSS - FSegments[aIndex].startSS, CU.formatSeconds(FSegments[aIndex].EndSS - FSegments[aIndex].startSS)]);
  lblTitle.caption        := FSegments[aIndex].title;
  shape1.brush.color      := FSegments[aIndex].color;
  shape1.brush.style      := bsSolid;
  shape1.pen.color        := $7E7E7E;
  shape1.margins.top      := 1;
  shape1.margins.bottom   := 1;
  shape1.alignWithMargins := TRUE;
  imgTrashCan.visible     := FSegments[aIndex].deleted;
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

  case MI.mediaStreams[aIndex].selected of  TRUE: begin lblStream.font.color := clSilver; lblStream.font.style := [fsBold]; end;
                                           FALSE: begin lblStream.font.color := clDkGray; lblStream.font.style := [fsItalic]; end;end;

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

procedure TStreamListForm.createParams(var Params: TCreateParams);
// no taskbar icon for this window
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := SELF.Handle; // normally application.handle
end;

procedure TStreamListForm.formClose(Sender: TObject; var Action: TCloseAction);
begin
  streamListForm.free; streamListForm := NIL;
end;

procedure TStreamListForm.formCreate(Sender: TObject);
begin
  clSegments.borderStyle       := bsNone;
  clSegments.styleElements     := []; // don't allow any theme alterations
  clSegments.color := $232323;
  clSegments.itemSelectionOptions.focusedColor  := $232323;
  clSegments.itemSelectionOptions.hotColor      := $232323;
  clSegments.itemSelectionOptions.selectedColor := $232323;
  clStreams.borderStyle        := bsNone;
  clStreams.styleElements     := []; // don't allow any theme alterations
  clStreams.color := $232323;
  clStreams.itemSelectionOptions.focusedColor  := $232323;
  clStreams.itemSelectionOptions.hotColor      := $232323;
  clStreams.itemSelectionOptions.selectedColor := $232323;

  SELF.width  := 556;
  SELF.height := 600;

  pageControl.tabWidth := 0; // tab widths are controlled by the width of the captions
  tsSegments.caption := '          Segments          ';
  tsStreams.caption  := '          Streams          ';
  tsOptions.caption  := '          Options          ';
  tsLog.caption      := '      Log      ';


  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  font.color        := clSilver;

  clSegments.itemCount := 0;
  clStreams.itemCount  := 0;

//  MI.lowestID := getStreamInfo;
end;

function TStreamListForm.getStreamInfo(const aMediaFilePath: string): integer;
begin
  result := -1;

  clStreams.itemCount  := 0;

  MI.initMediaInfo(aMediaFilePath);
  updateStreamsCaption;
  clStreams.itemCount := MI.mediaStreams.count;
  MI.mediaStreams.sort(TComparer<TMediaStream>.construct(
      function (const L, R: TMediaStream): integer
      begin
         case length(L.ID) = 1 of TRUE: L.ID := '0' + L.ID; end;
         case length(R.ID) = 1 of TRUE: R.ID := '0' + R.ID; end;
         result := compareText(L.ID, R.ID)
      end
      ));
  case MI.mediaStreams.count > 0 of TRUE: case tryStrToInt(MI.mediaStreams[0].ID, result) of FALSE: result := 0; end;end;

  updateExportButton(MI.selectedCount > 0);
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
  streamListForm := NIL;

finalization
  case streamListForm <> NIL of TRUE: begin streamListForm.close; streamListForm.free; streamListForm := NIL; end;end;

end.
