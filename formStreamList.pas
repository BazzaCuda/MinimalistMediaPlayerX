{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ControlList, generics.collections,
  formTimeline;

type
  TStreamListForm = class(TForm)
    backPanel: TPanel;
    PageControl1: TPageControl;
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
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure clSegmentsBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
  private
    FSegments: TObjectList<TSegment>;
  protected
    function applySegments(const aSegments: TObjectList<TSegment>): boolean;
    procedure CreateParams(var Params: TCreateParams);
  public
  end;

function applySegments(const aSegments: TObjectList<TSegment>): boolean;
function showStreamList(const Pt: TPoint; const aHeight: integer; const createNew: boolean = TRUE): boolean;
function showingStreamList: boolean;
function shutStreamList: boolean;

implementation

uses consts, commonUtils, _debugWindow;

var
  streamListForm: TStreamListForm;

function showingStreamList: boolean;
begin
  result := StreamListForm <> NIL;
end;

function showStreamList(const Pt: TPoint; const aHeight: integer; const createNew: boolean = TRUE): boolean;
begin
  case (StreamListForm = NIL) and createNew of TRUE: StreamListForm := TStreamListForm.create(NIL); end;
  case StreamListForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current StreamList window. Used for repositioning the window when the main UI moves or resizes.

//  case aHeight > UI_DEFAULT_AUDIO_HEIGHT of TRUE: StreamListForm.height := aHeight; end;
  screen.cursor := crDefault;

  StreamListForm.show;

  winAPI.Windows.setWindowPos(StreamListForm.handle, HWND_TOP, Pt.X, Pt.Y - streamListForm.height, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
end;

function shutStreamList: boolean;
begin
  case StreamListForm <> NIL of TRUE: begin StreamListForm.close; StreamListForm.free; StreamListForm := NIL; end;end;
  StreamListForm := NIL;
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

procedure TStreamListForm.clSegmentsBeforeDrawItem(AIndex: Integer; ACanvas: TCanvas; ARect: TRect; AState: TOwnerDrawState);
begin
  lblSegID.caption        := format('%d', [AIndex + 1]);
  lblSegDetails.caption   := format('%ds - %ds', [FSegments[aIndex].startSS, FSegments[aIndex].EndSS]);
  lblDuration.caption     := format('Duration: %d secs (%s)', [FSegments[aIndex].EndSS - FSegments[aIndex].startSS, CU.formatSeconds(FSegments[aIndex].EndSS - FSegments[aIndex].startSS)]);
  shape1.brush.color      := FSegments[aIndex].color;
  shape1.brush.style      := bsSolid;
  shape1.pen.color        := $7E7E7E;
  shape1.margins.top      := 1;
  shape1.margins.bottom   := 1;
  shape1.alignWithMargins := TRUE;
  imgTrashCan.visible     := FSegments[aIndex].deleted;
end;

procedure TStreamListForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := self.Handle; // normally application.handle
end;

procedure TStreamListForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StreamListForm.free; StreamListForm := NIL;
end;

procedure TStreamListForm.FormCreate(Sender: TObject);
begin
  clSegments.borderStyle       := bsNone;
  clSegments.styleElements     := []; // don't allow any theme alterations
  clSegments.color := $232323;
  clStreams.borderStyle        := bsNone;
  clStreams.styleElements     := []; // don't allow any theme alterations
  clStreams.color := $232323;

  SELF.width  := 556;
  SELF.height := 600;

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  font.color        := clSilver;

  clSegments.ItemCount := 0;
end;

initialization
  StreamListForm := NIL;

finalization
  case StreamListForm <> NIL of TRUE: begin StreamListForm.close; StreamListForm.free; StreamListForm := NIL; end;end;

end.
