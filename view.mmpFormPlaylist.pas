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
unit view.mmpFormPlaylist;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls, Vcl.AppEvnts,
  mmpAction;

type
  TNotifyIntEvent = procedure(aValue: integer) of object;

  TNoScrollListBox = class(TListBox)
  strict private
  protected
    procedure createParams(var aParams: TCreateParams); override;
  end;

  TScrollDetectListBox = class(TListBox)
  strict private
    FOnScroll: TNotifyIntEvent;
  private
  protected
    procedure wndProc(var aMessage: TMessage); override;
  public
    property onScroll: TNotifyIntEvent read FOnScroll write FOnScroll;
  end;

  IPlaylistForm = interface
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  TPlaylistForm = class(TForm, IPlaylistForm)
    backPanel:          TPanel;
    buttonPanel:        TPanel;
    shiftLabel:         TLabel;
    moveLabel:          TLabel;
    LB: TListBox;
    applicationEvents:  TApplicationEvents;
    lblFolder:          TLabel;
    procedure   applicationEventsMessage(var msg: tagMSG; var handled: Boolean);
    procedure   FormCreate(sender: TObject);
    procedure   LBDblClick(sender: TObject);
    procedure   LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure   LBKeyDn(sender: TObject; var key: Word; Shift: TShiftState);
    procedure   LBKeyPress(sender: TObject; var key: Char);
    procedure   LBKeyUp(sender: TObject; var key: Word; shift: TShiftState);
    procedure   LBMouseEnter(Sender: TObject);
    procedure   LBMouseLeave(Sender: TObject);
  private
    FMouseOver: boolean;
//    LB:         TScrollDetectListBox;
    lbNumbers:  TNoScrollListBox;
    lbDuration: TNoScrollListBox;
    FTopIndex:  integer;
    function    fillNumbers: TVoid;
    function    isItemVisible: boolean;
    function    playItemIndex(const aItemIndex: integer): boolean;
    function    visibleItemCount: integer;
  protected
    procedure   createParams(var params: TCreateParams);
  public
    destructor  Destroy; override;
    function    highlightCurrentItem: boolean;
    function    loadPlaylistBox(const bCaptionOnly: boolean = FALSE): boolean;
    procedure   syncIndexes(aDelta: integer);
  end;
  {$ENDREGION}

implementation

uses
  winApi.shellApi,
  system.strUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpCmd, mmpConsts, mmpGlobalState, mmpKeyboardUtils, mmpUtils,
  viewModel.mmpKeyboardOps,
  _debugWindow;

type
  // can't implement IPlaylistForm with the TForm so we use an intermediary
  TPlayListFormProxy = class(TInterfacedObject, IPlaylistForm)
  strict private
    FListBoxLoaded: boolean;
    FPlaylistForm:  TPlaylistForm;
    FSubscriber:    ISubscriber;
  private
    function    createForm(const bCreateNew: boolean): TPlaylistForm;
    function    moveForm(const wr: TWndRec): boolean;
    function    onNotify(const aNotice: INotice): INotice;
    function    showForm: boolean;
    function    shutForm: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
  end;

function PL: IPlaylistForm;
{$J+} const gPlaylistFormProxy: IPlaylistForm = NIL; {$J-}
begin
  case gPlaylistFormProxy = NIL of TRUE: gPlaylistFormProxy := TPlaylistFormProxy.create; end;
  result := gPlaylistFormProxy;
end;

{$R *.dfm}

procedure TPlaylistForm.applicationEventsMessage(var msg: tagMSG; var handled: boolean);
begin
  case FMouseOver of FALSE: EXIT; end;
  case (msg.message = WM_MOUSEWHEEL) of TRUE: begin
                                                var vDelta := short(hiWord(msg.wParam));
                                                case vDelta > 0 of TRUE: sendMessage(LB.handle, WM_VSCROLL, SB_PAGEUP, 0);    end;
                                                case vDelta < 0 of TRUE: sendMessage(LB.handle, WM_VSCROLL, SB_PAGEDOWN, 0);  end;
                                                end;
                                              end;
end;

procedure TPlaylistForm.createParams(var params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  params.ExStyle    := params.ExStyle OR (WS_EX_APPWINDOW);
  params.WndParent  := SELF.Handle; // normally application.handle
end;

destructor TPlaylistForm.Destroy;
begin
//  LB.onScroll := NIL;
  inherited;
end;

function TPlaylistForm.fillNumbers: TVoid;
begin
  case lbNumbers = NIL of TRUE: EXIT; end;
  lbNumbers.items.clear;
  for var i := 1 to LB.count do
    lbNumbers.items.add(format('%0.3d', [i]));
end;

procedure TPlaylistForm.FormCreate(Sender: TObject);
begin
  LB                  := TScrollDetectListBox.create(SELF);
  LB.parent           := backPanel;
  LB.onDrawItem       := LBDrawItem;

  LB.align            := alClient;
  LB.bevelInner       := bvNone;
  LB.bevelOuter       := bvNone;
  LB.borderStyle      := bsNone;
  LB.margins.top      := 10;
  LB.margins.bottom   := 10;
  LB.margins.left     := 10;
  LB.margins.right    := 10;

  LB.alignWithMargins := TRUE;

  SELF.width  := 556;

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  LB.onDblClick     := LBDblClick;
  LB.onKeyUp        := LBKeyUp;
  LB.onKeyDown      := LBKeyDn;

  lblFolder.margins.bottom    := 4;
  lblFolder.alignWithMargins  := TRUE;
  lblFolder.font.color        := DARK_MODE_SILVER;
  lblFolder.font.style        := [fsBold];
  lblFolder.autoSize          := FALSE;

  // newstyle playlist
  LB.style                    := lbOwnerDrawFixed;
  EXIT;

  LB.margins.left             := 00;
  LB.margins.right            := 00;

  lbNumbers                    := TNoScrollListBox.create(SELF);
  lbNumbers.parent             := backPanel;
  lbNumbers.width              := 25;
  lbNumbers.align              := alLeft;
  lbNumbers.items.add('001');
  lbNumbers.items.add('002');
  lbNumbers.items.add('003');

  lbDuration                  := TNoScrollListBox.create(SELF);
  lbDuration.parent           := backPanel;
  lbDuration.width            := 60;
  lbDuration.align            := alRight;
  lbDuration.items.add('99:99:99');
  lbDuration.items.add('7:22');
  lbDuration.items.add('11:22');

  lbNumbers.style             := lbOwnerDrawFixed;
  lbNumbers.onDrawItem        := LBDrawItem;
  lbDuration.style            := lbOwnerDrawFixed;
  lbDuration.onDrawItem       := LBDrawItem;
  LB.itemHeight               := 13;
  lbNumbers.itemHeight        := 13;
  lbDuration.itemHeight       := 13;
  lbNumbers.bevelInner        := bvNone;
  lbNumbers.bevelOuter        := bvNone;
  lbNumbers.borderStyle       := bsNone;
  lbDuration.style            := lbOwnerDrawFixed;
  lbDuration.bevelInner       := bvNone;
  lbDuration.bevelOuter       := bvNone;
  lbDuration.borderStyle      := bsNone;
  lbNumbers.margins.top       := 10;
  lbNumbers.margins.bottom    := 10;
  lbNumbers.margins.left      := 00;
  lbNumbers.margins.right     := 00;
  lbDuration.margins.top      := 10;
  lbDuration.margins.bottom   := 10;
  lbDuration.margins.left     := 00;
  lbDuration.margins.right    := 10;
  lbNumbers.alignWithMargins  := TRUE;
  lbDuration.alignWithMargins := TRUE;

  lbNumbers.tag   := 1;
  LB.tag          := 2;
  lbDuration.tag  := 3;

//  LB.onScroll     := syncIndexes;
end;

function TPlaylistForm.highlightCurrentItem: boolean;
begin
  var vCurrentIx := mmp.cmd(evPLReqCurrentIx).integer;
  case vCurrentIx = -1 of TRUE: EXIT; end;

  try
    case LB.count > 0 of TRUE:  begin
                                  LB.itemIndex := vCurrentIx;
                                  LB.selected[LB.itemIndex] := TRUE;
                                  case isItemVisible of TRUE: EXIT; end;
                                  var vTopIndex := LB.itemIndex - (visibleItemCount div 2); // try to position it in the middle of the listbox
                                  case vTopIndex >= 0 of  TRUE: LB.topIndex := vTopIndex;
                                                         FALSE: LB.topIndex := 0; end;
                                  syncIndexes(-1); end;end;
  finally
  end;
end;

function TPlaylistForm.isItemVisible: boolean;
var
  vTopIndex, vVisibleItemCount: integer;
begin
  // Get the index of the top visible item
  vTopIndex := LB.topIndex;

  // Calculate the number of items that can fit in the visible area
  vVisibleItemCount := visibleItemCount;

  // Calculate the index of the bottom visible item: (TopIndex + VisibleItemCount - 1)
  result := (LB.itemIndex >= vTopIndex) and (LB.itemIndex <= vTopIndex + vVisibleItemCount - 1);
end;

procedure TPlaylistForm.LBDblClick(sender: TObject);
begin
  playItemIndex(LB.itemIndex);
end;

procedure TPlaylistForm.LBDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var LB:       TListBox := control as TListBox;

  case (index mod 2 = 0) of  TRUE: LB.canvas.brush.color := DARK_MODE_DARK;
                            FALSE: LB.canvas.brush.color := TColor(longint(DARK_MODE_DARK) - $050505);
  end;


  LB.canvas.fillRect(rect);

  case LB.tag = 3 of   TRUE: begin
                                var  vTextWidth := LB.canvas.textWidth(LB.items[index]);
                                LB.canvas.textOut(rect.right - vTextWidth - 4, rect.Top + 2, LB.items[index]); end; // right-justify duration
                      FALSE: LB.canvas.textOut(rect.left + 4, rect.top + 2, LB.items[index]); end;
end;

procedure TPlaylistForm.LBKeyPress(sender: TObject; var key: Char);
begin
  key := #0;
end;

procedure TPlaylistForm.LBKeyDn(sender: TObject; var key: Word; Shift: TShiftState);
begin
  case GS.userInput of TRUE: EXIT; end;
  case key in [VK_LEFT, VK_RIGHT] of TRUE: key := 0; end;
end;

procedure TPlaylistForm.LBKeyUp(sender: TObject; var key: Word; Shift: TShiftState);
begin
  case GS.userInput of TRUE: EXIT; end;
  case key in [VK_LEFT, VK_RIGHT] of TRUE: key := 0; end;
  case (key = VK_RETURN) and NOT mmpCtrlKeyDown of TRUE: playItemIndex(LB.itemIndex); end;
end;

procedure TPlaylistForm.LBMouseEnter(Sender: TObject);
begin
  FMouseOver := TRUE;
end;

procedure TPlaylistForm.LBMouseLeave(Sender: TObject);
begin
  FMouseOver := FALSE;
end;

function TPlaylistForm.loadPlaylistBox(const bCaptionOnly: boolean = FALSE): boolean;
begin
  var vShuffle := mmpIfThenElse(GS.shuffle, 'Shuffle ', EMPTY);
  lblFolder.caption := format('%sFolder: %s', [vShuffle, mmp.cmd(evPLReqCurrentFolder).text]);
  case bCaptionOnly of TRUE: EXIT; end;
  mmp.cmd(evPLFillListBox, LB);
  highlightCurrentItem;
  mmpProcessMessages; // EXPERIMENTAL
end;

function TPlaylistForm.playItemIndex(const aItemIndex: integer): boolean;
begin
  var vThisItem := mmp.cmd(evPLReqThisItem, aItemIndex).text;
  mmp.cmd(evPLFind, vThisItem); // set as current
  mmp.cmd(evVMMPPlayCurrent);
end;

procedure TPlaylistForm.syncIndexes(aDelta: integer);
begin
  case (lbNumbers = NIL) or (lbDuration = NIL) or (LB = NIL) of TRUE: EXIT; end;

  case aDelta = -1 of  TRUE:  begin
                                lbNumbers.topIndex  := LB.topIndex;
                                lbDuration.topIndex := LB.topIndex; end;
                      FALSE:  begin
                                lbNumbers.topIndex  := LB.topIndex + aDelta;
                                lbDuration.topIndex := LB.topIndex + aDelta; end;end;
end;

function TPlaylistForm.visibleItemCount: integer;
begin
  result := LB.clientHeight div LB.itemHeight;
end;

{ TPlayListFormProxy }

constructor TPlayListFormProxy.Create;
begin
  FSubscriber := appEvents.subscribe(newSubscriber(onNotify));
end;

function TPlayListFormProxy.createForm(const bCreateNew: boolean): TPlaylistForm;
begin
  result := FPlaylistForm;
  case bCreateNew of FALSE: EXIT; end;
  result := TPlaylistForm.create(NIL);
end;

destructor TPlayListFormProxy.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
  shutForm;
  inherited;
end;

function TPlayListFormProxy.moveForm(const wr: TWndRec): boolean;
begin
  FPlaylistForm := createForm(wr.createNew);
  case FPlaylistForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current playlist window. Used for repositioning the window when the main UI moves or resizes.
  GS.notify(newNotice(evGSShowingPlaylist, TRUE));
  case wr.height > UI_DEFAULT_AUDIO_HEIGHT of  TRUE: FPlaylistForm.height := wr.height;
                                              FALSE: FPlaylistForm.height := 400; end;
  mmp.cmd(evGSWidthHelp, FPlaylistForm.width);
  screen.cursor := crDefault;

  mmp.cmd(FListBoxLoaded, evNone, evPLFormLoadBox); // do once when the playlist is first opened
  FListBoxLoaded := TRUE;

  mmp.cmd(evPLFormShow);
  mmp.cmd(evPLFormHighlight);

  var vRect: TRect;
  getWindowRect(FPlaylistForm.handle, vRect);

  case (vRect.location.X <> wr.pt.X) or (vRect.location.Y <> wr.pt.Y) of TRUE:
    winAPI.windows.setWindowPos(FPlaylistForm.handle, HWND_TOP, wr.pt.X, wr.pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE); end;
end;

function TPlayListFormProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TPlayListFormProxy.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;

  case aNotice.event of
    evPLFormFillNumbers: FPlaylistForm.fillNumbers;
    evPLFormMove:       moveForm(aNotice.wndRec);
    evPLFormShutForm:   shutForm;
    evPLFormLoadBox:    case FPlaylistForm = NIL of FALSE: begin FPlaylistForm.loadPlaylistBox(aNotice.tf); FPlaylistForm.fillNumbers; end;end;
    evPLFormHighlight:  case FPlaylistForm = NIL of FALSE: FPlaylistForm.highlightCurrentItem; end;
    evPLFormShow:       showForm;
    evPLNewPlaylist:    case FPlaylistForm = NIL of FALSE: begin FPlaylistForm.loadPlaylistBox; FPlaylistForm.fillNumbers; end;end;
  end;
end;

function TPlayListFormProxy.showForm: boolean;
begin
  FPlaylistForm.show;
end;

function TPlayListFormProxy.shutForm: boolean;
begin
  case FPlaylistForm = NIL of TRUE: EXIT; end;
  FPlaylistForm.close;
  FPlaylistForm.free;
  FPlaylistForm := NIL;
  mmp.cmd(evGSWidthHelp, 0);
  GS.notify(newNotice(evGSShowingPlaylist, FALSE));
  FListBoxLoaded := FALSE;
end;

{ TNoScrollListBox }

procedure TNoScrollListBox.createParams(var aParams: TCreateParams);
begin
  inherited createParams(aParams);
  aParams.style := aParams.style and not WS_VSCROLL and not WS_HSCROLL;
end;

{ TScrollDetectListBox }

procedure TScrollDetectListBox.wndProc(var aMessage: TMessage);
begin
  case assigned(FOnScroll) of FALSE: begin
                                      inherited WndProc(aMessage);
                                      EXIT; end;end;

  case (aMessage.msg = WM_VSCROLL) or (aMessage.msg = WM_KEYDOWN) of TRUE:  begin
                                                                              inherited WndProc(aMessage);
                                                                              FOnScroll(-1);
                                                                              EXIT; end;end;

  case (aMessage.msg = WM_MOUSEWHEEL) of TRUE:  begin
                                                  var vDelta          := smallint(aMessage.WParam shr 16);
                                                  var vLinesToScroll  := -vDelta div WHEEL_DELTA;

                                                  var vScrollLines: integer;
                                                  systemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @vScrollLines, 0);

                                                  vLinesToScroll := vLinesToScroll * vScrollLines;

                                                  FOnScroll(vLinesToScroll);
                                                  inherited wndProc(aMessage);
                                                  EXIT; end;end;

  inherited WndProc(aMessage);
end;

initialization
  PL; // to create the appEvents.subscriber

finalization
//  gPlaylistFormProxy := NIL;

end.
