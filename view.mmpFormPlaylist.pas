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
unit view.mmpFormPlaylist;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls, Vcl.AppEvnts;

type
  IPlaylistForm = interface
    ['{AA9DD8D3-6275-4C84-A7BA-FB2D2BA3C41F}']
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  TPlaylistForm = class(TForm)
    backPanel:    TPanel;
    buttonPanel:  TPanel;
    shiftLabel:   TLabel;
    moveLabel:    TLabel;
    LB:           TListBox;
    ApplicationEvents: TApplicationEvents;
    lblFolder:    TLabel;
    procedure ApplicationEventsMessage(var msg: tagMSG; var handled: Boolean);
    procedure FormCreate(sender: TObject);
    procedure LBDblClick(sender: TObject);
    procedure LBKeyDn(sender: TObject; var key: Word; Shift: TShiftState);
    procedure LBKeyPress(sender: TObject; var key: Char);
    procedure LBKeyUp(sender: TObject; var key: Word; shift: TShiftState);
    procedure LBMouseEnter(Sender: TObject);
    procedure LBMouseLeave(Sender: TObject);
  private
    FMouseOver: boolean;
    function    isItemVisible: boolean;
    function    playItemIndex(const aItemIndex: integer): boolean;
    function    visibleItemCount: integer;
  protected
    procedure   createParams(var params: TCreateParams);
  public
    function    highlightCurrentItem: boolean;
    function    loadPlaylistBox(const forceReload: boolean = FALSE): boolean;
  end;
  {$ENDREGION}

implementation

uses
  winApi.shellApi,
  system.strUtils,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpKeyboardUtils, mmpUtils,
  viewModel.mmpGlobalState, viewModel.mmpKeyboard, viewModel.mmpKeyboardOps,
  _debugWindow;

type
  // can't implement IPlaylistForm with the TForm so we use an intermediary
  TPlayListFormProxy = class(TInterfacedObject, IPlaylistForm)
  strict private
    FListBoxLoaded: boolean;
    FPlaylistForm:  TPlaylistForm;
  private
    function    createForm(const bCreateNew: boolean): TPlaylistForm;
    function    moveForm(const wr: TWndRec): boolean;
    function    onNotify(const aNotice: INotice): INotice;
    function    showForm: boolean;
    function    shutForm: boolean;
  public
    constructor create;
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
  end;

var
  gPlaylistFormProxy: IPlaylistForm;
function PL: IPlaylistForm;
begin
  case gPlaylistFormProxy = NIL of TRUE: gPlaylistFormProxy := TPlaylistFormProxy.create; end;
  result := gPlaylistFormProxy;
end;

{$R *.dfm}

procedure TPlaylistForm.ApplicationEventsMessage(var msg: tagMSG; var handled: boolean);
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
  Params.ExStyle    := Params.ExStyle OR (WS_EX_APPWINDOW);
  Params.WndParent  := SELF.Handle; // normally application.handle
end;

procedure TPlaylistForm.FormCreate(Sender: TObject);
begin
  LB.align            := alClient;
  LB.bevelInner       := bvNone;
  LB.bevelOuter       := bvNone;
  LB.borderStyle      := bsNone;
  LB.margins.bottom   := 10;
  LB.margins.left     := 10;
  LB.margins.right    := 10;
  LB.margins.top      := 10;
  LB.AlignWithMargins := TRUE;

  SELF.width  := 556;
//  SELF.height := 840;

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
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
end;

function TPlaylistForm.highlightCurrentItem: boolean;
begin
  var vCurrentIx := notifyApp(newNotice(evPLReqCurrentIx)).integer;
  case vCurrentIx = -1 of TRUE: EXIT; end;
//  LB.items.beginUpdate;
  try
    case LB.count > 0 of TRUE:  begin
                                  LB.itemIndex := vCurrentIx;
                                  LB.selected[LB.itemIndex] := TRUE;
                                  case isItemVisible of TRUE: EXIT; end;
                                  var vTopIndex := LB.itemIndex - (visibleItemCount div 2); // try to position it in the middle of the listbox
                                  case vTopIndex >= 0 of  TRUE: LB.topIndex := vTopIndex;
                                                         FALSE: LB.topIndex := 0; end;end;end;
  finally
//    LB.items.endUpdate;
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

function TPlaylistForm.loadPlaylistBox(const forceReload: boolean = FALSE): boolean;
begin
  lblFolder.caption := 'Folder: ';
  LB.items.beginUpdate; // prevent flicker when moving the window

  try
    notifyApp(newNotice(evPLFillListBox, LB));

    highlightCurrentItem;
  finally
    LB.items.endUpdate;
  end;
  case LB.items.count > 0 of TRUE: lblFolder.caption := format('Folder: %s', [notifyApp(newNotice(evPLReqCurrentFolder)).text]); end;
end;

function TPlaylistForm.playItemIndex(const aItemIndex: integer): boolean;
begin
  notifyApp(newNotice(evVMShutTimeline));
  var vThisItem := notifyApp(newNotice(evPLReqThisItem, aItemIndex)).text;
  notifyApp(newNotice(evPLFind, vThisItem)); // set as current
  notifyApp(newNotice(evVMMPPlayCurrent));
end;

function TPlaylistForm.visibleItemCount: integer;
begin
  result := LB.clientHeight div LB.itemHeight;
end;

{ TPlayListFormProxy }

constructor TPlayListFormProxy.create;
begin
  appNotifier.subscribe(newSubscriber(onNotify));
end;

function TPlayListFormProxy.createForm(const bCreateNew: boolean): TPlaylistForm;
begin
  result := FPlaylistForm;
  case bCreateNew of FALSE: EXIT; end;
  result := TPlaylistForm.create(NIL);
end;

destructor TPlayListFormProxy.Destroy;
begin
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
  screen.cursor := crDefault;

  case FListBoxLoaded of FALSE: notifyApp(newNotice(evPLFormLoadBox)); end; // do once when the playlist is first opened
  FListBoxLoaded := TRUE;

  notifyApp(newNotice(evPLFormShow));
  notifyApp(newNotice(evPLFormHighlight));

  winAPI.windows.setWindowPos(FPlaylistForm.handle, HWND_TOP, wr.pt.X, wr.pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  notifyApp(newNotice(evGSWidthHelp, FPlaylistForm.width));
end;

function TPlayListFormProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TPlayListFormProxy.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
//  TDebug.debugEnum<TNoticeEvent>('PLForm.onNotify', aNotice.event);
  case aNotice.event of
    evPLFormMove:       moveForm(aNotice.wndRec);
    evPLFormShutForm:   shutForm;
    evPLFormLoadBox:    case FPlaylistForm = NIL of FALSE: FPlaylistForm.loadPlaylistBox; end;
    evPLFormHighlight:  case FPlaylistForm = NIL of FALSE: FPlaylistForm.highlightCurrentItem; end;
    evPLFormShow:       showForm;
    evPLNewPlaylist:    case FPlaylistForm = NIL of FALSE: FPlaylistForm.loadPlaylistBox; end;
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
  notifyApp(newNotice(evGSWidthHelp, 0));
  GS.notify(newNotice(evGSShowingPlaylist, FALSE));
  FListBoxLoaded := FALSE;
end;

initialization
  gPlaylistFormProxy  := NIL;
  PL; // to create the appNotifier subscriber

finalization
  gPlaylistFormProxy  := NIL;

end.
