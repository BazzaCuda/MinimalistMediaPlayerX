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
unit view.mmpFormHelp;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.stdCtrls, vcl.extCtrls, vcl.forms,
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  IHelpForm = interface
    ['{1FF2B312-F655-4254-800D-C489EE2A712B}']
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  THelpForm = class(TForm)
    backPanel:    TPanel;
    buttonPanel:  TPanel;
    shiftLabel:   TLabel;
    helpLabel:    TLabel;
    md1:          TMarkdownViewer;
    md2:          TMarkdownViewer;
    md3:          TMarkdownViewer;
    procedure   FormResize(Sender: TObject);
  protected
    constructor create(const aHeight: integer; const aHelpType: THelpType);
    procedure   CreateParams(var Params: TCreateParams);
  public
  end;
  {$ENDREGION}

implementation

uses
  winApi.shellAPI, system.strUtils,
  mmpGlobalState, mmpFuncProcs, mmpMarkDownUtils,
  _debugWindow;

type
  // can't implement IHelpForm with the TForm so we use an intermediary
  THelpFormProxy = class(TInterfacedObject, IHelpForm)
  strict private
    FHelpForm:    THelpForm;
    FSubscriber:  ISubscriber;
  private
    function    onNotify(const aNotice: INotice): INotice;
  protected
    function    createForm(const wr: TWndRec): THelpForm;
    function    moveForm(const wr: TWndRec): boolean;
    function    showForm: boolean;
    function    shutForm: boolean;
  public
    constructor create;
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
  end;

var gHelpFormProxy: IHelpForm = NIL;
function HW: IHelpForm;
begin
  case gHelpFormProxy = NIL of TRUE: gHelpFormProxy := THelpFormProxy.create; end;
  result := gHelpFormProxy;
end;

{$R *.dfm}

{ THelpForm }

constructor THelpForm.create(const aHeight: integer; const aHelpType: THelpType);
begin
  inherited create(NIL);

  initMarkDownViewer(md1);
  initMarkDownViewer(md2);
  initMarkDownViewer(md3);

  md1.defFontSize := 10;
  md2.defFontSize := 10;
  md3.defFontSize := 10;

  md1.align       := alLeft;
  md3.align       := alRight;
  md2.align       := alClient;

  md1.noSelect    := TRUE;
  md2.noSelect    := TRUE;
  md3.noSelect    := TRUE;

  md1.cursor      := crDefault;
  md2.cursor      := crDefault;
  md3.cursor      := crDefault;

  SELF.width      :=  600;
  case aHeight > UI_DEFAULT_AUDIO_HEIGHT of  TRUE: SELF.height := aHeight;
                                            FALSE: SELF.height := 600; end;

  md1.width       := SELF.width div 3;
  md2.width       := SELF.width div 3;
  md3.width       := SELF.width div 3;

  md1.margins.top := 6;
  md2.margins.top := 6;
  md3.margins.top := 6;

  buttonPanel.margins.bottom := 4;

  case aHelpType of   htMain: begin
                                loadMarkDownFromResource(md1, 'resource_mdHelp1');
                                loadMarkDownFromResource(md2, 'resource_mdHelp2');
                                loadMarkDownFromResource(md3, 'resource_mdHelp3'); end;
                    htImages: begin
                                loadMarkDownFromResource(md1, 'resource_mdImages1');
                                loadMarkDownFromResource(md2, 'resource_mdImages2');
                                loadMarkDownFromResource(md3, 'resource_mdImages3'); end;
  end;

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;
end;

procedure THelpForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  params.exStyle    := params.ExStyle or (WS_EX_APPWINDOW);
  params.wndParent  := SELF.Handle; // normally application.handle
end;

procedure THelpForm.FormResize(Sender: TObject);
begin
  helpLabel.invalidate;
  buttonPanel.invalidate;
end;

{ THelpProxy }

constructor THelpFormProxy.create;
begin
  inherited;
  FSubscriber := appNotifier.subscribe(newSubscriber(onNotify));
end;

function THelpFormProxy.createForm(const wr: TWndRec): THelpForm;
begin
  result := FHelpForm;
  case wr.createNew of FALSE: EXIT; end;
  result := THelpForm.create(wr.height, wr.helpType);
end;

destructor THelpFormProxy.Destroy;
begin
  appNotifier.unsubscribe(FSubscriber);
  shutForm;
  inherited;
end;

function THelpFormProxy.moveForm(const wr: TWndRec): boolean;
begin
  FHelpForm := createForm(wr);
  case FHelpForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current playlist window. Used for repositioning the window when the main UI moves or resizes.
  GS.notify(newNotice(evGSShowingHelp, TRUE));
  case wr.height > UI_DEFAULT_AUDIO_HEIGHT of TRUE: FHelpForm.height := wr.height; end;
  screen.cursor := crDefault;

  mmpDo(evHelpShowHelp);

  winAPI.windows.setWindowPos(FHelpForm.handle, HWND_TOP, wr.pt.X, wr.pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  setForegroundWindow(wr.HWND);
  mmpDo(evGSWidthHelp, FHelpForm.width);
end;

function THelpFormProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function THelpFormProxy.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evHelpMoveHelp: moveForm(aNotice.wndRec);
    evHelpShutHelp: shutForm;
    evHelpShowHelp: showForm;
  end;
end;

function THelpFormProxy.showForm: boolean;
begin
  case FHelpForm = NIL of TRUE: EXIT; end;
  FHelpForm.show;
end;

function THelpFormProxy.shutForm: boolean;
begin
  case FHelpForm = NIL of TRUE: EXIT; end;
  FHelpForm.close;
  FHelpForm.Free;
  FHelpForm := NIL;
  mmpDo(evGSWidthHelp, 0);
  GS.notify(newNotice(evGSShowingHelp, FALSE));
end;

initialization
  HW; // to create the appNotifier subscriber

finalization
  gHelpFormProxy := NIL;

end.
