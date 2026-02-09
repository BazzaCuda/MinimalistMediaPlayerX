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
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.stdCtrls, vcl.extCtrls, vcl.forms,
  {$endif}
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpAction, mmpConsts;

type
  IHelpForm = interface
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  THelpForm = class(TForm)
    backPanel:    TPanel;
    buttonPanel:  TPanel;
    shiftLabel:   TLabel;
    helpLabel:    TLabel;
    procedure   FormResize(Sender: TObject);
  protected
    constructor Create(const aHeight: integer; const aHelpType: THelpType); reintroduce;
    procedure   CreateParams(var Params: TCreateParams); reintroduce;
  public
  end;
  {$ENDREGION}

implementation

uses
  winApi.shellAPI, system.strUtils,
  bazCmd,
  mmpGlobalState, mmpMarkDownUtils,
  view.mmpFormStreamList,
  _debugWindow;

type
  // There were problems in D11 if we put the interface on a TForm, so an intermediary was used
  THelpFormProxy = class(TInterfacedObject, IHelpForm)
  strict private
    FHelpForm:    THelpForm;
    FSubscriber:  ISubscriber;
  private
    function    onNotify(const aNotice: INotice): INotice;
  protected
    function    createForm(const wr: TWndRec): THelpForm;
    function    moveForm(const wr: TWndRec): TVoid;
    function    showForm: TVoid;
    function    shutForm: TVoid;
  public
    constructor Create;
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
  end;

function HW: IHelpForm;
{$J+} const gHelpFormProxy: IHelpForm = NIL; {$J-}
begin
  case gHelpFormProxy = NIL of TRUE: gHelpFormProxy := THelpFormProxy.create; end;
  result := gHelpFormProxy;
end;

{$R *.dfm}

{ THelpForm }

constructor THelpForm.Create(const aHeight: integer; const aHelpType: THelpType);
begin
  inherited Create(NIL);

  var md1 := TMarkdownViewer.create(backPanel);
  var md2 := TMarkdownViewer.create(backPanel);
  var md3 := TMarkdownViewer.create(backPanel);

  md1.parent := backPanel;
  md2.parent := backPanel;
  md3.parent := backPanel;

  mmpInitMarkDownViewer(md1);
  mmpInitMarkDownViewer(md2);
  mmpInitMarkDownViewer(md3);

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

  case aHelpType of htMain: begin
                              mmpLoadMarkDownFromResource(md1, 'resource_mdHelp1');
                              mmpLoadMarkDownFromResource(md2, 'resource_mdHelp2');
                              mmpLoadMarkDownFromResource(md3, 'resource_mdHelp3'); end;
                    htIATB: begin
                              mmpLoadMarkDownFromResource(md1, 'resource_mdImages1');
                              mmpLoadMarkDownFromResource(md2, 'resource_mdImages2');
                              mmpLoadMarkDownFromResource(md3, 'resource_mdImages3'); end;
  end;

  setWindowLong(handle, GWL_STYLE, getWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;
end;

procedure THelpForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  params.exStyle    := params.ExStyle or (WS_EX_APPWINDOW);
  case GS.showingTimeline of   TRUE: params.wndParent := mmpStreamListHandle;
                              FALSE: params.wndParent := GS.mainForm.HANDLE; end; //  application.HANDLE;
end;

procedure THelpForm.FormResize(Sender: TObject);
begin
  helpLabel.invalidate;
  buttonPanel.invalidate;
end;

{ THelpProxy }

constructor THelpFormProxy.Create;
begin
  inherited;
  FSubscriber := appEvents.subscribe(newSubscriber(onNotify));
end;

function THelpFormProxy.createForm(const wr: TWndRec): THelpForm;
begin
  appEvents.unsubscribe(FSubscriber);
  result := FHelpForm;
  case wr.createNew of FALSE: EXIT; end;
  result := THelpForm.create(wr.height, wr.helpType);
end;

destructor THelpFormProxy.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
  shutForm;
  inherited;
end;

function THelpFormProxy.moveForm(const wr: TWndRec): TVoid;
begin
  FHelpForm := createForm(wr);
  case FHelpForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current playlist window. Used for repositioning the window when the main UI moves or resizes.
  GS.notify(newNotice(evGSShowingHelp, TRUE));
  case wr.height > UI_DEFAULT_AUDIO_HEIGHT of TRUE: FHelpForm.height := wr.height; end;
  screen.cursor := crDefault;

  mmp.cmd(evHelpShowHelp);

  winAPI.windows.setWindowPos(FHelpForm.handle, HWND_TOP, wr.pt.X, wr.pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  setForegroundWindow(wr.HWND);
  mmp.cmd(evGSWidthHelp, FHelpForm.width);
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

function THelpFormProxy.showForm: TVoid;
begin
  case FHelpForm = NIL of TRUE: EXIT; end;
  FHelpForm.show;
end;

function THelpFormProxy.shutForm: TVoid;
begin
  case FHelpForm = NIL of TRUE: EXIT; end;
  FHelpForm.close;
  FHelpForm.Free;
  FHelpForm := NIL;
  mmp.cmd(evGSWidthHelp, 0);
  GS.notify(newNotice(evGSShowingHelp, FALSE));
end;

initialization
  HW; // to create the appEvents.subscriber

end.
