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
  MarkDownViewerComponents,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazAction,
  mmpConsts, HTMLUn2, HtmlView;

type
  IHelpFullForm = interface
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm:  TVoid;
  end;

  THelpFullForm = class(TForm, IHelpFullForm)
    pageControl: TPageControl;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  strict private
    FHelpType: THelpType;
  protected
    function createTabs(const aHelpType: THelpType): TVoid;
  public
    function getHandle: HWND;
    function init(const aHelpType: THelpType): TVoid;
    function showForm: TVoid;
  end;

function mmpHelpFull(const aHelpType: THelpType): TVoid;

implementation

uses
  bazCmd, bazVCL,
  mmpGlobalState, mmpMarkDownUtils,
  _debugWindow;

{$R *.dfm}

type
  TMarkdownRec = record
    helpType: THelpType;
    caption:  string;
    resource: string;
  end;

const
    MARKDOWN_RESOURCES: array[0..6] of TMarkDownRec =
    (
      (helpType: htMain;    caption: 'what';       resource: 'resource_mdHelp1'),
      (helpType: htMain;    caption: 'not';        resource: 'resource_mdHelp2'),
      (helpType: htMain;    caption: 'biggles';    resource: 'resource_mdHelp3'),
      (helpType: htImages;  caption: 'flies';      resource: 'resource_mdImages1'),
      (helpType: htImages;  caption: 'undone';     resource: 'resource_mdImages2'),
      (helpType: htImages;  caption: 'on';         resource: 'resource_mdImages3'),
      (helpType: htBoth;    caption: 'Editing';    resource: 'Resource_mdEditing')
    );

var gHelpFullForm: IHelpFullForm = NIL;

function mmpHelpFull(const aHelpType: THelpType): TVoid;
begin
  case gHelpFullForm = NIL of TRUE: gHelpFullForm := THelpFullForm.create(app); end;
  gHelpFullForm.init(aHelpType);
  setForegroundWindow(gHelpFullForm.getHandle); // the order of these two is important
  setWindowPos(gHelpFullForm.getHandle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
  gHelpFullForm.showForm;
end;

function THelpFullForm.createTabs(const aHelpType: THelpType): TVoid;
  function initLabel(const aLabel: TLabel): TVoid;
  begin
    aLabel.align          := alBottom;
    aLabel.alignment      := taCenter;
    aLabel.caption        := 'hit [Escape] to close this window';
    aLabel.font.color     := clTeal;
    aLabel.font.name      := 'Segoe UI';
    aLabel.font.style     := [fsItalic];
    aLabel.parentFont     := FALSE;
    alabel.styleElements  := [seClient, seBorder];
  end;

  function initMarkDownViewer(const aMarkDownViewer: TMarkdownViewer): TVoid;
  begin
    aMarkdownViewer.align   := alClient;
    aMarkdownViewer.margins.setBounds(0, 0, 0, 0);
  end;

begin
  while pageControl.pageCount > 0 do pageControl.pages[0].free;

  for var vIx := low(MARKDOWN_RESOURCES) to high(MARKDOWN_RESOURCES) do
  begin
    case MARKDOWN_RESOURCES[vIx].HelpType in [aHelpType, htBoth] of FALSE: CONTINUE; end;
    var vTabSheet := TTabSheet.Create(pageControl);
    vTabSheet.pageControl       := pageControl;
    vTabSheet.caption           := MARKDOWN_RESOURCES[vIx].caption;

    var vLabel                  := TLabel.Create(vTabSheet);
    vLabel.parent               := vTabSheet;
    initLabel(vLabel);

    var vMarkDownViewer         := TMarkDownViewer.Create(vTabSheet);
    vMarkDownViewer.parent      := vTabSheet;
    initMarkDownViewer(vMarkDownViewer);

    mmpInitMarkdownViewer(vMarkDownViewer);
    vMarkDownViewer.defFontSize := 9;

    mmpLoadMarkDownFromResource(vMarkdownViewer, MARKDOWN_RESOURCES[vIx].resource);
  end;
end;

procedure THelpFullForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  gHelpFullForm := NIL;
end;

procedure THelpFullForm.FormCreate(Sender: TObject);
begin
  pageControl.activePageIndex := 0;
end;

procedure THelpFullForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: modalResult := mrOK;  end;
  case key = VK_ESCAPE of TRUE: close;                end;
end;

function THelpFullForm.getHandle: HWND;
begin
  result := SELF.HANDLE;
end;

function THelpFullForm.init(const aHelpType: THelpType): TVoid;
begin
  case aHelpType = FHelpType of FALSE: createTabs(aHelpType); end;
  FHelpType := aHelpType;
end;

function THelpFullForm.showForm: TVoid;
begin
  show;
end;

end.
