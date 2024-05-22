unit formReleaseNotes;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.stdCtrls,
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  strUtils, Vcl.ExtCtrls;

type
  TReleaseNotesForm = class(TForm)
    md: TMarkdownViewer;
    Panel1: TPanel;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mdHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
  private
    function initReleaseNotes: boolean;
    function loadReleaseNotes(const aReleaseTag: string): boolean;
  public
  end;

function showReleaseNotes(const aReleaseTag: string): boolean;

implementation

uses
  winApi.shellApi,
  mmpConsts, TProgramUpdatesClass, _debugWindow;

function showReleaseNotes(const aReleaseTag: string): boolean;
begin
  with TReleaseNotesForm.create(NIL) do begin
    loadReleaseNotes(aReleaseTag);
    showModal;
    free;
  end;
end;

{$R *.dfm}

procedure TReleaseNotesForm.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TReleaseNotesForm.FormCreate(Sender: TObject);
begin
  initReleaseNotes;
end;

procedure TReleaseNotesForm.FormShow(Sender: TObject);
begin
  initReleaseNotes;
end;

function TReleaseNotesForm.initReleaseNotes: boolean;
begin
  md.DefBackground    := DARK_MODE_DARK;
  md.defFontColor     := DARK_MODE_SILVER;
  md.defHotSpotColor  := DARK_MODE_SILVER;
  md.defOverLinkColor := DARK_MODE_SILVER;
  md.borderStyle      := htNone;
  md.defFontName      := 'Tahoma';
  md.defFontSize      := 11;
  md.scrollBars       := ssVertical;
  md.htOptions        := [htOverLinksActive];
  SELF.color          := md.defBackground;
  btnClose.default    := TRUE;
  btnClose.cancel     := TRUE;
end;

function TReleaseNotesForm.loadReleaseNotes(const aReleaseTag: string): boolean;
begin
  SELF.caption := 'Release Notes ' + aReleaseTag;
  md.serverRoot := PU.getReleaseNotesFolder;
  md.loadFromFile(PU.getReleaseNotesFilePath(aReleaseTag));
end;

procedure TReleaseNotesForm.mdHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
begin
  shellExecute(0, 'open', PWideChar(SRC), '', '', SW_SHOW);
  handled := TRUE;
end;

end.
