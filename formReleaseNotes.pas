unit formReleaseNotes;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.stdCtrls,
  HTMLUn2, HtmlView, MarkDownViewerComponents,
  strUtils;

type
  TReleaseNotesForm = class(TForm)
    md: TMarkdownViewer;
    procedure FormShow(Sender: TObject);
  private
  public
  end;

var
  ReleaseNotesForm: TReleaseNotesForm;

implementation

{$R *.dfm}

procedure TReleaseNotesForm.FormShow(Sender: TObject);
begin
  md.DefBackground := $2B2B2B;
  md.defFontColor  := $C0C0C0;
  md.borderStyle   := htNone;
  md.defFontName   := 'Tahoma';
  md.defFontSize   := 11;
  SELF.color       := md.defBackground;
//  SELF.styleElements := [];
  md.scrollBars    := ssVertical;
  md.htOptions     := [htOverLinksActive];
  md.serverRoot    := 'B:\Win64_Dev\workshops\markdownViewer\Win64\Release\';
  md.loadFromFile('B:\Win64_Dev\workshops\markdownViewer\new 11.md');
  md.LoadFromString(replaceStr(md.MarkdownContent.Text, '\r', #13), FALSE);
  md.LoadFromString(replaceStr(md.MarkdownContent.Text, '\n', #10), FALSE);
  md.loadFromFile('B:\Win64_Dev\workshops\markdownViewer\README.md');
  md.loadFromFile('B:\Win64_Dev\workshops\markdownViewer\releaseNotes.md');
  md.serverRoot := extractFilePath('B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\releaseNotes\releaseNotes v2_2_1.md');
  md.loadFromFile('B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\releaseNotes\releaseNotes v2_2_1.md');
//  md.loadFromFile('B:\Win64_Dev\3P\MarkdownHelpViewer\Demo\Help\BrowseTab.md');
end;

end.
