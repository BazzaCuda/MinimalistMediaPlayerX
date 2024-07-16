unit view.mmpFormDownload;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics, vcl.stdCtrls;

type
  TDownloadForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
  public
    property byteLabel: TLabel read label1;
  end;

implementation

{$R *.dfm}

end.
