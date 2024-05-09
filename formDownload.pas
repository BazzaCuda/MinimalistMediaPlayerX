unit formDownload;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TDownloadForm = class(TForm)
    Label1: TLabel;
  private
    { Private declarations }
  public
    property byteLabel: TLabel read label1;
  end;


implementation

{$R *.dfm}

end.
