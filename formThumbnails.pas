unit formThumbnails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, vcl.extCtrls;

type
  TThumbnailsForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
    function initThumbnails(const aForm: TForm): boolean;
  end;

function TF: TThumbnailsForm;
function resizeThumbnailsForm(const aWidth: integer; const aHeight: integer): boolean;
function toggleThumbnailsParent(const aForm: TForm): boolean;

implementation

uses
  formMain, _debugWindow;

var
  gTN: TThumbnailsForm;

function TF: TThumbnailsForm;
begin
  case gTN = NIL of TRUE: gTN := TThumbnailsForm.create(NIL); end;
  result := gTN;
end;

function resizeThumbnailsForm(const aWidth: integer; const aHeight: integer): boolean;
begin
  case gTN = NIL of TRUE: EXIT; end;
  gTN.width  := aWidth;
  gTN.height := aHeight;
end;

function toggleThumbnailsParent(const aForm: TForm): boolean;
// MPV doesn't close properly if MMP closes while MC is still the parent of this form!
begin
  case gTN = NIL of TRUE: EXIT; end;

  case gTN.parent = NIL of  TRUE: gTN.parent := aForm;
                           FALSE: gTN.parent := NIL; end;
end;

{$R *.dfm}

procedure TThumbnailsForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for this window
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
//  Params.WndParent  := SELF.Handle; // normally application.handle
end;

function TThumbnailsForm.initThumbnails(const aForm: TForm): boolean;
begin
  toggleThumbnailsParent(aForm);

  case SELF.parent = NIL of TRUE: begin
                                      aForm.windowState := TWindowState.wsNormal; // restore MC's size
                                      SELF.hide;
                                      EXIT;
                                    end;end;

  aForm.WindowState := TWindowState.wsMaximized; // MC to entirely cover the video window...
  SELF.parent      := aForm;
  SELF.borderIcons := [];
  SELF.borderStyle := bsNone;
  SELF.align       := alClient;                  // ...so that this can, too.
  SELF.show;
end;

initialization
  gTN := NIL;

finalization
  case gTN <> NIL of TRUE: gTN.free; end;

end.
