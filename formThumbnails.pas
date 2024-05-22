unit formThumbnails;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.Controls, vcl.Dialogs, vcl.extCtrls, vcl.Forms;

type
  TThumbnailsForm = class(TForm)
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
    function initThumbnails(const aForm: TForm): boolean;
  end;

function TF: TThumbnailsForm;
function toggleThumbnailsParent(const aForm: TForm): boolean;

implementation

uses
  TThumbnailsClass, _debugWindow;

var
  gTF: TThumbnailsForm;

function TF: TThumbnailsForm;
begin
  case gTF = NIL of TRUE: gTF := TThumbnailsForm.create(NIL); end;
  result := gTF;
end;

function toggleThumbnailsParent(const aForm: TForm): boolean;
// MPV doesn't close properly if MMP closes while MC is still the parent of this form!
begin
  case gTF = NIL of TRUE: EXIT; end;

  case gTF.parent = NIL of  TRUE: gTF.parent := aForm;
                           FALSE: gTF.parent := NIL; end;
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
  gTF := NIL;

finalization
  case gTF <> NIL of TRUE: gTF.free; end;

end.