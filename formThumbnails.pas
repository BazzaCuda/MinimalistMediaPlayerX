unit formThumbnails;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.Controls, vcl.Dialogs, vcl.extCtrls, vcl.Forms, Vcl.ComCtrls,
  MPVBasePlayer, Vcl.Imaging.jpeg;

type
  TThumbnailsForm = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    Panel2: TPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    mpv: TMPVBasePlayer;
    FMainForm: TForm;
    procedure onInitMPV(sender: TObject);
  protected
  public
    function initThumbnails(const aForm: TForm): boolean;
    property mainForm: TForm read FMainForm write FMainForm;
  end;

function showThumbnails(const aForm: TForm): boolean;

implementation

uses
  TGlobalVarsClass, TThumbnailsClass, _debugWindow;

function showThumbnails(const aForm: TForm): boolean;
begin
  var vTF := TThumbnailsForm.create(NIL);
  try
    vTF.initThumbnails(aForm);
    GV.showingThumbs := TRUE;
    vTF.mainForm := aForm;
    vTF.showModal;
  finally
    vTF.free;
    GV.showingThumbs := FALSE;
  end;
end;

{$R *.dfm}

procedure TThumbnailsForm.FormActivate(Sender: TObject);
begin
  FMainForm.hide; // delay this until the last possible moment
end;

procedure TThumbnailsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case mpv = NIL of FALSE: freeAndNIL(mpv); end;
end;

procedure TThumbnailsForm.FormCreate(Sender: TObject);
begin
    mpv := TMPVBasePlayer.Create;
//    mpv.OnStateChged := onStateChange;
    mpv.onInitMPV    := onInitMPV;
    mpv.initPlayer(intToStr(panel1.handle), '', extractFilePath(paramStr(0)), '');  // THIS RECREATES THE INTERNAL MPV OBJECT
//    mpv.OpenFile('B:\Images\_cropes_\Cropes001-136\022.jpg');
end;

procedure TThumbnailsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
//  key := 0;
end;

procedure TThumbnailsForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
//  key := #0;
end;

procedure TThumbnailsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case (key = ord('X')) OR (key = ord('x'))  of TRUE: begin close; EXIT; end;end;
  case key = VK_ESCAPE of TRUE: begin close; EXIT; end;end;
//  key := 0;
end;

function TThumbnailsForm.initThumbnails(const aForm: TForm): boolean;
begin
  SELF.top    := aForm.top;
  SELF.left   := aForm.left;
  SELF.width  := aForm.Width;
  SELF.height := aForm.Height;

  SELF.borderIcons   := [biSystemMenu, biMaximize];
  SELF.borderStyle   := bsSizeable;
  SELF.borderWidth   := 0;
  SELF.color         := clBlack;

  panel1.align          := alClient;
  panel2.align          := alClient;

  panel1.visible := FALSE;

  panel2.styleElements  := [];
  panel2.bevelOuter     := bvNone;
  panel2.color          := clBlack;

//  setWindowPos(SELF.handle, 0, 0, 0, 0, 0, SWP_NOSIZE OR SWP_NOMOVE OR SWP_SHOWWINDOW);
end;

procedure TThumbnailsForm.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  with sender as TMPVBasePlayer do begin
    SetPropertyString('osc', 'no'); // On Screen Control
    SetPropertyString('force-window', 'yes');
    SetPropertyString('config-dir', extractFilePath(paramStr(0))); // mpv.conf location
    SetPropertyString('config', 'yes');  // DISABLE USER ACCESS TO MPV.CONF? - NO!
    SetPropertyBool('keep-open', FALSE); // ensure libmpv MPV_EVENT_END_FILE_ event at the end of every media file
    SetPropertyBool('keep-open-pause', FALSE);

    setPropertyString('sub-font', 'Segoe UI');
    setPropertyString('sub-color', '#808080');
    setPropertyString('osd-color', '#808080');
    setPropertyString('osd-bold',  'yes');
    setPropertyString('osd-back-color', '#00000000');
    setPropertyString('osd-shadow-offset', '0');
    setPropertyString('screenshot-format', 'png');
    SetPropertyString('osd-font-size', '10');
    SetPropertyInt64('osd-duration', 3000);
    setPropertyString('osd-align-x', 'right');
    setPropertyString('osd-align-y', 'bottom');
    setPropertyString('osd-margin-x', '4');
    setPropertyString('osd-margin-y', '24');
    setPropertyString('screenshot-png-compression', '0');
    setPropertyString('screenshot-template', '%F %p %04n');
    setPropertyString('sid', '1');
    setPropertyString('image-display-duration', 'inf');
//    SetPropertyDouble('sub-delay', -00.99);
  end;
end;

end.
