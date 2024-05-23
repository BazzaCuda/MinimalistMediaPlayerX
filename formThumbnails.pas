{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
unit formThumbnails;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.Controls, vcl.Dialogs, vcl.extCtrls, vcl.Forms, Vcl.ComCtrls,
  MPVBasePlayer, Vcl.Imaging.jpeg,
  thumbnailsKeyboard;

type
  TThumbnailsForm = class(TForm)
    Panel1: TPanel;
    StatusBar: TStatusBar;
    Panel2: TPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
  private
    mpv: TMPVBasePlayer;
    FKeyHandled: boolean;
    FMainForm: TForm;
    procedure onInitMPV(sender: TObject);
    function processKeyOp(aKeyOp: TKeyOp): boolean;
  protected
    procedure createParams(var params: TCreateParams); override;
  public
    function initThumbnails(const aForm: TForm): boolean;
    property mainForm: TForm read FMainForm write FMainForm;
  end;

function showThumbnails(const aForm: TForm): boolean;

implementation

uses
  mmpMPVCtrls, mmpMPVProperties,
  TCommonUtilsClass, TGlobalVarsClass, TThumbnailsClass, _debugWindow;

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

procedure TThumbnailsForm.createParams(var params: TCreateParams);
begin
  inherited;
  params.exStyle := params.exStyle OR WS_EX_APPWINDOW; // put an icon on the taskbar for the user
end;

procedure TThumbnailsForm.FormActivate(Sender: TObject);
begin
  CU.delay(1000);
  FMainForm.hide; // delay this until ThumbnailsForm has displayed over the main form
end;

procedure TThumbnailsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case mpv = NIL of FALSE: freeAndNIL(mpv); end;
end;

procedure TThumbnailsForm.FormCreate(Sender: TObject);
begin
    mpvCreate(mpv);
    mpv.onInitMPV    := onInitMPV;
    mpvInitPlayer(mpv, panel1.handle, '', extractFilePath(paramStr(0)));  // THIS RECREATES THE INTERNAL MPV OBJECT in TMPVBasePlayer
    mpvOpenFile(mpv, 'B:\Images\_cropes_\Cropes001-136\022.jpg');
end;

procedure TThumbnailsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  var vKeyOp: TKeyOp := processKeyStroke(mpv, key, shift, kdDown);
  FKeyHandled := processKeyOp(vKeyOp);
end;

procedure TThumbnailsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FKeyHandled of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  processKeyOp(processKeyStroke(mpv, key, shift, kdUp));
end;

function TThumbnailsForm.initThumbnails(const aForm: TForm): boolean;
begin
//  SELF.keyPreview := TRUE;

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

  panel2.visible := FALSE;

  panel2.styleElements  := [];
  panel2.bevelOuter     := bvNone;
  panel2.color          := clBlack;

//  setWindowPos(SELF.handle, 0, 0, 0, 0, 0, SWP_NOSIZE OR SWP_NOMOVE OR SWP_SHOWWINDOW);
end;

procedure TThumbnailsForm.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(mpv, extractFilePath(paramStr(0)));
end;


function TThumbnailsForm.processKeyOp(aKeyOp: TKeyOp): boolean;
begin
  result := FALSE;

  case aKeyOp of
    koNone:         EXIT;   // key not processed. bypass setting result to TRUE
    koCloseApp:     close;  // closes this form only

    koBrightnessUp: mpvBrightnessUp(mpv);
    koBrightnessDn: mpvBrightnessDn(mpv);
    koRotateR:      mpvRotateRight(mpv);
    koRotateL:      mpvRotateLeft(mpv);

  end;

  result := TRUE; // key was processed
end;

end.
