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
    procedure FormShow(Sender: TObject);
  strict private
    mpv: TMPVBasePlayer;
    FInitialFolder: string;
    FKeyHandled: boolean;
  private
    procedure onInitMPV(sender: TObject);
    function processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
    function takeScreenshot: string;
  protected
    procedure createParams(var params: TCreateParams); override;
  public
    function initThumbnails(const aFolder: string; const aRect: TRect): boolean;
  end;

function showThumbnails(const aFolder: string; const aRect: TRect): boolean;

implementation

uses
  mmpMPVCtrls, mmpMPVProperties,
  TGlobalVarsClass, TThumbnailsClass,
  _debugWindow;

function showThumbnails(const aFolder: string; const aRect: TRect): boolean;
begin
  var vTF := TThumbnailsForm.create(NIL);
  try
    vTF.initThumbnails(aFolder, aRect);
    GV.showingThumbs := TRUE;
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

procedure TThumbnailsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case mpv = NIL of FALSE: freeAndNIL(mpv); end;
end;

procedure TThumbnailsForm.FormCreate(Sender: TObject);
begin
  mpvCreate(mpv);
  mpv.onInitMPV    := onInitMPV;
  mpvInitPlayer(mpv, panel1.handle, '', extractFilePath(paramStr(0)));  // THIS RECREATES THE INTERNAL MPV OBJECT in TMPVBasePlayer
  mpvOpenFile(mpv, 'B:\Images\Asterix the Gaul\16 Asterix in Switzerland\Asterix -07- Asterix in Switzerland - 12.jpg');
end;

procedure TThumbnailsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  var vKeyOp: TKeyOp := processKeyStroke(mpv, key, shift, kdDn);
  FKeyHandled := processKeyOp(vKeyOp, shift);
end;

procedure TThumbnailsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FKeyHandled of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  processKeyOp(processKeyStroke(mpv, key, shift, kdUp), shift);
end;

procedure TThumbnailsForm.FormShow(Sender: TObject);
begin
// start here - create first page of thumbnails from FInitialFolder
end;

function TThumbnailsForm.initThumbnails(const aFolder: string; const aRect: TRect): boolean;
begin
  FInitialFolder := aFolder;

  SELF.top    := aRect.top;
  SELF.left   := aRect.left;
  SELF.width  := aRect.Width;
  SELF.height := aRect.Height;

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
end;

procedure TThumbnailsForm.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(mpv, extractFilePath(paramStr(0)));
end;


function TThumbnailsForm.processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
begin
  result := FALSE;

  case aKeyOp of
    koNone:         EXIT;   // key not processed. bypass setting result to TRUE
    koCloseApp:     close;  // closes this form only

    koBrightnessUp:     mpvBrightnessUp(mpv);
    koBrightnessDn:     mpvBrightnessDn(mpv);
    koBrightnessReset:  mpvBrightnessReset(mpv);
    koContrastUp:       mpvContrastUp(mpv);
    koContrastDn:       mpvContrastDn(mpv);
    koContrastReset:    mpvContrastReset(mpv);
    koGammaUp:          mpvGammaUp(mpv);
    koGammaDn:          mpvGammaDn(mpv);
    koGammaReset:       mpvGammaReset(mpv);
    koPanLeft:          mpvPanLeft(mpv, aShiftState);
    koPanRight:         mpvPanRight(mpv, aShiftState);
    koPanUp:            mpvPanUp(mpv, aShiftState);
    koPanDn:            mpvPanDn(mpv, aShiftState);
    koPanReset:         mpvPanReset(mpv);
    koRotateR:          mpvRotateRight(mpv);
    koRotateL:          mpvRotateLeft(mpv);
    koRotateReset:      mpvRotateReset(mpv);
    koSaturationUp:     mpvSaturationUp(mpv);
    koSaturationDn:     mpvSaturationDn(mpv);
    koSaturationReset:  mpvSaturationReset(mpv);
    koScreenshot:       takeScreenshot;
    koZoomIn:           mpvZoomIn(mpv);
    koZoomOut:          mpvZoomOut(mpv);
    koZoomReset:        mpvZoomReset(mpv);

    koAllReset:         begin mpvBrightnessReset(mpv); mpvContrastReset(mpv); mpvGammaReset(mpv); mpvPanReset(mpv); mpvRotateReset(mpv); mpvSaturationReset(mpv); mpvZoomReset(mpv); end;

    koPausePlay:;
    koStartOver:;
    koShowCaption:;
    koPlayFirst:;
    koPlayNext:;
    koPlayPrev:;
    koPlayLast:;
    koFullscreen:;
    koGreaterWindow:;
    koToggleControls:;
    koToggleBlackout:;
    koCentreWindow:;
    koMinimizeWindow:;
    koDeleteCurrentItem:;
    koRenameFile:;
    koEscape:;
    koClipboard:;
    koKeep:;
    koReloadPlaylist:;
    koToggleHelp:;
    koBrighterPB:;
    koDarkerPB:;
    koTogglePlaylist:;
    koCloseAll:;
    koToggleRepeat:;
    koAboutBox:;
    koMaximize:;
    koSpeedUp:;
    koSpeedDn:;
    koSpeedReset:;
  end;

  result := TRUE; // key was processed
end;

function TThumbnailsForm.takeScreenshot: string;
begin
  var vScreenshotDirectory: string;
  mpvGetPropertyString(mpv, 'screenshot-directory', vScreenshotDirectory);

//  case vScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, PL.currentFolder);                  // otherwise screenshots of an image go to Windows/System32 !!
  case vScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, extractFilePath(mpvFileName(mpv)));   // otherwise screenshots of an image go to Windows/System32 !!
                                    FALSE: result := mpvTakeScreenshot(mpv, vScreenshotDirectory); end;
end;

end.
