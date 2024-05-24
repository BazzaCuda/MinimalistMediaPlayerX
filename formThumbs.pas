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
unit formThumbs;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, vcl.graphics, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.Forms, Vcl.ComCtrls,
  MPVBasePlayer,
  mmpThumbsKeyboard,
  TMPVHostClass, TThumbsClass;

type
  TThumbsForm = class(TForm)
    StatusBar: TStatusBar;
    pnlThumbsHost: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
  strict private
    mpv: TMPVBasePlayer;
    FInitialFilePath: string;
    FKeyHandled: boolean;
    FMPVHost: TMPVHost;
    FThumbs: TThumbs;
  private
    procedure onInitMPV(sender: TObject);
    procedure onOpenFile(const aURL: string);
    function processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
    function showPlaylist: boolean;
    function takeScreenshot: string;
  protected
    procedure CreateParams(var params: TCreateParams); override;
  public
    function initThumbnails(const aFilePath: string; const aRect: TRect): boolean;
  end;

function showThumbs(const aFilePath: string; const aRect: TRect): boolean;

implementation

uses
  mmpMPVCtrls, mmpMPVProperties,
  mmpConsts,
  formPlaylist,
  TGlobalVarsClass, TSendAllClass,
  _debugWindow;

function showThumbs(const aFilePath: string; const aRect: TRect): boolean;
begin
  var vTF := TThumbsForm.create(NIL);
  try
    vTF.initThumbnails(aFilePath, aRect);
    GV.showingThumbs := TRUE;
    vTF.showModal;
  finally
    vTF.free;
    GV.showingThumbs := FALSE;
  end;
end;

{$R *.dfm}

procedure TThumbsForm.CreateParams(var params: TCreateParams);
begin
  inherited;
  params.exStyle := params.exStyle OR WS_EX_APPWINDOW; // put an icon on the taskbar for the user
end;

procedure TThumbsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  case FMPVHost = NIL of FALSE: freeAndNIL(FMPVHost); end;
  case FThumbs  = NIL of FALSE: freeAndNIL(FThumbs); end;
  case mpv      = NIL of FALSE: freeAndNIL(mpv); end;
end;

procedure TThumbsForm.FormCreate(Sender: TObject);
begin
  FMPVHost := TMPVHost.create(SELF);
  FMPVHost.parent := SELF;
  FMPVHost.OnOpenFile := onOpenFile;

  mpvCreate(mpv);
  mpv.onInitMPV    := onInitMPV;
  mpvInitPlayer(mpv, FMPVHost.handle, '', extractFilePath(paramStr(0)));  // THIS RECREATES THE INTERNAL MPV OBJECT in TMPVBasePlayer
end;

procedure TThumbsForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  var vKeyOp: TKeyOp := processKeyStroke(mpv, key, shift, kdDn);
  FKeyHandled := processKeyOp(vKeyOp, shift);
end;

procedure TThumbsForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case FKeyHandled of TRUE: EXIT; end; //  Keys that can be pressed singly or held down for repeat action: don't process the KeyUp as well as the KeyDown
  processKeyOp(processKeyStroke(mpv, key, shift, kdUp), shift);
end;

procedure TThumbsForm.FormResize(Sender: TObject);
begin
  case FThumbs = NIL of TRUE: EXIT; end;
  FThumbs.playThumbs;
end;

procedure TThumbsForm.FormShow(Sender: TObject);
begin
  FThumbs := TThumbs.create;
  FThumbs.initThumbs(FMPVHost, pnlThumbsHost);
  FThumbs.playThumbs(FInitialFilePath);
end;

function TThumbsForm.initThumbnails(const aFilePath: string; const aRect: TRect): boolean;
begin
  FInitialFilePath := aFilePath;

  SELF.top    := aRect.top;
  SELF.left   := aRect.left;
  SELF.width  := aRect.Width;
  SELF.height := aRect.Height;

  SELF.borderIcons   := [biSystemMenu, biMaximize];
  SELF.borderStyle   := bsSizeable;
  SELF.borderWidth   := 0;
  SELF.color         := clBlack;

  FMPVHost.align      := alClient;
  pnlThumbsHost.align := alClient;

  FMPVHost.visible      := FALSE;
  pnlThumbsHost.visible := TRUE;

  pnlThumbsHost.styleElements  := [];
  pnlThumbsHost.bevelOuter     := bvNone;
  pnlThumbsHost.color          := clBlack;
end;

procedure TThumbsForm.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(mpv, extractFilePath(paramStr(0)));
end;

procedure TThumbsForm.onOpenFile(const aURL: string);
begin
  FMPVHost.visible      := TRUE;
  pnlThumbsHost.visible := FALSE;
  mpvOpenFile(mpv, aURL);
end;


function TThumbsForm.processKeyOp(const aKeyOp: TKeyOp; const aShiftState: TShiftState): boolean;
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
    koPlayNext:         FThumbs.playThumbs;
    koPlayPrev:         FThumbs.playPrevThumbsPage;
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
    koTogglePlaylist:   showPlaylist;
    koCloseAll:         begin close; SA.postToAll(WIN_CLOSEAPP); end;
    koToggleRepeat:;
    koAboutBox:;
    koMaximize:;
    koSpeedUp:;
    koSpeedDn:;
    koSpeedReset:;
  end;

  result := TRUE; // key was processed
end;

function TThumbsForm.showPlaylist: boolean;
begin
//  EXIT; // EXPERIMENTAL
  var vPt := pnlThumbsHost.ClientToScreen(point(pnlThumbsHost.left + pnlThumbsHost.width, pnlThumbsHost.top - 2)); // screen position of the top right corner of the application window, roughly.
  formPlaylist.showPlaylist(FThumbs.FPlaylist, vPt, pnlThumbsHost.height, TRUE);
end;

function TThumbsForm.takeScreenshot: string;
begin
  var vScreenshotDirectory: string;
  mpvGetPropertyString(mpv, 'screenshot-directory', vScreenshotDirectory);

//  case vScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, PL.currentFolder);                  // otherwise screenshots of an image go to Windows/System32 !!
  case vScreenshotDirectory = '' of  TRUE: result := mpvTakeScreenshot(mpv, extractFilePath(mpvFileName(mpv)));   // otherwise screenshots of an image go to Windows/System32 !!
                                    FALSE: result := mpvTakeScreenshot(mpv, vScreenshotDirectory); end;
end;

end.
