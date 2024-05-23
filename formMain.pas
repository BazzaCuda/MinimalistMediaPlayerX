{   Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit formMain;

interface

uses
  winApi.messages, winapi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics;

type
  TMMPUI = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure WMNCHitTest(var msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSizing(var msg: TMessage); message WM_SIZING;
    procedure WMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;
    procedure WMEnterSizeMove(var Message: TMessage); message WM_ENTERSIZEMOVE;
  public
  end;

var
  MMPUI: TMMPUI;

implementation

uses
  winApi.shellApi,
  mmpConsts, mmpDialogs, mmpFileUtils, mmpUtils,
  formCaptions, formMediaCaption,
  TConfigFileClass, TGlobalVarsClass, TMediaInfoClass, TMediaPlayerClass, TMediaTypesClass, TPlaylistClass, TParamStringsClass, TProgressBarClass, TSysCommandsClass, TUICtrlsClass,
  _debugWindow;

{$R *.dfm}

{ TMMPUI }

procedure TMMPUI.FormCreate(Sender: TObject);
begin
  GV.appWnd := APPLICATION.HANDLE;

{ initApp()
============}
  UI.initUI(SELF);
  CF.initConfigFile(mmpConfigFilePath);
  MP.initMediaPlayer;
  ST.initCaptions(UI.videoPanel); // multiple captions at the bottom of the window
  MC.initCaption(UI.videoPanel);  // single caption at the top of the window
  PB.initProgressBar(ST);
{============}

  case PS.noFile of TRUE:  begin
                                mmpShowOKCancelMsgDlg('Typically, you would use "Open with..." in your File Explorer / Manager, to open a media file'#13#10
                                                    + 'or to permanently associate media file types with this application.'#13#10#13#10
                                                    + 'Alternatively, you can drag and drop a media file onto the window background',
                                                      mtInformation, [MBOK]);

                                postMessage(GV.appWnd, WM_SHOW_WINDOW, 0, 0);
                                EXIT; end;end;


  PL.fillPlaylist(PS.fileFolder);

  case PL.find(PS.fileFolderAndName) of TRUE: postMessage(GV.appWnd, WM_PLAY_CURRENT_ITEM, 0, 0); end;

  UI.Initialized := TRUE; // UI.formResize is only allowed to do anything after the first media plays.

  mmpDelay(100);
  sendMessage(GV.appWnd, WM_AUTO_CENTRE_WINDOW, 0, 0);

  postMessage(GV.appWnd, WM_SHOW_WINDOW, 0, 0);

  case GV.closeApp of TRUE: sendSysCommandClose(UI.handle); end; // pending since user tried to exit during initialization
end;

procedure TMMPUI.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ST.opInfo := MP.volDown;
end;

procedure TMMPUI.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  ST.opInfo := MP.volUp;
end;

procedure TMMPUI.WMDropFiles(var msg: TWMDropFiles);
// Allow a media file to be dropped onto the window.
// The playlist will be entirely refreshed using the contents of this media file's folder.
var vFilePath: string;
begin
  inherited;
  var hDrop := msg.Drop;
  try
    var droppedFileCount := dragQueryFile(hDrop, $FFFFFFFF, nil, 0);
    for var i := 0 to pred(droppedFileCount) do begin
      var fileNameLength := dragQueryFile(hDrop, i, nil, 0);
      setLength(vFilePath, fileNameLength);
      dragQueryFile(hDrop, i, PChar(vFilePath), fileNameLength + 1);

      PL.fillPlaylist(extractFilePath(vFilePath));
      PL.find(vFilePath);
      case PL.hasItems of TRUE: postMessage(GV.appWnd, WM_PLAY_CURRENT_ITEM, 0, 0); end;
      UI.Initialized := TRUE; // UI.formResize is only allowed to do anything after the first media plays.

      BREAK;              // we currently only process the first file if multiple files are dropped
    end;
  finally
    dragFinish(hDrop);
  end;
  msg.result := 0;
end;

procedure TMMPUI.WMEnterSizeMove(var Message: TMessage);
// the user manually starts to resize the window
begin
  UI.autoCentre := FALSE;
end;

procedure TMMPUI.WMNCHitTest(var msg: TWMNCHitTest);
begin
  inherited;
  // Prevent the cursor from changing when hovering over the bottom edge (HTBOTTOM)
  if msg.result = HTBOTTOM then
    msg.result := HTCLIENT;
end;

procedure TMMPUI.WMSizing(var msg: TMessage);
// restricts the vertical resizing by modifying the bottom edge of the resizing rectangle to ensure that the window's height remains constant.
// The user can control the width of a video - the app controls the height.
var
  newRect: PRect;
begin
  inherited;
  // Prevent vertical resizing by adjusting the rectangle's top and bottom edges
  newRect := PRect(Msg.LParam);
  newRect^.bottom := newRect^.top + height;
end;

end.
