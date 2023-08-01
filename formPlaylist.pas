{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit formPlaylist;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TPlaylistForm = class(TForm)
    backPanel: TPanel;
    buttonPanel: TPanel;
    shiftLabel: TLabel;
    moveLabel: TLabel;
    LB: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
  end;

function showPlaylist(const Pt: TPoint; const createNew: boolean = TRUE): boolean;
function showingPlaylist: boolean;
function shutPlaylist: boolean;

implementation

uses ShellAPI, UICtrls, system.strUtils, commonUtils;

var
  playlistForm: TPlaylistForm;

function showingPlaylist: boolean;
begin
  result := playlistForm <> NIL;
end;

function showPlaylist(const Pt: TPoint; const createNew: boolean = TRUE): boolean;
begin
  case (playlistForm = NIL) and createNew of TRUE: playlistForm := TPlaylistForm.create(NIL); end;
  case playlistForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current playlist window. Used for repositioning the window when the main UI moves or resizes.

  playlistForm.show;
  winAPI.Windows.setWindowPos(playlistForm.handle, HWND_TOP, Pt.X, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  enableWindow(playlistForm.handle, FALSE);    // this window won't get any keyboard or mouse messages, etc.
  setForegroundWindow(UI.handle); // so the UI keyboard functions can still be used when this form is open.
end;

function shutPlaylist: boolean;
begin
  case playlistForm <> NIL of TRUE: begin playlistForm.close; playlistForm.free; playlistForm := NIL; end;end;
  playlistForm := NIL;
end;

{$R *.dfm}

procedure TPlaylistForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := self.Handle; // normally application.handle
end;

procedure TPlaylistForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  playlistForm.free; playlistForm := NIL;
end;

procedure TPlaylistForm.FormCreate(Sender: TObject);
begin
  LB.align          := alClient;
  LB.bevelInner     := bvNone;
  LB.bevelOuter     := bvNone;
  LB.borderStyle    := bsNone;

  SELF.width  := 556;
//  SELF.height := 840;

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;
end;

initialization
  playlistForm := NIL;

finalization
  case playlistForm <> NIL of TRUE: begin playlistForm.close; playlistForm.free; playlistForm := NIL; end;end;

end.
