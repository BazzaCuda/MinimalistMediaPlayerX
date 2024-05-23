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
unit formReleaseNotes;

interface

uses
  winApi.windows, winApi.messages,
  system.classes, system.strUtils, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls,
  HTMLUn2, HtmlView, MarkDownViewerComponents;

type
  TReleaseNotesForm = class(TForm)
    md: TMarkdownViewer;
    Panel1: TPanel;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mdHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
  private
    function initReleaseNotes: boolean;
    function loadReleaseNotes(const aReleaseTag: string): boolean;
  public
  end;

function showReleaseNotes(const aReleaseTag: string): boolean;

implementation

uses
  winApi.shellApi,
  mmpConsts, mmpFileUtils, mmpMarkDownUtils,
  TProgramUpdatesClass,
  _debugWindow;

function showReleaseNotes(const aReleaseTag: string): boolean;
begin
  with TReleaseNotesForm.create(NIL) do begin
    loadReleaseNotes(aReleaseTag);
    showModal;
    free;
  end;
end;

{$R *.dfm}

procedure TReleaseNotesForm.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TReleaseNotesForm.FormCreate(Sender: TObject);
begin
  initReleaseNotes;
end;

procedure TReleaseNotesForm.FormShow(Sender: TObject);
begin
  initReleaseNotes;
end;

function TReleaseNotesForm.initReleaseNotes: boolean;
begin
  initMarkDownViewer(md);

  SELF.color          := md.defBackground;
  btnClose.default    := TRUE;
  btnClose.cancel     := TRUE;
end;

function TReleaseNotesForm.loadReleaseNotes(const aReleaseTag: string): boolean;
begin
  SELF.caption := 'Release Notes ' + aReleaseTag;
  md.serverRoot :=  mmpExePath;
  md.loadFromFile(PU.getReleaseNotesFilePath(aReleaseTag));
end;

procedure TReleaseNotesForm.mdHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
begin
  shellExecute(0, 'open', PWideChar(SRC), '', '', SW_SHOW);
  handled := TRUE;
end;

end.
