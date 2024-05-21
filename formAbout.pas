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
unit formAbout;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblReleaseVersion: TLabel;
    lblCopyright: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblWebsiteURL: TLabel;
    Bevel1: TBevel;
    btnOK: TButton;
    Bevel2: TBevel;
    Label6: TLabel;
    lblBuildVersion: TLabel;
    lblLatestReleaseVersion: TLabel;
    Label7: TLabel;
    btnWhatsNew: TButton;
    procedure lblWebsiteURLClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure lblWebsiteURLMouseEnter(Sender: TObject);
    procedure lblWebsiteURLMouseLeave(Sender: TObject);
    procedure btnWhatsNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
    function compareVersions(const thisVersion: string; const latestVersion: string): boolean;
    function setBuildVersion(const aBuild: string): boolean;
    function setCopyrightYear(const aYear: WORD): boolean;
    function setLatestReleaseVersion(const aRelease: string): boolean;
    function setReleaseVersion(const aRelease: string): boolean;
    function setWhatsNew(const aHasReleaseNotes: boolean): boolean;
  end;

function showAboutBox(const thisVersion: string; const buildVersion: string): boolean;

implementation

uses
  shellAPI,
  formReleaseNotes,
  TGlobalVarsClass, TCommonUtilsClass, TProgramUpdatesClass, _debugWindow;

{$R *.dfm}

function showAboutBox(const thisVersion: string; const buildVersion: string): boolean;
begin
  GV.userInput := TRUE;
  with TAboutForm.create(NIL) do
  try
    setCopyrightYear(currentYear);
    setReleaseVersion(thisVersion);
    setBuildVersion(buildVersion);
    setLatestReleaseVersion(PU.releaseTag);        // if the releaseTag is got, PU also downloads the release zip file and the release notes
    compareVersions(thisVersion, PU.releaseTag);
    setWhatsNew(PU.hasReleaseNotes);
    showModal;
  finally
    free;
    CU.delay(1000);
    GV.userInput := FALSE;
  end;
end;

procedure TAboutForm.btnOKClick(Sender: TObject);
begin
  modalResult := mrOK;
end;

procedure TAboutForm.btnWhatsNewClick(Sender: TObject);
begin
  with TReleaseNotesForm.create(NIL) do begin
    showModal;
    free;
  end;
end;

function TAboutForm.compareVersions(const thisVersion: string; const latestVersion: string): boolean;
begin
  case latestVersion[1] = 'v' of FALSE: EXIT; end;
  case thisVersion = latestVersion of FALSE: lblLatestReleaseVersion.font.style := [fsBold, fsUnderline]; end;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  case btnWhatsNew.visible of TRUE: btnWhatsNew.setFocus; end;
end;

procedure TAboutForm.lblWebsiteURLClick(Sender: TObject);
begin
  shellExecute(0, 'open', 'https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/latest', '', '', SW_SHOW);
end;

procedure TAboutForm.lblWebsiteURLMouseEnter(Sender: TObject);
begin
  lblWebsiteURL.font.style := [fsUnderline];
end;

procedure TAboutForm.lblWebsiteURLMouseLeave(Sender: TObject);
begin
  lblWebsiteURL.font.style := [];
end;

function TAboutForm.setBuildVersion(const aBuild: string): boolean;
begin
  lblBuildVersion.Caption := aBuild;
end;

function TAboutForm.setCopyrightYear(const aYear: WORD): boolean;
begin
  lblCopyright.caption := lblCopyright.caption + intToStr(aYear);
end;

function TAboutForm.setLatestReleaseVersion(const aRelease: string): boolean;
begin
  lblLatestReleaseVersion.caption := aRelease;
end;

function TAboutForm.setReleaseVersion(const aRelease: string): boolean;
begin
  lblReleaseVersion.Caption := aRelease;
end;

function TAboutForm.setWhatsNew(const aHasReleaseNotes: boolean): boolean;
begin
  btnWhatsNew.visible := aHasReleaseNotes;
  btnWhatsNew.default := aHasReleaseNotes;
end;

end.
