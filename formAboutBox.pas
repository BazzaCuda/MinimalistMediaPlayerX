{   MMP: Minimalist Media Player
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
unit formAboutBox;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls;

type
  TAboutBoxForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblReleaseVersion: TLabel;
    lblCopyright: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblWebsiteURL: TLabel;
    Bevel1: TBevel;
    btnClose: TButton;
    Bevel2: TBevel;
    Label6: TLabel;
    lblBuildVersion: TLabel;
    lblLatestReleaseVersion: TLabel;
    Label7: TLabel;
    btnWhatsNew: TButton;
    Label3: TLabel;
    lblWikiURL: TLabel;
    procedure lblWebsiteURLClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblWebsiteURLMouseEnter(Sender: TObject);
    procedure lblWebsiteURLMouseLeave(Sender: TObject);
    procedure btnWhatsNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblWikiURLClick(Sender: TObject);
    procedure lblWikiURLMouseEnter(Sender: TObject);
    procedure lblWikiURLMouseLeave(Sender: TObject);
  private
  public
    function compareVersions(const thisVersion: string; const latestVersion: string): boolean;
    function setBuildVersion(const aBuild: string): boolean;
    function setCopyrightYear(const aYear: WORD): boolean;
    function setLatestReleaseVersion(const aRelease: string): boolean;
    function setReleaseVersion(const aRelease: string): boolean;
    function setNoStyle: boolean;
    function setWhatsNew(const aHasReleaseNotes: boolean): boolean;
  end;

function showAboutBox(const thisVersion: string; const buildVersion: string): boolean; overload;
function showAboutBox: boolean; overload;

implementation

uses
  shellAPI,
  mmpFileUtils, mmpShellUtils, mmpSingletons, mmpUtils,
  formReleaseNotes,
  _debugWindow;

{$R *.dfm}

function showAboutBox(const thisVersion: string; const buildVersion: string): boolean; overload;
begin
  GV.userInput := TRUE;
  with TAboutBoxForm.create(NIL) do
  try
    setNoStyle;
    setCopyrightYear(currentYear);
    setReleaseVersion(thisVersion);
    setBuildVersion(buildVersion);
    setLatestReleaseVersion(PU.releaseTag);        // if the releaseTag is got, PU also downloads the release zip file and the release notes
    compareVersions(thisVersion, PU.releaseTag);
    setWhatsNew(PU.hasReleaseNotes(PU.releaseTag));
    showModal;
  finally
    free;
    mmpDelay(1000);
    GV.userInput := FALSE;
  end;
end;

function showAboutBox: boolean; overload;
begin
  showAboutBox(mmpFileVersionFmt('', 'v%d.%d.%d'), mmpFileVersionFmt);
end;

procedure TAboutBoxForm.btnCloseClick(Sender: TObject);
begin
  modalResult := mrOK;
end;

procedure TAboutBoxForm.btnWhatsNewClick(Sender: TObject);
begin
  showReleaseNotes(PU.releaseTag);
end;

function TAboutBoxForm.compareVersions(const thisVersion: string; const latestVersion: string): boolean;
begin
  case latestVersion[1] = 'v' of FALSE: EXIT; end;
  case thisVersion = latestVersion of FALSE: lblLatestReleaseVersion.font.style := [fsBold, fsUnderline]; end;
end;

procedure TAboutBoxForm.FormShow(Sender: TObject);
begin
  case btnWhatsNew.visible of TRUE: btnWhatsNew.setFocus; end;
  btnClose.cancel := TRUE;
end;

procedure TAboutBoxForm.lblWebsiteURLClick(Sender: TObject);
begin
  mmpShellExec('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/latest');
end;

procedure TAboutBoxForm.lblWebsiteURLMouseEnter(Sender: TObject);
begin
  lblWebsiteURL.font.style := [fsUnderline];
end;

procedure TAboutBoxForm.lblWebsiteURLMouseLeave(Sender: TObject);
begin
  lblWebsiteURL.font.style := [];
end;

procedure TAboutBoxForm.lblWikiURLClick(Sender: TObject);
begin
  mmpShellExec('https://minimalistmediaplayer.com');
end;

procedure TAboutBoxForm.lblWikiURLMouseEnter(Sender: TObject);
begin
  lblWikiURL.font.style := [fsUnderline];
end;

procedure TAboutBoxForm.lblWikiURLMouseLeave(Sender: TObject);
begin
  lblWikiURL.font.style := [];
end;

function TAboutBoxForm.setBuildVersion(const aBuild: string): boolean;
begin
  lblBuildVersion.Caption := aBuild;
end;

function TAboutBoxForm.setCopyrightYear(const aYear: WORD): boolean;
begin
  lblCopyright.caption := lblCopyright.caption + intToStr(aYear);
end;

function TAboutBoxForm.setLatestReleaseVersion(const aRelease: string): boolean;
begin
  lblLatestReleaseVersion.caption := aRelease;
end;

function TAboutBoxForm.setNoStyle: boolean;
begin
  lblWebsiteURL.styleElements := [seClient, seBorder];
  lblWikiURL.styleElements    := [seClient, seBorder];
end;

function TAboutBoxForm.setReleaseVersion(const aRelease: string): boolean;
begin
  lblReleaseVersion.Caption := aRelease;
end;

function TAboutBoxForm.setWhatsNew(const aHasReleaseNotes: boolean): boolean;
begin
  btnWhatsNew.visible := aHasReleaseNotes;
  btnWhatsNew.default := aHasReleaseNotes;
end;

end.
