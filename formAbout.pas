{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblReleaseVersion: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblWebsiteURL: TLabel;
    Bevel1: TBevel;
    btnOK: TButton;
    Bevel2: TBevel;
    Label6: TLabel;
    lblBuildVersion: TLabel;
    procedure lblWebsiteURLClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
  public
    function setBuildVersion(const aBuild: string): boolean;
    function setReleaseVersion(const aRelease: string): boolean;
  end;

function showAboutBox(const releaseVersion: string; const buildVersion: string): boolean;

implementation

uses shellAPI, globalVars, commonUtils;

{$R *.dfm}

function showAboutBox(const releaseVersion: string; const buildVersion: string): boolean;
begin
  GV.userInput := TRUE;
  with TAboutForm.create(NIL) do
  try
    setReleaseVersion(releaseVersion);
    setBuildVersion(buildVersion);
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

procedure TAboutForm.lblWebsiteURLClick(Sender: TObject);
begin
  shellExecute(0, 'open', 'https://github.com/BazzaCuda/MinimalistMediaPlayerX', '', '', SW_SHOW);
end;

function TAboutForm.setBuildVersion(const aBuild: string): boolean;
begin
  lblBuildVersion.Caption := aBuild;
end;

function TAboutForm.setReleaseVersion(const aRelease: string): boolean;
begin
  lblReleaseVersion.Caption := aRelease;
end;

end.
