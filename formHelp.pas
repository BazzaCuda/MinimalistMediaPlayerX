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
unit formHelp;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.stdCtrls, vcl.extCtrls, vcl.forms,
  HTMLUn2, HtmlView, MarkDownViewerComponents;

type
  THelpType = (htHelp, htImages);

  THelpForm = class(TForm)
    backPanel: TPanel;
    buttonPanel: TPanel;
    shiftLabel: TLabel;
    helpLabel: TLabel;
    Panel1: TPanel;
    md1: TMarkdownViewer;
    md2: TMarkdownViewer;
    md3: TMarkdownViewer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
  protected
    constructor create(const aHelpType: THelpType);
    procedure CreateParams(var Params: TCreateParams);
  public
  end;

function showHelp(const aHWND: HWND; const Pt: TPoint; const aHelpType: THelpType; const createNew: boolean = TRUE): boolean;
function shutHelp: boolean;

implementation

uses
  winApi.shellAPI, system.strUtils,
  mmpConsts, mmpMarkDownUtils,
  TGlobalVarsClass;

var
  helpForm: THelpForm;

function showHelp(const aHWND: HWND; const Pt: TPoint; const aHelpType: THelpType; const createNew: boolean = TRUE): boolean;
begin
  case (helpForm = NIL) and createNew of TRUE: helpForm := THelpForm.create(aHelpType); end;
  case helpForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current help window. Used for repositioning the window when the main UI moves or resizes.

  helpForm.show;
  winAPI.Windows.setWindowPos(helpForm.handle, HWND_TOP, Pt.X - 1, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  enableWindow(helpForm.handle, FALSE);    // this window won't get any keyboard or mouse messages, etc.
  setForegroundWindow(aHWND); // so the UI keyboard functions can still be used when this form is open.
  GV.showingHelp := TRUE;
end;

function shutHelp: boolean;
begin
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;
  helpForm := NIL;
  GV.showingHelp := FALSE;
end;

{$R *.dfm}

{ THelpForm }

constructor THelpForm.create(const aHelpType: THelpType);
begin
  inherited create(NIL);

  initMarkDownViewer(md1);
  initMarkDownViewer(md2);
  initMarkDownViewer(md3);

  md1.defFontSize := 10;
  md2.defFontSize := 10;
  md3.defFontSize := 10;

  md1.align := alLeft;
  md3.align := alRight;
  md2.align := alClient;

  SELF.width  := 740;
  SELF.height := 960;

  md1.width := SELF.width div 3;
  md2.width := SELF.width div 3;
  md3.width := SELF.width div 3;

  case aHelpType of   htHelp: begin
                                loadMarkDownFromResource(md1, 'resource_mdHelp1');
                                loadMarkDownFromResource(md2, 'resource_mdHelp2');
                                loadMarkDownFromResource(md3, 'resource_mdHelp3'); end;
                    htImages: begin
                                loadMarkDownFromResource(md1, 'resource_mdImages1');
                                loadMarkDownFromResource(md2, 'resource_mdImages2');
                                loadMarkDownFromResource(md3, 'resource_mdImages3'); end;
  end;

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;
end;

procedure THelpForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := SELF.Handle; // normally application.handle
end;

procedure THelpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  helpForm.free; helpForm := NIL;
end;

initialization
  helpForm := NIL;

finalization
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;

end.
