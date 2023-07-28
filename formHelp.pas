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
unit formHelp;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  THelpForm = class(TForm)
    backPanel: TPanel;
    buttonPanel: TPanel;
    shiftLabel: TLabel;
    moveLabel: TLabel;
    RT: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    function  loadHelp: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
  end;

function showHelp(const Pt: TPoint; const createNew: boolean = TRUE): boolean;
function showingHelp: boolean;
function shutHelp: boolean;

implementation

uses ShellAPI, UICtrls, system.strUtils;

var
  helpForm: THelpForm;

function showingHelp: boolean;
begin
  result := helpForm <> NIL;
end;

function showHelp(const Pt: TPoint; const createNew: boolean = TRUE): boolean;
begin
  case (helpForm = NIL) and createNew of TRUE: helpForm := THelpForm.create(NIL); end;
  case helpForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current help window. Used for repositioning the window when the main UI moves or resizes.

  helpForm.show;
  winAPI.Windows.setWindowPos(helpForm.handle, HWND_TOP, Pt.X, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  enableWindow(helpForm.handle, FALSE);    // this window won't get any keyboard or mouse messages, etc.
  setForegroundWindow(UI.handle); // so the UI keyboard functions can still be used when this form is open.
end;

function shutHelp: boolean;
begin
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;
  helpForm := NIL;
end;

{$R *.dfm}

procedure THelpForm.CreateParams(var Params: TCreateParams);
// no taskbar icon for the app
begin
  inherited;
  Params.ExStyle    := Params.ExStyle or (WS_EX_APPWINDOW);
  Params.WndParent  := self.Handle; // normally application.handle
end;

procedure THelpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  helpForm.free; helpForm := NIL;
end;

procedure THelpForm.FormCreate(Sender: TObject);
begin
  loadHelp;
  RT.align          := alClient;
  RT.plainText      := FALSE;
  RT.hideSelection  := TRUE;
  RT.bevelInner     := bvNone;
  RT.bevelOuter     := bvNone;
  RT.borderStyle    := bsNone;
  RT.readOnly       := TRUE;
  RT.lines.loadFromFile('help.rtf');

  SELF.width  := 700;
  SELF.height := 820;

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;
end;

function THelpForm.loadHelp: boolean;
var
  i: integer;
  posBar: integer;

//  function rightSideBlank: boolean;
//  var rightSide: string;
//  begin
//    result := TRUE;
//    rightSide := trim(lb.items[i]);
//    posBar := pos('|', rightSide);
//    case posBar = 0 of TRUE: EXIT; end;
//    delete(rightSide, 1, posBar);
//    result := rightSide = '';
//  end;
begin
//  lb.items.beginUpdate;
//  try
//    lb.items.LoadFromFile('readme.md');
//    for i := lb.items.count - 1 downto 0 do begin
//      case (pos('|', lb.items[i]) = 0) and (pos('#', lb.items[i]) = 0) of TRUE: begin lb.items.delete(i); CONTINUE; end;end;
//      case rightSideBlank of TRUE: lb.items[i] := replaceStr(lb.items[i], '|', ''); end;
//      lb.items[i] := replaceStr(lb.items[i], '`', '');
//      lb.items[i] := replaceStr(lb.items[i], '###', '');
//      lb.items[i] := replaceStr(lb.items[i], #9, '');
//    end;
//  finally
//    lb.items.endUpdate;
//  end;
end;

initialization
  helpForm := NIL;

finalization
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;

end.
