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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  THelpForm = class(TForm)
    backPanel: TPanel;
    buttonPanel: TPanel;
    shiftLabel: TLabel;
    lb: TListBox;
    moveLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure lbDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
  private
    FListBoxWndProc: TWndMethod;
    function  loadHelp: boolean;
    procedure ListBoxWndProc(var Msg: TMessage);
  protected
    procedure CreateParams(var Params: TCreateParams);
  public
  end;

function showHelp(const Pt: TPoint; const createNew: boolean = TRUE): boolean;
function showingHelp: boolean;
function shutHelp: boolean;

implementation

uses ShellAPI, UICtrls, system.strUtils;

const
  COL_GAP1 = 24;
  COL_GAP2 = 6;

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
  height  :=  (lb.items.count * lb.itemHeight) + buttonPanel.height + 2; // make sure the window is tall enough to show all the items and wide enough for the longest item
  width   :=  lb.canvas.textWidth('Left-click the window and hold') {widest in column 1} + COL_GAP1
          +   lb.canvas.textWidth('[T]ab back through the media file a 100th (default), 200th or 10th of its duration (use ALT (10th) and CAPS LOCK (200th) to modify). Can be held down for rapid tabbing.') + 1; {widest in column 2}

  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;

  FListBoxWndProc := lb.WindowProc;  // save old window proc
  lb.WindowProc   := listBoxWndProc; // subclass
  lb.Style        := lbOwnerDrawFixed;
end;

procedure THelpForm.FormDestroy(Sender: TObject);
begin
  lb.WindowProc   := FListBoxWndProc; // restore window proc
  FListBoxWndProc := NIL;
end;

function THelpForm.loadHelp: boolean;
var
  i: integer;
  posBar: integer;

  function rightSideBlank: boolean;
  var rightSide: string;
  begin
    result := TRUE;
    rightSide := trim(lb.items[i]);
    posBar := pos('|', rightSide);
    case posBar = 0 of TRUE: EXIT; end;
    delete(rightSide, 1, posBar);
    result := rightSide = '';
  end;
begin
  lb.items.beginUpdate;
  try
    lb.items.LoadFromFile('readme.md');
    for i := lb.items.count - 1 downto 0 do begin
      case (pos('|', lb.items[i]) = 0) and (pos('#', lb.items[i]) = 0) of TRUE: begin lb.items.delete(i); CONTINUE; end;end;
      case rightSideBlank of TRUE: lb.items[i] := replaceStr(lb.items[i], '|', ''); end;
      lb.items[i] := replaceStr(lb.items[i], '`', '');
      lb.items[i] := replaceStr(lb.items[i], '###', '');
      lb.items[i] := replaceStr(lb.items[i], #9, '');
    end;
  finally
    lb.items.endUpdate;
  end;
end;

procedure THelpForm.lbDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  var vText: string := lb.items[index];

  case odSelected in State of TRUE: begin
                                      lb.Canvas.Brush.Color := $2B2B2B;
                                      lb.Canvas.Font.Color  := clWhite; end;end;

  var vCenterText: integer := (Rect.Bottom - Rect.Top - lb.Canvas.TextHeight(text)) div 2;

  // use the vertical bar in the text like a tab character to implement two columns
  var posBar: integer := pos('|', vText);
  case posBar = 0 of  TRUE: begin  // no vertical bar? Use the text as a section header and make it bold
                              lb.canvas.font.Style := [fsBold];
                              lb.Canvas.TextOut(Rect.left, Rect.Top + vCenterText, vText); end;
                     FALSE: begin
                              lb.Canvas.TextOut(Rect.left, Rect.Top + vCenterText, copy(vText, 1, posBar - 1)); // text before the vertical bar
                              delete(vText, 1, posBar);
                              // use the longest text in the first column to determine the position of the second column
                              var tab1: integer := lb.canvas.textWidth('Left Click on the window and hold');
                              posBar := pos('|', vText); // is there a second vertical bar ?
                              case posBar = 0 of  TRUE: lb.canvas.TextOut(rect.left + tab1 + COL_GAP1, Rect.Top + vCenterText, vText); // all the text after the first vertical bar
                                                 FALSE: begin
                                                          lb.canvas.TextOut(rect.left + tab1 + COL_GAP1, Rect.Top + vCenterText, copy(vText, 1, posBar - 1)); // text before the bar
                                                          var tab2: integer := lb.canvas.textWidth('when zoomed in / out:');
                                                          lb.canvas.TextOut(rect.left + tab1 + COL_GAP1 + tab2 + COL_GAP2, Rect.Top + vCenterText, copy(vText, posBar + 1, 255)); // text after the bar
                                                        end;end;

                              end;end;

  if odFocused in State then lb.Canvas.DrawFocusRect(Rect); // prevents the dotted box around a focused item. It gets XOR-ed in the VCL's call to DrawFocusRect.
end;

procedure THelpForm.ListBoxWndProc(var Msg: TMessage);
begin
  ShowScrollBar(lb.Handle, SB_HORZ, FALSE);
//  ShowScrollBar(lb.Handle, SB_VERT, FALSE);
  FListBoxWndProc(Msg); // process message
end;

initialization
  helpForm := NIL;

finalization
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;

end.
