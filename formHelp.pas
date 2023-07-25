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

function showHelp(Pt: TPoint; createNew: boolean = TRUE): boolean;
function shutHelp: boolean;

implementation

uses ShellAPI, UICtrls;

const
  COL_GAP1 = 24;
  COL_GAP2 = 6;

var
  helpForm: THelpForm;

function showHelp(Pt: TPoint; createNew: boolean = TRUE): boolean;
begin
  case (helpForm = NIL) and createNew of TRUE: helpForm := THelpForm.create(NIL); end;
  case helpForm = NIL of TRUE: EXIT; end; // createNew = FALSE and there isn't a current help window. Used for repositioning the window when the main UI moves or resizes.

  helpForm.show;
  WinAPI.Windows.setWindowPos(helpForm.handle, HWND_TOP, Pt.X, Pt.Y, 0, 0, SWP_SHOWWINDOW + SWP_NOSIZE);
  EnableWindow(helpForm.handle, FALSE);    // this window won't get any keyboard or mouse messages, etc.
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
          +   lb.canvas.textWidth('[T]ab back through the media file a 100th (Alt-T a 50th, Shift-T a 20th, Caps Lock T a 10th) of its duration') + 1; {widest in column 2}

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

begin
  lb.items.beginUpdate;
  try
  with lb.items do begin
    add('Alt-SpaceBar|Activate the system menu to get to the About Box (but not while this Help window is open)');
    add('ESCape|exit Fullscreen mode, or exit the app if it''s in window mode');
    add('SpaceBar|pause / resume playback (also left double-click in the window, or right single-click)');
    add('A|play the first media file in the playlist (Z plays last)');
    add('B|[B]lackout / restore progress [B]ar');
    add('C|show / hide the on-screen [C]ontrol for the media file timestamp');
    add('Ctrl-C|show / hide all the on-screen [C]ontrols: media file timestamp and media file metadata');
    add('D and Del|[D]elete the current media file (after confirmation)');
    add('Ctrl-D and Ctrl-Del|[D]elete all files in the current media file''s folder (after confirmation)');
    add('E|[E]ars - Mute / Unmute sound');
    add('F|show / cancel [F]ullScreen mode');
    add('G|[G]reater window size');
    add('Ctrl-G|reduce, i.e. un[G]reater, the window size');
    add('H|position the window [H]orizontally (and Vertically) in the center of the screen');
    add('I|zoom [I]n by 10% of the video''s height and width');
    add('J|ad[J]ust the window''s aspect ratio to match the video''s aspect ratio');
    add('K|mark this media file as [K]eep (adds an underscore _ to the beginning of the file name)');
    add('L|re[L]oad the list of supported media files from the current folder');
    add('M|[M]aximize / restore the window');
    add('N|mi[N]imize the window to the Windows taskbar');
    add('O|zoom [O]ut by 10% of the video''s height and width');
    add('P|pause the media file and play it with [P]otplayer instead, if it''s installed');
    add('Q|play the previous media file in the [Q]ueue / playlist');
    add('R|[R]ename the current media file');
    add('S|re[S]tart the current media file from the beginning, aka [S]tartover');
    add('T|[T]ab through the media file a 100th (Alt-T a 50th, Shift-T a 20th, Caps Lock T a 10th) of its duration');
    add('Ctrl-T|[T]ab back through the media file a 100th (Alt-T a 50th, Shift-T a 20th, Caps Lock T a 10th) of its duration'); // this is the longest item
    add('Tab|tab forwards 1/200th the duration of the media file');
    add('Ctrl-Tab|tab backwards 1/200th the duration of the media file');
    add('U|[U]nzoom, i.e. re-fit the video to the window');
    add('V|maximize / restore the [V]iew, same as [M]');
    add('W|[W]atch the next video in the list (or play the next audio)');
    add('X|e[X]it the application');
    add('Y|tr[Y]out the media file by sampling it at various points, until [Y] is pressed again');
    add('Z|play the last media file in the playlist (A plays the first)');
    add('Up Arrow|increase the volume by 1%');
    add('Down Arrow|decrease the volume by 1%');
    add('Ctrl-Up Arrow|increase the playback speed by 10%');
    add('Ctrl-Down Arrow|decrease the playback speed by 10%');
    add('Right Arrow|step forwards one frame and freeze the video');
    add('Left Arrow|step backwards one frame and freeze the video');
    add('/|increase playback speed by 10%');
    add('\|decrease playback speed by 10%');
    add('0|briefly show the media caption');
    add('1|reset the playback speed to normal, i.e. [1]00%');
    add('2|resize the window so that 2 instances of the application can be placed side-by-side');
    add('4|resize to a mini-window in the top-right corner of the screen');
    add('Ctrl-4|move the window to the top-right corner of the screen but maintain its current size');
    add('5|save / bookmark the current media file''s timestamp to an INI file');
    add('6|retrieve a saved / bookmarked media file timestamp from an INI file and continue playback from that point');
    add('7|delete any previously saved / bookmarked INI file for the current media file');
    add('8|set the video to 1 pixel larger than the window on all four sides');
    add('9|resize the window to the width of the video');
    add('=|copy the media file''s name to the clipboard');
    add('F12|pause the media file and open it in the Shotcut video editor, if it''s installed');
    add('Zoom');
    add('Ctrl-Up Arrow|when zoomed in / out:|move the video Up inside the window');
    add('Ctrl-Down Arrow|when zoomed in / out:|move the video Down inside the window');
    add('Ctrl-Right Arrow|when zoomed in / out:|move the video Right inside the window');
    add('Ctrl-Left Arrow|when zoomed in / out:|move the video Left inside the window');
    add('Additional');
    add('Left double-click the window|pause / resume playback');
    add('Right single-click the window|pause / resume playback');
    add('Left-click the window and hold|the window can be dragged around and repositioned');
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
                              var tab1: integer := lb.canvas.textWidth('Left Click window and hold');
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
  ShowScrollBar(lb.Handle, SB_VERT, FALSE);
  FListBoxWndProc(Msg); // process message
end;

initialization
  helpForm := NIL;

finalization
  case helpForm <> NIL of TRUE: begin helpForm.close; helpForm.free; helpForm := NIL; end;end;

end.
