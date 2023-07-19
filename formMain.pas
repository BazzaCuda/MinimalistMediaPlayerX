{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TMMPUI = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
  protected
  public
  end;

var
  MMPUI: TMMPUI;

implementation

uses
  uiCtrls, globalVars, mediaPlayer, consts, commonUtils, _debugWindow, playlist, progressBar, mediaInfo;

{$R *.dfm}

{ TMMPUI }

procedure TMMPUI.FormCreate(Sender: TObject);
begin
  styleElements := [];
  color := clBlack;
  initUI(SELF);
  PB.initProgressBar(SELF);
  GV.mainWnd := APPLICATION.HANDLE; // or SELF.HANDLE ?  hmmm.
  MP.initMediaPlayer(SELF);

  GV.mainForm := SELF;
  GV.UIWnd    := SELF.HANDLE;


  PL.add('B:\Movies\Blazing Saddles (1974).mp4');
  PL.add('B:\Movies\Nobody (2021).mp4');
  PL.add('B:\Movies\Leon The Professional (1994) extended.mp4');
  PL.add('B:\Movies\LOTR 1 - The Fellowship of the Ring (2001) extended.mp4');
  PL.add('B:\Movies\Blazing Saddles (1974).mp4');
  PL.add('B:\Movies\Let The Right One In (2008).mp4');
//  PL.add('B:\Videos\Airplane 2 - The Sequel (6_10) Movie CLIP - It''s Very Likely That We''re All Going to Die (1982) HD [edited].mp4');
//  PL.add('B:\Videos\ManorSolomon.mp4');
  PL.add('B:\AudioLibrary\Harry Potter\01 - Harry Potter and The Philosopher''s Stone\Harry Potter and the Philosopher''s Stone 1.mp3');

  PL.first; PL.next; PL.next;

  case MP.openURL(PL.currentItem) of FALSE: EXIT; end;

  MP.play;
  MP.volume := 100; // there seems to be a problem with setting this before .play, which is concerning
//  MP.position := MP.duration div 2; // TEMPORARY
  delay(100);
  MI.initMediaInfo('B:\Movies\Blazing Saddles (1974).mp4');
end;


procedure TMMPUI.FormResize(Sender: TObject);
begin
  GV.mainTop   := top;
  GV.mainLeft  := left;
  GV.mainWidth := width;
end;

end.
