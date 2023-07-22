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
  uiCtrls, globalVars, mediaPlayer, consts, commonUtils, _debugWindow, playlist, progressBar, mediaInfo, formSubtitles, formCaption;

{$R *.dfm}

{ TMMPUI }

procedure TMMPUI.FormCreate(Sender: TObject);
var
  url: string;
begin
  url := 'https://rr5---sn-cu-cgnl.googlevideo.com/videoplayback?expire=1689994186&ei=au-6ZKXcIbXFmLAPvv27gAM&ip=';
  url := url + '109.181.218.17&id=o-ABEfS1e4PkA6sfTUKRreCZXFI1DwTsCX0uHGEUzjiwwq&itag=18&source=youtube&requiressl=yes&mh=Eb&mm=';
  url := url + '31%2C29&mn=sn-cu-cgnl%2Csn-cu-auod&ms=au%2Crdu&mv=m&mvi=5&pl=25&pcm2=yes&initcwndbps=1366250&bui=AYlvQAuXlsA9k7oy';
  url := url + 'VexC2ea29Gm8dJJjH25U2Lf8nNRQ12VycmeR7m1JpxLEUbiew_3u0xuOr0mjoUaK0AEqP9GdCiRt_dJG&spc=Ul2Sq2Nvs5bQcHYyDh3_fwNSmSBW2yPCLdj1th';
  url := url + 'Wm_A&vprv=1&svpuc=1&mime=video%2Fmp4&ns=OS7hDAwphTEImNvjEeVSxC0O&gir=yes&clen=60664831&ratebypass=yes&dur=1376.502&lmt=16888';
  url := url + '53632278732&mt=1689972086&fvip=1&fexp=24007246&c=WEB&txp=5319224&n=UzZg2dEdPxcYte-PIL&sparams=expire%2Cei%2Cip%2Cid%2Citag%2';
  url := url + 'Csource%2Crequiressl%2Cpcm2%2Cbui%2Cspc%2Cvprv%2Csvpuc%2Cmime%2Cns%2Cgir%2Cclen%2Cratebypass%2Cdur%2Clmt&sig=AOq0QJ8wRQIhAIVd';
  url := url + 'pIdaWrwRdpr-8HBRULOr3e9uPqx3rmx-nctNJwHkAiBu4FoCbOri9W1WxT4WL3t7T_bPM5Po3IuXzgj0leeSQw%3D%3D&lsparams=mh%2Cmm%2Cmn%2Cms%2Cmv%2';
  url := url + 'Cmvi%2Cpl%2Cinitcwndbps&lsig=AG3C_xAwRQIhAKu0daOCiSE7I6gBosqnX7nTuzOF6anRrWgl5mti_a2QAiBGbLZsZYHvs_SaCrxzuKV7IhnI0dTY_msRc1UKI7UzDA%3D%3D';
  UI.initUI(SELF);
  MP.initMediaPlayer(SELF);
  ST.initSubtitles(UI.videoPanel);
  MC.initCaption(UI.videoPanel);
  PB.initProgressBar(ST);
  GV.mainWnd := APPLICATION.HANDLE; // or SELF.HANDLE ?  hmmm.

  GV.mainForm := SELF;
  GV.UIWnd    := SELF.HANDLE;

//  PL.add(url);
  PL.add('B:\Movies\Blazing Saddles (1974).mp4');
  PL.add('B:\Movies\Nobody (2021).mp4');
  PL.add('B:\Movies\Leon The Professional (1994) extended.mp4');
  PL.add('B:\Movies\LOTR 1 - The Fellowship of the Ring (2001) extended.mp4');
  PL.add('B:\Movies\Let The Right One In (2008).mp4');
  PL.add('B:\Videos\Airplane 2 - The Sequel (6_10) Movie CLIP - It''s Very Likely That We''re All Going to Die (1982) HD [edited].mp4');
//  PL.add('B:\Videos\ManorSolomon.mp4');
//  PL.add('B:\AudioLibrary\Harry Potter\01 - Harry Potter and The Philosopher''s Stone\Harry Potter and the Philosopher''s Stone 1.mp3');

  PL.first; // PL.next; PL.next; PL.next; PL.next; PL.next; PL.next;

  case MP.openURL(PL.currentItem) of FALSE: EXIT; end;

  MP.play;
//  MP.volume := 100;
//  delay(100);
  MI.initMediaInfo(PL.currentItem);
  MI.getData(ST.dataMemo);
  MC.caption := PL.formattedItem;
//  ST.subTitle := 'This might still be useful';

end;


procedure TMMPUI.FormResize(Sender: TObject);
begin
  GV.mainTop   := top;
  GV.mainLeft  := left;
  GV.mainWidth := width;
end;

end.
