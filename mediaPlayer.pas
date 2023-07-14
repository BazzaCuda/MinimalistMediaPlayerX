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
unit mediaPlayer;

interface

uses
  MMFMediaEngineClass, forms, vcl.extCtrls;

type
  TMPEngine = (mpeNone, mpeMMF, mpeWMP);

  TMediaPlayer = class(TObject)
  strict private
   FMPEngine: TMPEngine;
   MMFMediaEngine: TcMediaEngine;
   FVideoPanel: TPanel;
   FVolume: integer;
  private
    constructor create;
    destructor  Destroy; override;
    function createSubTitleLayer: boolean;
    function releaseMMFEngine: HRESULT;
    function getVolume: integer;

    {property setters}
    procedure setMPEngine(const Value: TMPengine);
    procedure setVolume(const Value: integer);
    function getSubTitle: string;
  public
    function initMediaPlayer(aForm: TForm): boolean;
    function openURL(aURL: string): boolean;
    function play: boolean;
    function release: boolean;
    function setProgressBar: boolean;
    function stop: boolean;
    function volDown: boolean;
    function volUp: boolean;
    property MPEngine: TMPengine read FMPEngine write setMPEngine;
    property subTitle: string    read getSubTitle;
    property volume: integer     read getVolume write setVolume;
  end;

function MP: TMediaPlayer;

implementation

uses
  vcl.controls, vcl.graphics, winAPI.windows, globalVars, MMFTimedTextNotifyClass, formSubtitles, progressBar, _debugWindow;

var
  gMP: TMediaPlayer;

function MP: TMediaPlayer;
begin
  case gMP = NIL of TRUE: gMP := TMediaPlayer.create; end;
  result := gMP;
end;

{ TMediaPlayer }

constructor TMediaPlayer.create;
begin
  inherited;
  FMPEngine := mpeNone;
end;

function TMediaPlayer.createSubTitleLayer: boolean;
begin
  ST.initSubtitles(FVideoPanel);
end;

destructor TMediaPlayer.Destroy;
begin
  ReleaseMMFEngine;
  case MMFMediaEngine <> NIL of TRUE: MMFMediaEngine.free; end;
  inherited;
end;

function TMediaPlayer.getSubTitle: string;
begin
  result := MMFMediaEngine.pr_TimedTextNotify.SubTitle;
end;

function TMediaPlayer.getVolume: integer;
begin
  result := FVolume;
end;

function TMediaPlayer.initMediaPlayer(aForm: TForm): boolean;
begin
  FVideoPanel        := TPanel.create(aForm);
  FVideoPanel.parent := aForm;
  FVideoPanel.align  := alClient;
  FVideoPanel.color  := clBlack;
end;

function TMediaPlayer.openURL(aURL: string): boolean;
var
  hr: HRESULT;
begin
  result := FALSE;
  releaseMMFEngine;
  createSubTitleLayer; // the third param needs the subtitle form handle
  MMFMediaEngine := TcMediaEngine.Create(FVideoPanel.handle, GV.mainWnd, ST.HWND, hr); // does this really need windowHandle rather than handle?
  case SUCCEEDED(hr) of FALSE: EXIT; end;

  hr := MMFMediaEngine.OpenURL(PWideChar(aURL), EXTSUBRIP);
  result := SUCCEEDED(hr);
end;

function TMediaPlayer.play: boolean;
begin
  result := FALSE;
  case assigned(MMFMediaEngine) of FALSE: EXIT; end;
  result := SUCCEEDED(MMFMediaEngine.Play);
end;

function TMediaPlayer.release: boolean;
begin
  releaseMMFEngine;
end;

function TMediaPlayer.releaseMMFEngine(): HRESULT;
begin
  case assigned(MMFMediaEngine) of   TRUE:  begin
                                              MMFMediaEngine.FlushPlayer(); // To safely release all resources from the IMFMEdiaEngineEx, you need to call this function first!
                                              MMFMediaEngine.Free;
                                              MMFMediaEngine := NIL;
                                              result := S_OK; end;
                                    FALSE:  result := E_POINTER; end;
end;

procedure TMediaPlayer.setMPEngine(const Value: TMPengine);
begin
  case FMPEngine = mpeNone of TRUE: FMPEngine := Value; end;
end;

function TMediaPlayer.setProgressBar: boolean;
begin
  case assigned(MMFMediaEngine) of FALSE: EXIT; end;
  PB.max := trunc(MMFMediaEngine.pu_Duration);
  case MMFMediaEngine.pu_Duration > 0 of TRUE: PB.position := trunc(MMFMediaEngine.pu_CurrPosition); end;
end;

procedure TMediaPlayer.setVolume(const Value: integer);
begin
  FVolume := value;
  case FVolume > 100 of TRUE: FVolume := 100; end;
  case FVolume < 0   of TRUE: FVolume := 0; end;
  MMFMediaEngine.setVolume(FVolume * 0.01);
end;

function TMediaPlayer.stop: boolean;
begin
  MMFMediaEngine.Stop;
end;

function TMediaPlayer.volDown: boolean;
begin
  volume := volume - 1;
end;

function TMediaPlayer.volUp: boolean;
begin
  volume := volume + 1;
end;

initialization
  gMP := NIL;

finalization
  case gMP <> NIL of TRUE: gMP.free; end;

end.
