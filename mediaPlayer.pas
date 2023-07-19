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
  MMFMediaEngineClass, forms, vcl.extCtrls, types, system.classes;

type
  TMPEngine   = (mpeNone, mpeMMF, mpeWMP);
  TPlayState  = (psPaused, psPlaying);

  TMediaPlayer = class(TObject)
  strict private
   FMPEngine: TMPEngine;
   MMFMediaEngine: TcMediaEngine;
   FPlayState: TPlayState;
   FVideoPanel: TPanel;
   FVolume: integer;
  private
    constructor create;
    function createSubTitleLayer: boolean;
    function releaseMMFEngine: HRESULT;
    function getVolume: integer;

    {property setters}
    procedure setMPEngine(const Value: TMPengine);
    procedure setVolume(const Value: integer);
    function  getDuration: integer;
    function  getPosition: integer;
    function  getSubTitle: string;
    procedure setPosition(const Value: integer);
    function getFormattedDuration: string;
    function getFormattedTime: string;
  public
    destructor  Destroy; override;
    function frameBackwards: boolean;
    function frameForwards: boolean;
    function initMediaPlayer(aForm: TForm): boolean;
    function openURL(aURL: string): boolean;
    function pause: boolean;
    function pausePlay: boolean;
    function play: boolean;
    function release: boolean;
    function setProgressBar: boolean;
    function stop: boolean;
    function tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
    function volDown: boolean;
    function volUp: boolean;
    property duration:            integer   read getDuration;
    property formattedDuration:   string    read getFormattedDuration;
    property formattedTime:       string    read getFormattedTime;
    property MPEngine:            TMPengine read FMPEngine    write setMPEngine;
    property position:            integer   read getPosition  write setPosition;
    property subTitle:            string    read getSubTitle;
    property volume:              integer   read getVolume    write setVolume;
  end;

function MP: TMediaPlayer;

implementation

uses
  vcl.controls, vcl.graphics, winAPI.windows, globalVars, MMFTimedTextNotifyClass, formSubtitles, progressBar, keyboard, commonUtils, system.sysUtils,
  formCaption, _debugWindow;

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
  MC.initCaption(FVideoPanel);
end;

destructor TMediaPlayer.Destroy;
begin
  ReleaseMMFEngine;
  case MMFMediaEngine <> NIL of TRUE: MMFMediaEngine.free; end;
  inherited;
end;

function TMediaPlayer.frameBackwards: boolean;
begin
  MMFMediaEngine.FrameStep(FALSE);
end;

function TMediaPlayer.frameForwards: boolean;
begin
  MMFMediaEngine.FrameStep(TRUE);
end;

function TMediaPlayer.getDuration: integer;
begin
  result := trunc(MMFMediaEngine.pu_Duration);
end;

function TMediaPlayer.getFormattedDuration: string;
begin
  result := MMFMediaEngine.getFormattedSeconds(duration);
end;

function TMediaPlayer.getFormattedTime: string;
begin
  result := MMFMediaEngine.getFormattedSeconds(position);
end;

function TMediaPlayer.getPosition: integer;
begin
  result := trunc(MMFMediaEngine.pu_CurrPosition);
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
  FVideoPanel.BevelOuter := bvNone;
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

function TMediaPlayer.pause: boolean;
begin
  MMFMediaEngine.Pause;
  FPlayState := psPaused;
end;

function TMediaPlayer.pausePlay: boolean;
begin
  case MMFMediaEngine.pu_RenderingState of
    rsPlaying: MMFMediaEngine.Pause();
    rsStopped, rsPaused: MMFMediaEngine.Play;
    rsInitialised: MMFMediaEngine.Play;
  end;
end;

function TMediaPlayer.play: boolean;
begin
  result := FALSE;
  case assigned(MMFMediaEngine) of FALSE: EXIT; end;
  result := SUCCEEDED(MMFMediaEngine.Play);
  case result of TRUE: FPlayState := psPlaying; end;
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

procedure TMediaPlayer.setPosition(const Value: integer);
var
  hr: HRESULT;
begin
  hr := MMFMediaEngine.pr_MediaEngine.SetCurrentTime(value);
  case FAILED(hr) of TRUE: debug('no setPosition'); end;
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

function TMediaPlayer.tab(aShiftState: TShiftState; capsLock: boolean; aFactor: integer = 0): boolean;
var
  vFactor: integer;
  vTab: integer;
begin
  case aFactor <> 0 of  TRUE: vFactor := aFactor;
                       FALSE: vFactor := 100; end;

  case capsLock               of TRUE: vFactor := 10; end;
  case ssShift in aShiftState of TRUE: vFactor := 20; end;
  case ssAlt   in aShiftState of TRUE: vFactor := 50; end;
  vTab := trunc(duration / vFactor);

  pause; delay(100);
  case ssCtrl  in aShiftState of  TRUE: position := position - vTab;
                                 FALSE: position := position + vTab; end;
  delay(100); play;

  var newInfo := format('%dth = %s', [vFactor, formatSeconds(round(duration / vFactor))]);
  case ssCtrl in aShiftState of  TRUE: newInfo := '<< ' + newInfo;
                                FALSE: newInfo := '>> ' + newInfo;
  end;
  ST.opInfo := newInfo;
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
