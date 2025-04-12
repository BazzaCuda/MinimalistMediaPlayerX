{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
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
unit model.mmpMediaPlayer;

interface

uses
  winApi.windows,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  IMediaPlayer = interface(ISubscribable)
    ['{7666FECA-9BF6-4422-BB68-8EEAF6A6E6F7}']
    function  initMediaPlayer(const aHWND: HWND): boolean;
    function  notify(const aNotice: INotice): INotice;
  end;

function newMediaPlayer: IMediaPlayer;

implementation

uses
  system.sysUtils,
  MPVBasePlayer,
  mmpConsts, mmpFileUtils, mmpDoProcs, mmpFuncProg, mmpGlobalState, mmpTickTimer, mmpUtils,
  model.mmpConfigFile, model.mmpMediaTypes, model.mmpMPVCtrls, model.mmpMPVProperties,
  _debugWindow;

type
  TMediaPlayer = class(TInterfacedObject, IMediaPlayer)
  strict private
    mpv: TMPVBasePlayer;

    FCheckCount:              integer;
    FDimensionsDone:          boolean;
    FIgnoreTicks:             boolean;
    FImageDisplayDurationMs:  double;
    FMediaType:               TMediaType;
    FMPVScreenshotDirectory:  string;
    FNotifier:                INotifier;
    FVideoHeight:             integer;
    FVideoWidth:              integer;
  private
    procedure   onFileOpen(Sender: TObject; const aFilePath: string);
    procedure   onInitMPV(sender: TObject);
    procedure   onStateChange(cSender: TObject; eState: TMPVPlayerState);

    function    onNotify(const aNotice: INotice):     INotice;
    function    onTickTimer(const aNotice: INotice):  INotice;

    function    openURL(const aURL: string):          boolean;
    function    pausePlay:                            string;
    function    pausePlayImages:                      string;
    function    sendOpInfo(const aOpInfo: string):    boolean;
  protected
    procedure   setPosition(const aValue: integer);
  public
    constructor create;
    destructor  Destroy; override;
    function    initMediaPlayer(const aHWND: HWND):   boolean;
    function    notify(const aNotice: INotice):       INotice;

    // ISubscribable
    function    subscribe(const aSubscriber: ISubscriber): ISubscriber;
    procedure   unsubscribe(const aSubscriber: ISubscriber);

  end;

function newMediaPlayer: IMediaPlayer;
begin
  result := TMediaPlayer.create;
end;

{ TMediaPlayer }

constructor TMediaPlayer.create;
begin
  FNotifier := newNotifier;
  TT.subscribe(newSubscriber(onTickTimer));
  appEvents.subscribe(newSubscriber(onNotify));
end;

destructor TMediaPlayer.Destroy;
begin
  case mpv = NIL of FALSE: mpv.free; end;
  inherited;
end;

function TMediaPlayer.initMediaPlayer(const aHWND: HWND): boolean;
begin
  result := FALSE;

  case mpv = NIL of TRUE: mpvCreate(mpv); end;

  case mpv = NIL of TRUE: EXIT; end;

  mpv.OnFileOpen   := onFileOpen;
  mpv.OnStateChged := onStateChange;
  mpv.onInitMPV    := onInitMPV;

  mpvInitPlayer(mpv, aHWND, mmpExePath, mmpExePath);

  mpvGetPropertyString(mpv, 'screenshot-directory', FMPVScreenshotDirectory);
  mmpDo(evGSMPVScreenshotDirectory, FMPVScreenshotDirectory);

  var vImageDisplayDuration: string;
  mpvGetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, vImageDisplayDuration);
  vImageDisplayDuration   := mmp.use(vImageDisplayDuration = 'inf', IMAGE_DISPLAY_DURATION_STRING, vImageDisplayDuration); // if there's no image-display-duration= entry at all in mpv.conf, MPV defaults to 5
  FImageDisplayDurationMs := strToFloatDef(vImageDisplayDuration, IMAGE_DISPLAY_DURATION);                                // if the image-display-duration= entry isn't a valid integer
  mmpDo(evGSIDD, trunc(FImageDisplayDurationMs));                // stored as seconds, same as in mpv.conf
  FImageDisplayDurationMs := FImageDisplayDurationMs * 1000;     // this property isn't currently used anywhere (see evMPReqIDD)
  mpvSetPropertyString(mpv, MPV_IMAGE_DISPLAY_DURATION, 'inf');  // get the user's duration setting, if any, then override it. MMP controls how long an image is displayed for, not MPV

  pausePlayImages; // default is paused;

  mpvSetVolume(mpv, CF.asInteger[CONF_VOLUME]);
  mpvSetMute(mpv, CF.asBoolean[CONF_MUTED]);

  result := TRUE;
end;

function TMediaPlayer.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TMediaPlayer.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;

  case aNotice.event of
    evMPOpenUrl:          openUrl(aNotice.text);
    evMPBrightnessDn:     sendOpInfo(mpvBrightnessDn(mpv));
    evMPBrightnessReset:  sendOpInfo(mpvBrightnessReset(mpv));
    evMPBrightnessUp:     sendOpInfo(mpvBrightnessUp(mpv));
    evMPContrastDn:       sendOpInfo(mpvContrastDn(mpv));
    evMPContrastReset:    sendOpInfo(mpvContrastReset(mpv));
    evMPContrastUp:       sendOpInfo(mpvContrastUp(mpv));
    evMPCycleAudio:       mpvCycleAudio(mpv);
    evMPCycleSubs:        mpvCycleSubs(mpv);
    evMPFrameBackwards:   mpvFrameBackwards(mpv);
    evMPFrameForwards:    mpvFrameForwards(mpv);
    evMPGammaDn:          sendOpInfo(mpvGammaDn(mpv));
    evMPGammaReset:       sendOpInfo(mpvGammaReset(mpv));
    evMPGammaUp:          sendOpInfo(mpvGammaUp(mpv));
    evMPKeepOpen:         mpvSetKeepOpen(mpv, aNotice.tf);
    evMPMuteUnmute:       sendOpInfo(mpvMuteUnmute(mpv));
    evMPNextChapter:      mpvChapterNext(mpv);
    evMPPanDn:            sendOpInfo(mpvPanDn(mpv));
    evMPPanLeft:          sendOpInfo(mpvPanLeft(mpv));
    evMPPanReset:         sendOpInfo(mpvPanReset(mpv));
    evMPPanRight:         sendOpInfo(mpvPanRight(mpv));
    evMPPanUp:            sendOpInfo(mpvPanUp(mpv));
    evMPPause:            mpvPause(mpv);
    evMPPausePlay:        sendOpInfo(pausePlay);
    evMPPrevChapter:      mpvChapterPrev(mpv);
    evMPResetAll:         sendOpInfo(mpvResetAll(mpv));
    evMPResume:           mpvResume(mpv);
    evMPRotateLeft:       sendOpInfo(mpvRotateLeft(mpv));
    evMPRotateReset:      sendOpInfo(mpvRotateReset(mpv));
    evMPRotateRight:      sendOpInfo(mpvRotateRight(mpv));
    evMPSaturationDn:     sendOpInfo(mpvSaturationDn(mpv));
    evMPSaturationReset:  sendOpInfo(mpvSaturationReset(mpv));
    evMPSaturationUp:     sendOpInfo(mpvSaturationUp(mpv));
    evMPScreenshot:       mpvTakeScreenshot(mpv, aNotice.text);
    evMPSpeedDn:          sendOpInfo(mpvSpeedDn(mpv));
    evMPSpeedReset:       sendOpInfo(mpvSpeedReset(mpv));
    evMPSpeedUp:          sendOpInfo(mpvSpeedUp(mpv));
    evMPStartOver:        sendOpInfo(mpvStartOver(mpv));
    evMPStop:             mpvStop(mpv);
    evMPToggleRepeat:     sendOpInfo(mpvToggleRepeat(mpv));
    evMPToggleSubtitles:  sendOpInfo(mpvToggleSubtitles(mpv));
    evMPVolDn:            sendOpInfo(mpvVolDown(mpv));
    evMPVolUp:            sendOpInfo(mpvVolUp(mpv));
    evWheelDn:            sendOpInfo(mpvVolDown(mpv));
    evWheelUp:            sendOpInfo(mpvVolUp(mpv));
    evMPZoomIn:           sendOpInfo(mpvZoomIn(mpv));
    evMPZoomOut:          sendOpInfo(mpvZoomOut(mpv));
    evMPZoomReset:        sendOpInfo(mpvZoomReset(mpv));

    evPBClick:            setPosition(aNotice.integer);

    evMPReqDuration:      aNotice.integer := mpvDuration(mpv);
    evMPReqFileName:      aNotice.text    := mpvFileName(mpv);
    evMPReqIDD:           aNotice.integer := trunc(FImageDisplayDurationMs); // not currently used
    evMPReqPlaying:       aNotice.tf      := mpvState(mpv) = mpsPlay;
    evMPReqPosition:      aNotice.integer := mpvPosition(mpv);
    evMPReqVideoHeight:   aNotice.integer := mpvVideoHeight(mpv);
    evMPReqVideoWidth:    aNotice.integer := mpvVideoWidth(mpv);
  end;
end;

procedure TMediaPlayer.onInitMPV(sender: TObject);
//===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====
begin
  mpvSetDefaults(sender as TMPVBasePlayer, mmpExePath);
end;

procedure TMediaPlayer.onFileOpen(Sender: TObject; const aFilePath: string);
begin
  case FNotifier = NIL of TRUE: EXIT; end;
  case FMediaType of mtAudio, mtVideo:  begin
                                          FNotifier.notifySubscribers(mmpDo(evMPDuration, mpvDuration(mpv)));
                                          FNotifier.notifySubscribers(mmpDo(evMPPosition, 0)); end;end;

  case FMediaType of mtAudio, mtImage: mmpDo(evVMResizeWindow); end; // for mtImage, do it in onTickTimer

  var vNotice     := newNotice;
  vNotice.event   := evVMMPOnOpen;
  vNotice.text    := aFilePath;
  vNotice.integer := mpvDuration(mpv);
  FNotifier.notifySubscribers(vNotice);
end;

procedure TMediaPlayer.onStateChange(cSender: TObject; eState: TMPVPlayerState);
// no mpsStop event as yet
begin
//  TDebug.debugEnum<TMPVPlayerState>('eState ' + extractFileName(mpvFileName(mpv)), eState);
  case eState of
    mpsLoading:   ; // FNotifier.notifySubscribers(mmpDo(evMPStateLoading)); {not currently used}
    mpsEnd:       FNotifier.notifySubscribers(mmpDo(evMPStateEnd));
    mpsPlay:      FNotifier.notifySubscribers(mmpDo(evMPStatePlay));
  end;
end;

function TMediaPlayer.onTickTimer(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case FNotifier = NIL  of TRUE: EXIT; end;
  case FIgnoreTicks     of TRUE: EXIT; end;
  case FMediaType       of mtAudio, mtVideo: FNotifier.notifySubscribers(mmpDo(evMPPosition, mpvPosition(mpv))); end;

  case FDimensionsDone of FALSE:  begin // only ever false for videos
    inc(FCheckCount);
    FDimensionsDone := FCheckCount >= 3; // that's quite enough of that!
    case (mpvVideoWidth(mpv) <> FVideoWidth) or (mpvVideoHeight(mpv) <> FVideoHeight) of TRUE:  begin
                                                                                                  FVideoWidth     := mpvVideoWidth(mpv);
                                                                                                  FVideoHeight    := mpvVideoHeight(mpv);
                                                                                                  mmpDo(evVMResizeWindow); end;end;end;
  end;
end;

function TMediaPlayer.openURL(const aURL: string): boolean;
begin
  result          := FALSE;
  FIgnoreTicks    := TRUE;
  mpvSetKeepOpen(mpv, TRUE);  // VITAL! Prevents the slideshow from going haywire - so the next line won't immediately issue an mpsEnd for an image
  mpvOpenFile(mpv, aURL);     // let MPV issue an mpsEnd event for the current file before we change to the media type for the new file

  FMediaType      := MT.mediaType(aURL);
  FDimensionsDone := FMediaType in [mtAudio, mtImage]; // only applies to video
  case FMediaType of mtAudio, mtVideo: mpvSetKeepOpen(mpv, FALSE); end; // ideally, we only want audio and video files to issue mpsEnd events at end of playback

  FVideoWidth   := 0;
  FVideoHeight  := 0;
  FCheckCount   := 0;
  FIgnoreTicks  := FALSE; // react in onTickTimer

  mmpDo(evGSMediaType, FMediaType);
  mmpDo(evMIGetMediaInfo, aURL, FMediaType);
  mmpDo(evSTUpdateMetaData);
  mmpDo(evMCCaption, mmpDo(evPLReqFormattedItem).text);
  result := TRUE;
end;

function TMediaPlayer.pausePlay: string;
begin
  case FMediaType of
    mtImage:  result := pausePlayImages;
    mtAudio,
    mtVideo:  result := mpvPausePlay(mpv);
  end;
end;

function TMediaPlayer.pausePlayImages: string;
begin
  mmpDo(evGSImagesPaused, NOT GS.imagesPaused);

  case GS.imagesPaused of  TRUE: result := 'slideshow paused';
                        FALSE: result := 'slideshow unpaused'; end;
end;

function TMediaPlayer.sendOpInfo(const aOpInfo: string): boolean;
begin
  result := FALSE;
  mmpDo(evSTOpInfo, aOpInfo);
  result := TRUE;
end;

procedure TMediaPlayer.setPosition(const aValue: integer);
begin
  mpvSeek(mpv, aValue);
end;

function TMediaPlayer.subscribe(const aSubscriber: ISubscriber): ISubscriber;
begin
  result := FNotifier.subscribe(aSubscriber);
end;

procedure TMediaPlayer.unsubscribe(const aSubscriber: ISubscriber);
begin
  FNotifier.unsubscribe(aSubscriber);
end;

end.
