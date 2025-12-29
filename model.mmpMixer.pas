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
unit model.mmpMixer;

interface

implementation

uses
  winApi.activeX, winApi.MMSystem, winApi.windows,
  system.math, system.sysUtils, system.win.comObj,
  mmpMMDevApi_tlb,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts, mmpFuncProg;

type
  IMixer = interface
    ['{7CB1C821-559E-432E-8B74-DFB8C7742561}']
  end;

  TMixer = class(TInterfacedObject, IMixer)
  private
    function    onNotify(const aNotice: INotice): INotice;
  protected
    function    getMute: boolean;           virtual; abstract;
    function    getVolume: integer;         virtual; abstract;
    procedure   setMute(aValue: boolean);   virtual; abstract;
    procedure   setVolume(aValue: integer); virtual; abstract;
  public
    constructor Create;
    function    notify(const aNotice: INotice): INotice;
    function    setSysVolMax: string;
    property    muted:  boolean read getMute   write setMute;
    property    volume: integer read getVolume write setVolume;
  end;

  TXPMixer = class(TMixer)
  private
    Fmxct:      integer;
    FMixer:     HMIXER;
    procedure   chk(r: MMRESULT);
  protected
    function    getMute: boolean;           override;
    function    getVolume: integer;         override;
    procedure   setMute(aValue: boolean);   override;
    procedure   setVolume(aValue: integer); override;
  public
    constructor Create;
    destructor  Destroy; override;
  end;

  TVistaMixer = class(TMixer)
  private
    FmmDev:       IMMDevice;
    FmmDevEnum:   IMMDeviceEnumerator;
    FmmEndpoint:  IMMAudioEndpointVolume;
  protected
    function    getMute: boolean;           override;
    function    getVolume: integer;         override;
    procedure   setMute(aValue: boolean);   override;
    procedure   setVolume(aValue: integer); override;
  public
    constructor Create;
  end;

function MX: IMixer;
{$J+} const gMixer: IMixer = NIL; {$J-}
var
  verInfo: TOSVersioninfo;
begin
  case gMixer = NIL of TRUE:  begin
                                  verInfo.dwOSVersionInfoSize := sizeOf(TOSVersionInfo);
                                  getVersionEx(verInfo);
                                  case verInfo.dwMajorVersion >= 6 of  TRUE: gMixer := TVistaMixer.create;
                                                                      FALSE: gMixer := TXPMixer.create; end;end;end;
  result := gMixer;
end;

{ TXPMixer }

procedure TXPMixer.chk(r: MMRESULT);
var
  s: string;
begin
  case r = MMSYSERR_NOERROR of TRUE: EXIT; end;
  setLength(s, winApi.MMSystem.MAXERRORLENGTH + 1);
  waveOutGetErrorText(r, @s[1], winApi.MMSystem.MAXERRORLENGTH);
  raise exception.create(strPas(pChar(s)));
end;

constructor TXPMixer.Create;
begin
  inherited;
  Fmxct := MIXERLINE_COMPONENTTYPE_DST_SPEAKERS;
  chk(mixerOpen(@FMixer, 0, 0, 0, 0));
end;

destructor TXPMixer.Destroy;
begin
  case FMixer <> 0 of TRUE: mixerClose(FMixer); end;
  inherited;
end;

function TXPMixer.getMute: boolean;
var
  vMasterMute:  TMixerControl;
  vDetails:     TMixerControlDetails;
  vBoolDetails: TMixerControlDetailsBoolean;
  vLine:        TMixerLine;
  vControls:    TMixerLineControls;
begin
  zeroMemory(@vLine, sizeOf(vLine));
  vLine.cbStruct          := sizeOf(vLine);
  vLine.dwComponentType   := Fmxct;
  chk(mixerGetLineInfo(0, @vLine, MIXER_GETLINEINFOF_COMPONENTTYPE));

  zeroMemory(@vControls, sizeOf(vControls));
  vControls.cbStruct      := sizeOf(vControls);
  vControls.dwLineID      := vLine.dwLineID;
  vControls.cControls     := 1;
  vControls.dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
  vControls.cbmxctrl      := sizeOf(vMasterMute);
  vControls.pamxctrl      := @vMasterMute;
  chk(mixerGetLineControls(0, @vControls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  vDetails.cbStruct       := sizeOf(vDetails);
  vDetails.dwControlID    := vMasterMute.dwControlID;
  vDetails.cChannels      := 1;
  vDetails.cMultipleItems := 0;
  vDetails.cbDetails      := sizeOf(vBoolDetails);
  vDetails.paDetails      := @vBoolDetails;
  chk(mixerGetControlDetails(0, @vDetails, MIXER_GETCONTROLDETAILSF_VALUE));

  result := vBoolDetails.fValue <> 0;
end;

function TXPMixer.getVolume: integer;
var
  vLine:            TMixerLine;
  vControls:        TMixerLineControls;
  vMasterVolume:    TMixerControl;
  vDetails:         TMixerControlDetails;
  vUnsignedDetails: TMixerControlDetailsUnsigned;
begin
  zeroMemory(@vLine, sizeOf(vLine));
  vLine.cbStruct          := sizeOf(vLine);
  vLine.dwComponentType   := Fmxct;
  chk(mixerGetLineInfo(FMixer, @vLine, MIXER_GETLINEINFOF_COMPONENTTYPE));

  zeroMemory(@vControls, sizeOf(vControls));
  vControls.cbStruct      := sizeOf(vControls);
  vControls.dwLineID      := vLine.dwLineID;
  vControls.cControls     := 1;
  vControls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
  vControls.cbmxctrl      := sizeOf(vMasterVolume);
  vControls.pamxctrl      := @vMasterVolume;
  chk(mixerGetLineControls(FMixer, @vControls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  vdetails.cbStruct       := sizeOf(vDetails);
  vdetails.dwControlID    := vMasterVolume.dwControlID;
  vdetails.cChannels      := 1;
  vdetails.cMultipleItems := 0;
  vdetails.cbDetails      := sizeOf(vUnsignedDetails);
  vdetails.paDetails      := @vUnsignedDetails;
  chk(mixerGetControlDetails(FMixer, @vDetails, MIXER_GETCONTROLDETAILSF_VALUE));

  result := vUnsignedDetails.dwValue;
end;

procedure TXPMixer.setMute(aValue: boolean);
var
  vLine:        TMixerLine;
  vControls:    TMixerLineControls;
  vMasterMute:  TMixerControl;
  vDetails:     TMixerControlDetails;
  vBoolDetails: TMixerControlDetailsBoolean;
begin
  zeroMemory(@vLine, sizeOf(vLine));
  vLine.cbStruct          := sizeOf(vLine);
  vLine.dwComponentType   := Fmxct;
  chk(mixerGetLineInfo(FMixer, @vLine, MIXER_GETLINEINFOF_COMPONENTTYPE));

  zeroMemory(@vControls, sizeOf(vControls));
  vControls.cbStruct      := sizeOf(vControls);
  vControls.dwLineID      := vLine.dwLineID;
  vControls.cControls     := 1;
  vControls.dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
  vControls.cbmxctrl      := sizeOf(vMasterMute);
  vControls.pamxctrl      := @vMasterMute;
  chk(mixerGetLineControls(FMixer, @vControls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  vDetails.cbStruct       := sizeOf(vDetails);
  vDetails.dwControlID    := vMasterMute.dwControlID;
  vDetails.cChannels      := 1;
  vDetails.cMultipleItems := 0;
  vDetails.cbDetails      := sizeOf(vBoolDetails);
  vDetails.paDetails      := @vBoolDetails;
  mixerGetControlDetails(0, @vDetails, MIXER_GETCONTROLDETAILSF_VALUE);

  case aValue of   TRUE: vBoolDetails.fValue := 1;
                  FALSE: vBoolDetails.fValue := 0; end;

  chk(mixerSetControlDetails(0, @vDetails, MIXER_SETCONTROLDETAILSF_VALUE));
end;

procedure TXPMixer.setVolume(aValue: integer);
var
  vLine:            TMixerLine;
  vControls:        TMixerLineControls;
  vMasterVolume:    TMixerControl;
  vDetails:         TMixerControlDetails;
  vUnsignedDetails: TMixerControlDetailsUnsigned;
begin
  aValue := max(0, min(65535, aValue));

  zeroMemory(@vLine, sizeOf(vLine));
  vLine.cbStruct          := sizeOf(vLine);
  vLine.dwComponentType   := Fmxct;
  chk(mixerGetLineInfo(FMixer, @vLine, MIXER_GETLINEINFOF_COMPONENTTYPE));

  zeroMemory(@vControls, sizeOf(vControls));
  vControls.cbStruct      := sizeOf(vControls);
  vControls.dwLineID      := vLine.dwLineID;
  vControls.cControls     := 1;
  vControls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
  vControls.cbmxctrl      := sizeOf(vMasterVolume);
  vControls.pamxctrl      := @vMasterVolume;
  chk(mixerGetLineControls(FMixer, @vControls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  vDetails.cbStruct       := sizeOf(vDetails);
  vDetails.dwControlID    := vMasterVolume.dwControlID;
  vDetails.cChannels      := 1;
  vDetails.cMultipleItems := 0;
  vDetails.cbDetails      := sizeOf(vUnsignedDetails);
  vDetails.paDetails      := @vUnsignedDetails;
  vUnsignedDetails.dwValue := aValue;
  chk(mixerSetControlDetails(FMixer, @vDetails, MIXER_SETCONTROLDETAILSF_VALUE));
end;

{ TVistaMixer }

constructor TVistaMixer.Create;
begin
  inherited;
  CoCreateInstance(CLSID_MMDeviceEnumerator, NIL, CLSCTX_ALL, IID_IMMDeviceEnumerator, FmmDevEnum);
  FmmDevEnum.GetDefaultAudioEndpoint(eRender, eMultimedia, FmmDev);
  FmmDev.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, NIL, FmmEndpoint);
end;

function TVistaMixer.getMute: boolean;
var
  vResult: integer;
begin
  FmmEndpoint.getMute(vResult);
  result := boolean(vResult);
end;

function TVistaMixer.getVolume: integer;
var
  vVolLevel: single;
begin
  FmmEndpoint.getMasterVolumeLevelScalar(vVolLevel);
  result := round(vVolLevel * 65535);
end;

procedure TVistaMixer.setMute(aValue: boolean);
begin
  FmmEndpoint.setMute(integer(aValue), NIL);
end;

procedure TVistaMixer.setVolume(aValue: integer);
var
  vValue: single;
begin
  vValue := max(0, min(65535, aValue)) / 65535;
  FmmEndpoint.setMasterVolumeLevelScalar(vValue, NIL);
end;

{ TMixer }

constructor TMixer.Create;
begin
  appEvents.subscribe(newSubscriber(onNotify));
end;

function TMixer.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TMixer.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evMXSysVolMax: setSysVolMax;
  end;
end;

function TMixer.setSysVolMax: string;
begin
  result := EMPTY;
  setMute(FALSE);
  setVolume(65535);
  result := 'Sys Vol Max';
  mmp.cmd(evSTOpInfo, result);   // CHECK THIS. create IMixer each time
end;

initialization
  MX;

end.
