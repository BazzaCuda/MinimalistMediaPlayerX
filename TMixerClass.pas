{   MMP: Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayer

    NB: I didn't write this unit and I don't remember where I got it.
    As a courtesy, I have protected its use with this licence on behalf of the original author.
    The author is welcome to contact me if I have acted in error and they wish this licence to be removed.

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
unit TMixerClass;

interface

type

  TMixer = class
  protected
    function  getMute: boolean; virtual; abstract;
    function  getVolume: integer; virtual; abstract;
    procedure setMute(Value: boolean); virtual; abstract;
    procedure setVolume(Value: integer); virtual; abstract;
  public
    function setSysMaxVol: boolean; virtual; abstract;
    property muted:  boolean read getMute   write setMute;
    property volume: integer read getVolume write setVolume;
  end;

function mmpMixer: TMixer;

implementation

uses
  Windows, MMSystem, mmpMMDevApi_tlb, ComObj, ActiveX, SysUtils;

type

  TxpMixer = class(TMixer)
  private
    Fmxct: integer;
    FMixer: HMIXER;
    procedure chk(r: MMRESULT);
  protected
    function  getMute: boolean; override;
    function  getVolume: integer; override;
    procedure setMute(Value: boolean); override;
    procedure setVolume(Value: integer); override;
    function  setSysMaxVol: boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TVistaMixer = class(TMixer)
  private
    FmmDev: IMMDevice;
    FmmDevEnum: IMMDeviceEnumerator;
    FmmEndpoint: IMMAudioEndpointVolume;
  protected
    function  getMute: boolean; override;
    function  getVolume: integer; override;
    procedure setMute(Value: boolean); override;
    procedure setVolume(Value: integer); override;
    function  setSysMaxVol: boolean; override;
  public
    constructor Create;
  end;

var
  _g_mixer: TMixer;

function mmpMixer: TMixer;
var
  VerInfo: TOSVersioninfo;
begin
  if (_g_mixer = nil) then
  begin
    VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    GetVersionEx(VerInfo);
    if (VerInfo.dwMajorVersion >= 6) then
      _g_mixer := TVistaMixer.Create
    else
      _g_mixer := TxpMixer.Create;
  end;
  result := _g_mixer;
end;

{ TxpMixer }

procedure TxpMixer.chk(r: MMRESULT);
var
  s: string;
begin
  if (r = MMSYSERR_NOERROR) then
    exit;
  setLength(s, MMSystem.MAXERRORLENGTH + 1);
  waveOutGetErrorText(r, @s[1], MMSystem.MAXERRORLENGTH);
  raise Exception.Create(StrPas(pChar(s)));
end;

constructor TxpMixer.Create;
begin
  Fmxct := MIXERLINE_COMPONENTTYPE_DST_SPEAKERS;
  chk(mixerOpen(@Fmixer, 0, 0, 0, 0));
end;

destructor TxpMixer.Destroy;
begin
  if (FMixer <> 0) then
    mixerClose(FMixer);
  inherited;
end;

function TxpMixer.getMute: boolean;
var
  MasterMute: TMixerControl;
  Details: TMixerControlDetails;
  BoolDetails: TMixerControlDetailsBoolean;
  Line: TMixerLine;
  Controls: TMixerLineControls;
begin
  ZeroMemory(@Line, SizeOf(Line));
  Line.cbStruct := SizeOf(Line);
  Line.dwComponentType := Fmxct;
  chk(mixerGetLineInfo(0, @Line, MIXER_GETLINEINFOF_COMPONENTTYPE));

  ZeroMemory(@Controls, SizeOf(Controls));
  Controls.cbStruct := SizeOf(Controls);
  Controls.dwLineID := Line.dwLineID;
  Controls.cControls := 1;
  Controls.dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
  Controls.cbmxctrl := SizeOf(MasterMute);
  Controls.pamxctrl := @MasterMute;
  chk(mixerGetLineControls(0, @Controls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  Details.cbStruct := SizeOf(Details);
  Details.dwControlID := MasterMute.dwControlID;
  Details.cChannels := 1;
  Details.cMultipleItems := 0;
  Details.cbDetails := SizeOf(BoolDetails);
  Details.paDetails := @BoolDetails;
  chk(mixerGetControlDetails(0, @Details, MIXER_GETCONTROLDETAILSF_VALUE));

  result := BoolDetails.fValue <> 0;
end;

function TxpMixer.getVolume: integer;
var
  Line: TMixerLine;
  Controls: TMixerLineControls;
  MasterVolume: TMixerControl;
  Details: TMixerControlDetails;
  UnsignedDetails: TMixerControlDetailsUnsigned;
begin
  ZeroMemory(@Line, SizeOf(Line));
  Line.cbStruct := SizeOf(Line);
  Line.dwComponentType := Fmxct;
  chk(mixerGetLineInfo(Fmixer, @Line, MIXER_GETLINEINFOF_COMPONENTTYPE));
  ZeroMemory(@Controls, SizeOf(Controls));
  Controls.cbStruct := SizeOf(Controls);
  Controls.dwLineID := Line.dwLineID;
  Controls.cControls := 1;
  Controls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
  Controls.cbmxctrl := SizeOf(MasterVolume);
  Controls.pamxctrl := @MasterVolume;
  chk(mixerGetLineControls(Fmixer, @Controls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  details.cbStruct := SizeOf(Details);
  details.dwControlID := MasterVolume.dwControlID;
  details.cChannels := 1;
  details.cMultipleItems := 0;
  details.cbDetails := SizeOf(UnsignedDetails);
  details.paDetails := @UnsignedDetails;
  chk(mixerGetControlDetails(Fmixer, @Details, MIXER_GETCONTROLDETAILSF_VALUE));
  result := UnsignedDetails.dwValue;
end;

procedure TxpMixer.setMute(Value: boolean);
var
  Line: TMixerLine;
  Controls: TMixerLineControls;
  MasterMute: TMixerControl;
  Details: TMixerControlDetails;
  BoolDetails: TMixerControlDetailsBoolean;
begin
  ZeroMemory(@Line, SizeOf(Line));
  Line.cbStruct := SizeOf(Line);
  Line.dwComponentType := Fmxct;
  chk(mixerGetLineInfo(Fmixer, @Line, MIXER_GETLINEINFOF_COMPONENTTYPE));
  ZeroMemory(@Controls, SizeOf(Controls));
  Controls.cbStruct := SizeOf(Controls);
  Controls.dwLineID := Line.dwLineID;
  Controls.cControls := 1;
  Controls.dwControlType := MIXERCONTROL_CONTROLTYPE_MUTE;
  Controls.cbmxctrl := SizeOf(masterMute);
  Controls.pamxctrl := @masterMute;
  chk(mixerGetLineControls(Fmixer, @Controls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  details.cbStruct := SizeOf(Details);
  details.dwControlID := MasterMute.dwControlID;
  details.cChannels := 1;
  details.cMultipleItems := 0;
  details.cbDetails := SizeOf(BoolDetails);
  details.paDetails := @BoolDetails;
  mixerGetControlDetails(0, @Details, MIXER_GETCONTROLDETAILSF_VALUE);
  if (Value) then
    BoolDetails.fValue := 1
  else
    BoolDetails.fValue := 0;

  chk(mixerSetControlDetails(0, @Details, MIXER_SETCONTROLDETAILSF_VALUE));
end;

function TxpMixer.setSysMaxVol: boolean;
begin
  setMute(FALSE);
  setVolume(65535);
end;

procedure TxpMixer.setVolume(Value: integer);
var
  Line: TMixerLine;
  Controls: TMixerLineControls;
  MasterVolume: TMixerControl;
  Details: TMixerControlDetails;
  UnsignedDetails: TMixerControlDetailsUnsigned;
begin
  if (value < 0) then
    value := 0;
  if (value > 65535) then
    value := 65535;

  ZeroMemory(@Line, SizeOf(Line));
  Line.cbStruct := SizeOf(Line);
  Line.dwComponentType := Fmxct;
  chk(mixerGetLineInfo(Fmixer, @Line, MIXER_GETLINEINFOF_COMPONENTTYPE));
  ZeroMemory(@Controls, SizeOf(Controls));
  Controls.cbStruct := SizeOf(Controls);
  Controls.dwLineID := Line.dwLineID;
  Controls.cControls := 1;
  Controls.dwControlType := MIXERCONTROL_CONTROLTYPE_VOLUME;
  Controls.cbmxctrl := SizeOf(MasterVolume);
  Controls.pamxctrl := @MasterVolume;
  chk(mixerGetLineControls(Fmixer, @Controls, MIXER_GETLINECONTROLSF_ONEBYTYPE));

  details.cbStruct := SizeOf(Details);
  details.dwControlID := MasterVolume.dwControlID;
  details.cChannels := 1;
  details.cMultipleItems := 0;
  details.cbDetails := SizeOf(UnsignedDetails);
  details.paDetails := @UnsignedDetails;
  UnsignedDetails.dwValue := Value;
  chk(mixerSetControlDetails(Fmixer, @Details, MIXER_SETCONTROLDETAILSF_VALUE));
end;

{ TVistaMixer }

constructor TVistaMixer.Create;
begin
  CoCreateInstance(CLSID_MMDeviceEnumerator, nil, CLSCTX_ALL, IID_IMMDeviceEnumerator, FmmDevEnum);
  FmmDevEnum.GetDefaultAudioEndpoint(eRender, eMultimedia, FmmDev);
  FmmDev.Activate(IID_IAudioEndpointVolume, CLSCTX_ALL, nil, FmmEndpoint);
end;

function TVistaMixer.getMute: boolean;
var
  vResult: integer;
begin
  FmmEndpoint.GetMute(vResult);
  result := boolean(vResult);
end;

function TVistaMixer.getVolume: integer;
var
  VolLevel: Single;
begin
  FmmEndpoint.GetMasterVolumeLevelScalar(VolLevel);
  result := Round(VolLevel * 65535);
end;

procedure TVistaMixer.setMute(Value: boolean);
begin
  FmmEndpoint.SetMute(integer(Value), nil);
end;

function TVistaMixer.setSysMaxVol: boolean;
begin
  setMute(FALSE);
  setVolume(65535);
end;

procedure TVistaMixer.setVolume(Value: integer);
var
  fValue: Single;
begin
  if (value < 0) then
    value := 0;
  if (value > 65535) then
    value := 65535;
  fValue := Value / 65535;
  FmmEndpoint.SetMasterVolumeLevelScalar(fValue, nil);
end;

initialization
  _g_mixer := NIL;

finalization
  case assigned(_g_mixer) of TRUE: _g_mixer.Free; end;

end.
