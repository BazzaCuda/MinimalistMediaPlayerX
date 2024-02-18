unit MPVBasePlayer;

// MPV base player classes
// Author: Edward G. (nbuyer@gmail.com)

{.$DEFINE MPV_DYNAMIC_LOAD} // should define in project options "Conditional defines"

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  SysUtils, Classes, SyncObjs, Variants,
  MPVConst, MPVClient, MPVNode, MPVTrack;

const
  DEF_MPV_EVENT_SECONDS = 0.5;
  MPV_INVALID_SECOND = -10000000;

type
  TMPVBasePlayer = class;
  TMPVErrorCode = Integer;
  TMPVException = class(Exception);

  TMPVPlayerState = (mpsUnk, mpsLoading, mpsPlay, mpsStep, mpsPause, mpsStop, mpsEnd, mpsErr);

  TMPVEventThread = class(TThread)
  private
    m_cPlayer: TMPVBasePlayer;
  protected
    procedure Execute; override;
  public
    constructor Create(cPlayer: TMPVBasePlayer);
  end;

  TMPVDestroyThread = class(TThread)
  private
    m_hMPV: PMPVHandle;
  protected
    procedure Execute; override;
  public
    constructor Create(hMPV: PMPVHandle);
  end;

  TMPVFileOpen = procedure (cSender: TObject; const sPath: string) of object;
  // pData=PAnsiString/PDouble/PInt32/PInt64 depends on nFmt,
  // use TMPVNode.LoadFromMPVNode() if it is a MPV_FORMAT_NODE
  TMPVPropertyChangedEvent = procedure (cSender: TObject; nID: MPVUInt64;
    nFmt: mpv_format; pData: Pointer) of object;
  TMPVProgressEvent = procedure (cSender: TObject; fCurSec, fTotalSec: Double) of object;
  TMPVErrorMessage = procedure (cSender: TObject; const sPrefix: string;
    nLevel: Int32; const sMsg: string) of object;
  TMPVStateChanged = procedure (cSender: TObject; eState: TMPVPlayerState) of object;

  TMPVBasePlayer = class
  private
    m_cLock: SyncObjs.TCriticalSection; // Lock
    m_cEventThrd: TMPVEventThread; // Thread to process events
    m_fEventWait: Double; // Wait event seconds
    m_eOnFileOpen: TMPVFileOpen;
    m_eOnProgress: TMPVProgressEvent;
    m_eOnPropChged: TMPVPropertyChangedEvent;
    m_eOnErrMsg: TMPVErrorMessage;
    m_eOnStateChged: TMPVStateChanged;
    FOnInitMPV: TNotifyEvent;
  private
    procedure SetATrack(const Value: string);
    procedure SetSTrack(const Value: string);
    procedure SetVTrack(const Value: string);
    procedure SetCurSec(const Value: Double);
    procedure SetSpeed(const Value: Double);
    procedure SetVol(const Value: Double);
    procedure SetVolMute(const Value: Boolean);
    function GetAudioDev: string;
    procedure SetAudioDev(const Value: string);
    function GetAudioDevList: string;

    function GetOnProgress: TMPVProgressEvent;
    procedure SetOnProgress(const Value: TMPVProgressEvent);
    function GetOnProgChg: TMPVPropertyChangedEvent;
    procedure SetOnProgChg(const Value: TMPVPropertyChangedEvent);
    function GetOnErrMsg: TMPVErrorMessage;
    function GetOnFileOpen: TMPVFileOpen;
    procedure SetOnErrMsg(const Value: TMPVErrorMessage);
    procedure SetOnFileOpen(const Value: TMPVFileOpen);
    function GetOnStateChg: TMPVStateChanged;
    procedure SetOnStateChg(const Value: TMPVStateChanged);
    procedure SetState(eNewState: TMPVPlayerState);
  protected
    m_hMPV: PMPVHandle; // MPV Handle

    m_fLenInSec, m_fCurSec: Double; // Total / current seconds   "time-pos"
    m_fLenMax: Double; // Total-0.002, a var to check if reach max
    m_fSpeed: Double; // Speed
    m_fVol: Double; // Volume
    m_bMute: Boolean; // Mute flag
    m_nX, m_nY: Int64; // Video width/height
    m_sAudioDev: string; // name: "auto"
    m_sAudioDevList: string; // JSON array

    m_sFileName: string; // Current file name
    m_eState: TMPVPlayerState; // Current player state

    m_cTrackList: TMPVTrackList; // All tracks' list
    m_sCurVTrk, m_sCurATrk, m_sCurSTrk: string;  // Current track IDs

  protected
    procedure MPVGetMem(var P: Pointer; nSize: Integer); inline;
    procedure MPVFreeMem(P: Pointer); inline;
    function HandleError(nCode: Integer; const sFunc: string;
      bRaise: Boolean=False): TMPVErrorCode;

    procedure FreePlayer; virtual;
    procedure EventLoop(pbCancel: PBoolean); virtual;

    // Override these handlers to handle diff MPV events, see MPV documents
    function DoEventPropertyChange(nID: MPVUInt64;
      pEP: P_mpv_event_property): TMPVErrorCode; virtual;
    function DoEventFileLoaded: TMPVErrorCode; virtual;
    function DoEventLogMsg(pLM: P_mpv_event_log_message): TMPVErrorCode; virtual;
    function DoEventClientMsg(pCM: P_mpv_event_client_message): TMPVErrorCode; virtual;
    function DoEventVideoReconfig: TMPVErrorCode; virtual;
    function DoEventSeek: TMPVErrorCode; virtual;
    function DoEventRestart: TMPVErrorCode; virtual;
    function DoEventStartFile(pSF: P_mpv_event_start_file): TMPVErrorCode; virtual;
    function DoEventEndFile(pEF: P_mpv_event_end_file): TMPVErrorCode; virtual;
    function DoEventShutdown: TMPVErrorCode; virtual;
    function DoEventGetPropertyReply(nErr: MPVInt; nID: MPVUInt64;
      pEP: P_mpv_event_property): TMPVErrorCode; virtual;
    function DoEventSetPropertyReply(nErr: MPVInt;
      nID: MPVUInt64): TMPVErrorCode; virtual;
    function DoEventCommandReply(nErr: MPVInt; nID: MPVUInt64;
      pEC: P_mpv_event_command): TMPVErrorCode; virtual;
    procedure DoSetVideoSize; virtual;
    procedure OnStateChanged;

    function ObserveProperty(const sName: string; nID: UInt64;
      nFmt: MPVEnum = MPV_FORMAT_NODE): TMPVErrorCode;
    function SetTrack(eType: TMPVTrackType; const sID, sPropName: string): TMPVErrorCode;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock; inline;
    procedure Unlock; inline;
    procedure NotifyFree; virtual; // Only notify to free, no wait

    // Initialize player, bind MPV with window handle
    function InitPlayer(const sWinHandle, sScrShotDir, sConfigDir, sLogFile: string;
      fEventWait: Double = DEF_MPV_EVENT_SECONDS): TMPVErrorCode; virtual;
    // Override this to do things before/after MPV init()
    procedure ProcessCmdLine(bBeforeInit: Boolean); virtual;
    // Call your logger when needed
    procedure Log(const sMsg: string; bError: Boolean); virtual;

    // Send command(s) to MPV
    function CommandStr(const sCmd: string): TMPVErrorCode;
    function CommandList(cCmds: TStrings; nID: MPVUInt64 = 0): TMPVErrorCode;
    function Command(const yCmds: array of string; nID: MPVUInt64 = 0): TMPVErrorCode;

    // Get property from MPV
    function GetPropertyBool(const sName: string; var Value: Boolean; bLogError: Boolean = True): TMPVErrorCode;
    function SetPropertyBool(const sName: string; Value: Boolean; nID: MPVUInt64 = 0): TMPVErrorCode;
    function GetPropertyInt64(const sName: string; var Value: Int64; bLogError: Boolean = True): TMPVErrorCode;
    function SetPropertyInt64(const sName: string; Value: Int64; nID: MPVUInt64 = 0): TMPVErrorCode;
    function GetPropertyDouble(const sName: string; var Value: Double; bLogError: Boolean = True): TMPVErrorCode;
    function SetPropertyDouble(const sName: string; Value: Double; nID: MPVUInt64 = 0): TMPVErrorCode;
    function GetPropertyString(const sName: string; var Value: string; bLogError: Boolean = True): TMPVErrorCode;
    function SetPropertyString(const sName, sValue: string; nID: MPVUInt64 = 0): TMPVErrorCode;
    function GetPropertyNode(const sName: string; cNode: TMPVNode; bLogError: Boolean = True): TMPVErrorCode;

    // Observe property, set OnPropertyChanged to handle the change event
    function ObservePropertyBool(const sName: string; nID: UInt64): TMPVErrorCode;
    function ObservePropertyInt64(const sName: string; nID: UInt64): TMPVErrorCode;
    function ObservePropertyDouble(const sName: string; nID: UInt64): TMPVErrorCode;
    function ObservePropertyString(const sName: string; nID: UInt64): TMPVErrorCode;

    function UnobserveProperty(nID: UInt64): TMPVErrorCode;

    // Open file/URL to play
    function OpenFile(const sFullName: string): TMPVErrorCode;
    // Get MPV current state
    function GetState: TMPVPlayerState; inline;
    // Pause video
    function Pause: TMPVErrorCode;
    // Resume play
    function Resume: TMPVErrorCode;
    // Stop playing
    function Stop: TMPVErrorCode;
    // Seek to position(seconds)
    function Seek(fPos: Double; bRelative: Boolean): TMPVErrorCode;

    // Copy tracks' info filtered by eType(trkUnknown for all)
    function CopyTrackInfoList(eType: TMPVTrackType; cList: TMPVTrackList): Integer;
    // Set video track: sID = [id] or [title]
    function SetVideoTrack(const sID: string): TMPVErrorCode;
    // Set audio track: sID = [id] or [title]
    function SetAudioTrack(const sID: string): TMPVErrorCode;
    // Set subtitle: sID = [id] or [title]
    function SetSubTitle(const sIDFile: string; bExternal: Boolean=False): TMPVErrorCode;
    // Remove subtitle file
    function RemoveSubTitle(const sFile: string): TMPVErrorCode;

    // Get/set volume: 0~1000
    function GetVolume: Double;
    function SetVolume(fVol: Double): TMPVErrorCode;
    function SetMute(bMute: Boolean): TMPVErrorCode;
    function GetMute: Boolean;

    // Audio delay, subtitle delay
    function GetAudioDelay: Double;
    procedure SetAudioDelay(fSec: Double);
    function GetSubTitleDelay: Double;
    procedure SetSubTitleDelay(fSec: Double);
  public
    // Player current status/information
    property FileName: string read m_sFileName;
    property CurrentVideoTrack: string read m_sCurVTrk write SetVTrack;
    property CurrentAudioTrack: string read m_sCurATrk write SetATrack;
    property CurrentSubtitle: string read m_sCurSTrk write SetSTrack;
    property PlaybackSpeed: Double read m_fSpeed write SetSpeed;
    property TotalSeconds: Double read m_fLenInSec;
    // Current video progress, you can use a TTimer to display progress
    property CurrentSeconds: Double read m_fCurSec write SetCurSec;
    property VideoWidth: Int64 read m_nX;
    property VideoHeight: Int64 read m_nY;
    property Volume: Double read m_fVol write SetVol;
    property Mute: Boolean read m_bMute write SetVolMute;
    property AudioDevice: string read GetAudioDev write SetAudioDev;
    property AudioDeviceList: string read GetAudioDevList;

    // These events are called from another thread, be sure to use
    // TThread.Synchronize() if you want to update UI.
    property OnFileOpen: TMPVFileOpen read GetOnFileOpen write SetOnFileOpen;
    property OnErrorMessage: TMPVErrorMessage read GetOnErrMsg write SetOnErrMsg;
    property OnProgress: TMPVProgressEvent read GetOnProgress write SetOnProgress;
    property OnPropertyChanged: TMPVPropertyChangedEvent read GetOnProgChg write SetOnProgChg;
    property OnStateChged: TMPVStateChanged read GetOnStateChg write SetOnStateChg;

    property onInitMPV: TNotifyEvent read FOnInitMPV write FOnInitMPV;
  end;

function MPVLibLoaded(const sLibPath: string): Boolean;

implementation

uses _debugWindow;

{ TMPVEventThread }

constructor TMPVEventThread.Create(cPlayer: TMPVBasePlayer);
begin
  m_cPlayer := cPlayer;
  inherited Create(False);
end;

procedure TMPVEventThread.Execute;
begin
  while not Terminated do
  begin
    m_cPlayer.EventLoop(@Terminated);
  end;
end;

{ TMPVDestroyThread }

constructor TMPVDestroyThread.Create(hMPV: PMPVHandle);
begin
  m_hMPV := hMPV;
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TMPVDestroyThread.Execute;
begin
  if m_hMPV<>nil then mpv_terminate_destroy(m_hMPV);
end;

{ TMPVBasePlayer }

function TMPVBasePlayer.Command(const yCmds: array of string; nID: MPVUInt64): TMPVErrorCode;
var
  cStr: TStringList;
  i: Integer;
begin
  cStr := TStringList.Create;
  try
    for i := Low(yCmds) to High(yCmds) do
      cStr.Add(yCmds[i]);
    Result := CommandList(cStr, nID);
  finally
    cStr.Free;
  end;
end;

function TMPVBasePlayer.CommandList(cCmds: TStrings; nID: MPVUInt64): TMPVErrorCode;
var
  args, ppc: PPMPVChar;
  P: Pointer;
  s1: AnsiString;
  i, n: Integer;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  if (cCmds=nil) or (cCmds.Count<=0) then
  begin
    Result := MPV_ERROR_INVALID_PARAMETER;
    Exit;
  end;

  Result := MPV_ERROR_SUCCESS;

  // Construct args
  i := (cCmds.Count+1)*sizeof(PMPVChar);
  args := nil;
  MPVGetMem(Pointer(args), i);
  FillChar(args^, i, 0);
  ppc := args;
  for i := 0 to cCmds.Count-1 do
  begin
    s1 := UTF8Encode(cCmds[i]);
    n := Length(s1)+1; // #0 ended
    if n>1 then
    begin
      // Alloc memory to hold one line
      P := nil;
      MPVGetMem(P, n*sizeof(s1[1]));
      if P<>nil then
      begin
        Move(PAnsiChar(s1)^, P^, n*sizeof(s1[1]));
        ppc^ := P;
        Inc(ppc);
      end else
      begin
        Result := MPV_ERROR_NOMEM;
        Break;
      end;
    end;
  end;

  if Result=MPV_ERROR_SUCCESS then
  begin
    if nID>0 then
      Result := HandleError(mpv_command_async(m_hMPV, nID, args), 'mpv_command_async')
    else
      Result := HandleError(mpv_command(m_hMPV, args), 'mpv_command');
  end;

  // free memory
  ppc := args;
  for i := 0 to cCmds.Count-1 do
  begin
    if ppc<>nil then
      MPVFreeMem(ppc^);
    Inc(ppc);
  end;
  MPVFreeMem(args);
end;

function TMPVBasePlayer.CommandStr(const sCmd: string): TMPVErrorCode;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  Result := HandleError(mpv_command_string(m_hMPV, PMPVChar(UTF8Encode(sCmd))),
    'mpv_command_string');
end;

function TMPVBasePlayer.CopyTrackInfoList(eType: TMPVTrackType;
  cList: TMPVTrackList): Integer;
begin
  m_cLock.Enter;
  cList.Assign(m_cTrackList, eType);
  m_cLock.Leave;
  Result := cList.Count;
end;

constructor TMPVBasePlayer.Create;
begin
  m_cLock := SyncObjs.TCriticalSection.Create;
  m_cTrackList := TMPVTrackList.Create();
  m_eState := mpsUnk;
  inherited Create;
end;

destructor TMPVBasePlayer.Destroy;
begin
  FreePlayer;
  FreeAndNil(m_cTrackList);
  FreeAndNil(m_cLock);
  inherited;
end;

function GetEventStr(nID: MPVEnum): string;
begin
  case nID of
  MPV_EVENT_NONE: Result := 'None';
  MPV_EVENT_SHUTDOWN: Result := 'Shutdown';
  MPV_EVENT_LOG_MESSAGE_: Result := 'LogMsg';
  MPV_EVENT_GET_PROPERTY_REPLY: Result := 'GetPropReply';
  MPV_EVENT_SET_PROPERTY_REPLY: Result := 'SetPropReply';
  MPV_EVENT_COMMAND_REPLY: Result := 'CmdReply';
  MPV_EVENT_START_FILE_: Result := 'StartFile';
  MPV_EVENT_END_FILE_: Result := 'EndFile';
  MPV_EVENT_FILE_LOADED: Result := 'FileLoaded';
  MPV_EVENT_IDLE: Result := 'Idle';
  MPV_EVENT_TICK: Result := 'Tick';
  MPV_EVENT_CLIENT_MESSAGE_: Result := 'ClientMsg';
  MPV_EVENT_VIDEO_RECONFIG: Result := 'VideoRecfg';
  MPV_EVENT_AUDIO_RECONFIG: Result := 'AudioRecfg';
  MPV_EVENT_SEEK: Result := 'Seek';
  MPV_EVENT_PLAYBACK_RESTART: Result := 'PlayRestart';
  MPV_EVENT_PROPERTY_CHANGE: Result := 'PropChg';
  MPV_EVENT_QUEUE_OVERFLOW: Result := 'QueOverflow';
  MPV_EVENT_HOOK_: Result := 'Hook';
  else Result := IntToStr(nID);
  end;
end;

procedure TMPVBasePlayer.EventLoop(pbCancel: PBoolean);
var
  pe: P_mpv_event;
  bChg: Boolean;
begin
  if m_hMPV=nil then Exit;

  while not pbCancel^ do
  begin
    pe := mpv_wait_event(m_hMPV, m_fEventWait);  // seconds
    if pe<>nil then
    begin
      case pe^.event_id of
      MPV_EVENT_NONE: Continue;
      MPV_EVENT_PROPERTY_CHANGE, MPV_EVENT_SEEK, MPV_EVENT_PLAYBACK_RESTART,
      MPV_EVENT_LOG_MESSAGE_: ;  // Too many
      else
        begin
          Log(Format('event=%s; err=%d; ud=%d', [GetEventStr(pe^.event_id),
            pe^.error, pe^.reply_userdata]), False);
        end;
      end;

      case pe^.event_id of
      MPV_EVENT_SEEK:
        begin
          // Update progress!
          HandleError(DoEventSeek, 'DoEventSeek');
        end;
      MPV_EVENT_PROPERTY_CHANGE:
        begin
          HandleError(DoEventPropertyChange(pe^.reply_userdata,
            P_mpv_event_property(pe^.data)), 'DoEventPropertyChange');
        end;
      MPV_EVENT_LOG_MESSAGE_:
        begin
          HandleError(DoEventLogMsg(P_mpv_event_log_message(pe^.data)), 'DoEventLogMsg');
        end;
      MPV_EVENT_VIDEO_RECONFIG:
        begin
          // video changed, resize window!
          HandleError(DoEventVideoReconfig, 'DoEventVideoReconfig');
        end;
      MPV_EVENT_PLAYBACK_RESTART:
        begin
          // Start of playback or after seeking
          HandleError(DoEventRestart, 'DoEventRestart');
        end;
      MPV_EVENT_FILE_LOADED:
        begin
          SetState(mpsPlay);
          // start playback!
          HandleError(DoEventFileLoaded, 'DoEventFileLoaded');
        end;
      MPV_EVENT_START_FILE_:
        begin
          // before load file
          HandleError(DoEventStartFile(P_mpv_event_start_file(pe^.data)), 'DoEventStartFile');
        end;
      MPV_EVENT_END_FILE_:
        begin
          bChg := False;
          m_cLock.Enter;
          case m_eState of
          mpsLoading, // called after open new file?
          mpsEnd: ;
          else
            begin
              bChg := True;
            end;
          end;
          m_cLock.Leave;
          if bChg then SetState(mpsEnd);
          // after file unloaded
          HandleError(DoEventEndFile(P_mpv_event_end_file(pe^.data)), 'DoEventEndFile');
        end;
      MPV_EVENT_SHUTDOWN:
        begin
          SetState(mpsEnd);
          HandleError(DoEventShutdown, 'DoEventShutdown');
        end;
      MPV_EVENT_CLIENT_MESSAGE_:
        begin
          HandleError(DoEventClientMsg(P_mpv_event_client_message(pe^.data)),
            'DoEventClientMsg');
        end;
      MPV_EVENT_GET_PROPERTY_REPLY:
        begin
          HandleError(DoEventGetPropertyReply(pe^.error, pe^.reply_userdata,
            P_mpv_event_property(pe^.data)), 'DoEventGetPropertyReply');
        end;
      MPV_EVENT_SET_PROPERTY_REPLY:
        begin
          HandleError(DoEventSetPropertyReply(pe^.error, pe^.reply_userdata),
            'DoEventSetPropertyReply');
        end;
      MPV_EVENT_COMMAND_REPLY:
        begin
          HandleError(DoEventCommandReply(pe^.error, pe^.reply_userdata,
            P_mpv_event_command(pe^.data)), 'DoEventCommandReply');
        end;
//      MPV_EVENT_AUDIO_RECONFIG, MPV_EVENT_IDLE: ;
//      MPV_EVENT_QUEUE_OVERFLOW: ;
//      MPV_EVENT_HOOK_: ;
      end;
    end;
  end;
end;

function TMPVBasePlayer.DoEventCommandReply(nErr: MPVInt;  nID: MPVUInt64;
  pEC: P_mpv_event_command): TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventEndFile(pEF: P_mpv_event_end_file): TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventFileLoaded: TMPVErrorCode;
var
  eOpen: TMPVFileOpen;
begin
  m_fSpeed := 1.0;
  m_fLenInSec := -1;
  m_fLenMax := 0;
  GetPropertyDouble(STR_DURATION, m_fLenInSec);
  m_sFileName := '';
  GetPropertyString(STR_PATH, m_sFileName);
  // TODO: "://" => get 'media-title'
  GetPropertyInt64(STR_WIDTH, m_nX);
  GetPropertyInt64(STR_HEIGHT, m_nY);
  DoSetVideoSize;

  m_cLock.Enter;
  eOpen := m_eOnFileOpen;
  m_cLock.Leave;
  if Assigned(eOpen) then
  try
    eOpen(Self, m_sFileName);
  except
  end;


  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventGetPropertyReply(nErr: MPVInt; nID: MPVUInt64;
  pEP: P_mpv_event_property): TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventLogMsg(pLM: P_mpv_event_log_message): TMPVErrorCode;
var
  sPF, sLvl, sMsg: string;
  eOnErr: TMPVErrorMessage;
begin
  sLvl := string((pLM^.level)); // UTF8ToString
  if (Length(pLM^.text)>1) and (sLvl<>'warn') then // $0a
  begin
    sPF := string(pLM^.prefix);
    sMsg := string(UTF8ToString(pLM^.text));
    Log(Format('MPV: prefix=%s, loglevel=%d, level=%s, msg=%s', [sPF,
      pLM^.log_level, sLvl, sMsg]), False);
    if (sLvl='error') then
    begin
      m_cLock.Enter;
      eOnErr := m_eOnErrMsg;
      m_cLock.Leave;
      if Assigned(eOnErr) then
      try
        eOnErr(Self, sPF, pLM^.log_level, sMsg);
      except
      end;
    end;
  end;

  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventClientMsg(pCM: P_mpv_event_client_message): TMPVErrorCode;
var
  i: Integer;
  ppc: PPMPVChar;
begin
  if pCM^.num_args>0 then
  begin
    ppc := pCM^.args;
    for i := 0 to pCM^.num_args-1 do
    begin
      Log(Format('MPV clientmsg[%d]=%s', [i, UTF8ToString(ppc^)]), False);
      Inc(ppc);
    end;
  end;
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventPropertyChange(nID: MPVUInt64;
  pEP: P_mpv_event_property): TMPVErrorCode;
var
  s: string;
  cNode: TMPVNode;
  p: Pointer;
  eOnPropChg: TMPVPropertyChangedEvent;
  eOnProg: TMPVProgressEvent;
  eState: TMPVPlayerState;
begin
  Result := MPV_ERROR_SUCCESS;

  case nID of
  ID_PLAY_TIME:
    begin
      m_cLock.Enter;
      case pEP^.format of
      MPV_FORMAT_DOUBLE:
        begin
          m_fCurSec := PDouble(pEP^.data)^;
        end;
      MPV_FORMAT_NONE:
        begin
          m_fCurSec := 0;
        end;
      end;

      eState := m_eState;
      if (eState=mpsPlay) then
      begin
//        debugFormat('%f of %f', [m_fCurSec, m_fLenMax]);
        if (m_fCurSec>=m_fLenMax {- 0.25}) then
        begin
          // MPV does not notify EOF before unloading file
          // although reached the end, but player is still "playing"
          eState := mpsEnd;
        end;
      end else
      begin
        // playtime changed, not end pos, set to playing state if was ended
        case eState of
        mpsEnd:
          begin
            if (m_fCurSec<m_fLenMax-0.1) then eState := mpsPlay;
          end;
        end;
      end;
      eOnProg := m_eOnProgress;
      m_cLock.Leave;

      if Assigned(eOnProg) then
      try
        eOnProg(Self, m_fCurSec, m_fLenInSec);
      except
      end;

      SetState(eState);
    end;
  ID_PAUSE:
    begin
      m_cLock.Enter;
      eState := m_eState;
      m_cLock.Leave;
      if PMPVFlag(pEP^.data)^=0 then
      begin
        if eState=mpsPause then eState := mpsPlay; // change only when paused
      end else
      begin
        eState := mpsPause;
      end;
      SetState(eState);
    end;
  ID_VOLUME:
    begin
      m_cLock.Enter;
      case pEP^.format of
      MPV_FORMAT_DOUBLE:
        m_fVol := PDouble(pEP^.data)^; // Normally 100
      MPV_FORMAT_NONE:
        m_fVol := 0;
      end;
      m_cLock.Leave;
    end;
  ID_DURATION:
    begin
      m_cLock.Enter;
      case pEP^.format of
      MPV_FORMAT_DOUBLE:
        begin
          m_fLenInSec := PDouble(pEP^.data)^;
          m_fLenMax := m_fLenInSec-0.05;
        end;
      MPV_FORMAT_NONE:
        begin
          m_fLenInSec := 0;
          m_fLenMax := 0;
        end;
      end;
      m_cLock.Leave;
   end;
  ID_MUTE:
    begin
      m_cLock.Enter;
      if PMPVFlag(pEP^.data)^=0 then
      begin
        m_bMute := False;
      end else
        m_bMute := True;
      m_cLock.Leave;
    end;
  ID_SID:
    begin
      s := VarToStr(GetMPVPropertyValue(pEP^.format, pEP^.data));
      m_cLock.Enter;
      m_sCurSTrk := s;
      m_cLock.Leave;
    end;
  ID_AID:
    begin
      s := VarToStr(GetMPVPropertyValue(pEP^.format, pEP^.data));
      m_cLock.Enter;
      m_sCurATrk := s;
      m_cLock.Leave;
    end;
  ID_VID:
    begin
      s := VarToStr(GetMPVPropertyValue(pEP^.format, pEP^.data));
      m_cLock.Enter;
      m_sCurVTrk := s;
      m_cLock.Leave;
    end;
  ID_SPEED:
    begin
      m_cLock.Enter;
      case pEP^.format of
      MPV_FORMAT_DOUBLE:
        m_fSpeed := PDouble(pEP^.data)^;
      MPV_FORMAT_NONE:
        m_fSpeed := 0;
      end;
      m_cLock.Leave;
    end;
  ID_TRACK_LIST:
    begin
      if pEP^.format=MPV_FORMAT_NODE then
      begin
        cNode := TMPVNode.Create;
        cNode.LoadFromMPVNode(pEP^.data, False);
        //cNode.SaveToFile('.\trklist.txt');
        m_cLock.Enter;
        try
          m_cTrackList.LoadFromNode(cNode);
        except
        end;
        m_cLock.Leave;
        cNode.Free;
      end;
    end;
  ID_AUDIO_DEV:
    begin
      m_cLock.Enter;
      m_sAudioDev := VarToStr(GetMPVPropertyValue(pEP^.format, pEP^.data));
      m_cLock.Leave;
    end;
  ID_AUDIO_DEV_LIST:
    begin
      m_cLock.Enter;
      m_sAudioDevList := VarToStr(GetMPVPropertyValue(pEP^.format, pEP^.data));
      m_cLock.Leave;
      //'[{"name":"auto","description":"Autoselect device"},
      //{"name":"wasapi/{g-u-i-d-e9a4855ad585}",
      // "description":"Speakers (Realtek High Definition Audio(SST))"}]'
    end;
  end;

  m_cLock.Enter;
  eOnPropChg := m_eOnPropChged;
  m_cLock.Leave;
  if Assigned(eOnPropChg) then
  begin
    p := pEP^.data;
    if pEP^.format=MPV_FORMAT_STRING then
    begin
      s := string(UTF8ToString(PPMPVChar(pEP^.data)^));
      p := PAnsiChar(AnsiString(s));
    end;

    try
      eOnPropChg(Self, nID, pEP^.format, p);
    except
    end;
  end;
end;

function TMPVBasePlayer.DoEventRestart: TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventSeek: TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventSetPropertyReply(nErr: MPVInt;
  nID: MPVUInt64): TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventShutdown: TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventStartFile(pSF: P_mpv_event_start_file): TMPVErrorCode;
begin
  Result := MPV_ERROR_SUCCESS;
end;

function TMPVBasePlayer.DoEventVideoReconfig: TMPVErrorCode;
begin
  // Could be error at the beginning
  if GetPropertyInt64(STR_DWIDTH, m_nX, False)=MPV_ERROR_SUCCESS then
  begin
    if GetPropertyInt64(STR_DHEIGHT, m_nY)=MPV_ERROR_SUCCESS then
      DoSetVideoSize;
  end;
  Result := MPV_ERROR_SUCCESS;
end;

procedure TMPVBasePlayer.DoSetVideoSize;
begin
  // NULL
end;

procedure TMPVBasePlayer.FreePlayer;
begin
  if m_cEventThrd<>nil then
  begin
    m_cEventThrd.Terminate;
    m_cEventThrd.WaitFor; // may block if call after mpv_destroy()
    FreeAndNil(m_cEventThrd);
  end;
  if m_hMPV<>nil then
  begin
    // This call might cause very long time when debugging in Delphi,
    // but pretty fast when running alone.
    mpv_terminate_destroy(m_hMPV); //mpv_destroy(m_hMPV);
    //TMPVDestroyThread.Create(m_hMPV);
    m_hMPV := nil;
  end;
end;

function TMPVBasePlayer.GetAudioDelay: Double;
var
  dbl: Double;
begin
  dbl := 0;
  if GetPropertyDouble(STR_AUDIO_DELAY, dbl)=MPV_ERROR_SUCCESS then
    Result := dbl
  else
    Result := MPV_INVALID_SECOND;
end;

function TMPVBasePlayer.GetAudioDev: string;
begin
  m_cLock.Enter;
  Result := m_sAudioDev;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetAudioDevList: string;
begin
  m_cLock.Enter;
  Result := m_sAudioDevList;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetMute: Boolean;
begin
  m_cLock.Enter;
  Result := m_bMute;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetOnErrMsg: TMPVErrorMessage;
begin
  m_cLock.Enter;
  Result := m_eOnErrMsg;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetOnFileOpen: TMPVFileOpen;
begin
  m_cLock.Enter;
  Result := m_eOnFileOpen;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetOnProgChg: TMPVPropertyChangedEvent;
begin
  m_cLock.Enter;
  Result := m_eOnPropChged;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetOnProgress: TMPVProgressEvent;
begin
  m_cLock.Enter;
  Result := m_eOnProgress;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetOnStateChg: TMPVStateChanged;
begin
  m_cLock.Enter;
  Result := m_eOnStateChged;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetPropertyBool(const sName: string;
  var Value: Boolean; bLogError: Boolean): TMPVErrorCode;
var
  sNm: UTF8String;
  n: MPVInt;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  n := 0;
  Result := mpv_get_property(m_hMPV, PMPVChar(sNm),
    MPV_FORMAT_FLAG, @n);
  if Result<>MPV_ERROR_SUCCESS then
  begin
    if bLogError then
      HandleError(Result, 'mpv_get_property(bool):'+sName);
  end else
  begin
    Value := n<>0;
  end;
end;

function TMPVBasePlayer.GetPropertyDouble(const sName: string;
  var Value: Double; bLogError: Boolean): TMPVErrorCode;
var
  sNm: UTF8String;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  Result := mpv_get_property(m_hMPV, PMPVChar(sNm),
    MPV_FORMAT_DOUBLE, @Value);
  if Result<>MPV_ERROR_SUCCESS then
  begin
    if bLogError then
      HandleError(Result, 'mpv_get_property(dbl):'+sName);
  end;
end;

function TMPVBasePlayer.GetPropertyInt64(const sName: string;
  var Value: Int64; bLogError: Boolean): TMPVErrorCode;
var
  sNm: UTF8String;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  Result := mpv_get_property(m_hMPV, PMPVChar(sNm),
    MPV_FORMAT_INT64, @Value);
  if Result<>MPV_ERROR_SUCCESS then
  begin
    if bLogError then
      HandleError(Result, 'mpv_get_property(i64):'+sName);
  end;
end;

function TMPVBasePlayer.GetPropertyNode(const sName: string;
  cNode: TMPVNode; bLogError: Boolean): TMPVErrorCode;
var
  sNm: UTF8String;
  P: P_mpv_node;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  P := nil;
  Result := mpv_get_property(m_hMPV, PMPVChar(sNm),
    MPV_FORMAT_NODE, @P);
  if Result<>MPV_ERROR_SUCCESS then
  begin
    if bLogError then
      HandleError(Result, 'mpv_get_property(node):'+sName);
  end;
  if P<>nil then
  begin
    // Get value and free
    cNode.LoadFromMPVNode(P, True);
  end;
end;

function TMPVBasePlayer.GetPropertyString(const sName: string;
  var Value: string; bLogError: Boolean): TMPVErrorCode;
var
  sNm: UTF8String;
  P: PMPVChar;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  P := nil;
  Result := mpv_get_property(m_hMPV, PMPVChar(sNm),
    MPV_FORMAT_STRING, @P);
  if Result<>MPV_ERROR_SUCCESS then
  begin
    if bLogError then
      HandleError(Result, 'mpv_get_property(str):'+sName);
  end;
  if P<>nil then
  begin
    // Get value and free
    Value := string(UTF8ToString(P));
    mpv_free(P);
  end;
end;

function TMPVBasePlayer.GetState: TMPVPlayerState;
begin
  m_cLock.Enter;
  Result := m_eState;
  m_cLock.Leave;
end;

function TMPVBasePlayer.GetSubTitleDelay: Double;
var
  dbl: Double;
begin
  dbl := 0;
  if GetPropertyDouble(STR_SUB_DELAY, dbl)=MPV_ERROR_SUCCESS then
    Result := dbl
  else
    Result := MPV_INVALID_SECOND;
end;

function TMPVBasePlayer.GetVolume: Double;
var
  dbl: Double;
begin
  dbl := 0;
  if GetPropertyDouble(STR_VOLUME, dbl)=MPV_ERROR_SUCCESS then
    Result := dbl
  else
    Result := -1;
end;

function TMPVBasePlayer.Seek(fPos: Double; bRelative: Boolean): TMPVErrorCode;
var
  sAbs: string;
begin
  if bRelative then sAbs := 'relative' else sAbs := 'absolute';
  Result := Command([CMD_SEEK, FloatToStr(fPos), sAbs]);
  // update current position
  GetPropertyDouble(STR_TIME_POS, m_fCurSec, False);
end;

function TMPVBasePlayer.InitPlayer(const sWinHandle, sScrShotDir, sConfigDir, sLogFile: string;
  fEventWait: Double): TMPVErrorCode;
begin
  if not MPVLibLoaded('') then
  begin
    Result := MPV_ERROR_LOADING_FAILED;
    Exit;
  end;

  FreePlayer();

  // Basic procedure copied from MPV.NET
  m_hMPV := mpv_create();
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_NOMEM;
    Exit;
  end;

  mpv_request_log_messages(m_hMPV, 'terminal-default');

{$IFDEF CONSOLE}
  SetPropertyString('terminal', 'yes');
  SetPropertyString('input-terminal', 'yes');
  SetPropertyString('msg-level', 'osd/libass=fatal');
{$ENDIF}
  //===== THESE CAN ALL BE OVERRIDDEN IN MPV.CONF =====

  //SetPropertyString('watch-later-options', STR_MUTE+','+STR_SID+','+STR_AID);

//  SetPropertyString('osd-playing-msg', '${filename}');
  if sLogFile<>'' then SetPropertyString(STR_LOG_FILE, sLogFile);
  SetPropertyString(STR_WID, sWinHandle);

  case assigned(FOnInitMPV) of TRUE: FOnInitMPV(SELF); end;

//  SetPropertyBool('no-config', TRUE);
//  SetPropertyBool('idle-screen', FALSE);
//  setPropertyString('osc-visibility', 'never');
//  setPropertyString('audio-device', 'wasapi/{1881c69e-d424-40b8-bb62-9f2aa3f36e6a}');
//  SetPropertyString('video-unscaled', 'downscale-big'); // this option disables pan-scan

//  SetPropertyBool('input-default-bindings', True);
//  SetPropertyBool('input-builtin-bindings', False);
//  SetPropertyString('reset-on-next-file', 'brightness,speed,video-aspect-override,af,sub-visibility,audio-delay,pause');

  ProcessCmdLine(True);
  Result := HandleError(mpv_initialize(m_hMPV), 'mpv_initialize'); // all property values set prior to this can be overridden in mpv.conf
  if Result <> MPV_ERROR_SUCCESS then Exit;

  m_fEventWait := fEventWait;
  m_cEventThrd := TMPVEventThread.Create(Self);

  ObservePropertyBool(STR_PAUSE, ID_PAUSE);
  ObservePropertyBool(STR_MUTE, ID_MUTE);
  ObservePropertyInt64(STR_SID, ID_SID);
  ObservePropertyInt64(STR_AID, ID_AID);
  ObservePropertyInt64(STR_VID, ID_VID);
  ObservePropertyDouble(STR_DURATION, ID_DURATION);
  ObservePropertyDouble(STR_PLAY_TIME, ID_PLAY_TIME);
  ObservePropertyDouble(STR_SPEED, ID_SPEED);
  ObservePropertyDouble(STR_VOLUME, ID_VOLUME);
  ObserveProperty(STR_TRACK_LIST, ID_TRACK_LIST); // Node
  ObservePropertyString(STR_AUDIO_DEV, ID_AUDIO_DEV);
  //ObservePropertyString(STR_AUDIO_DEV_LIST, ID_AUDIO_DEV_LIST); // May cause unknown error

//  ObservePropertyBool(STR_WIN_MAX, ID_WIN_MAX);
//  ObservePropertyBool(STR_WIN_MIN, ID_WIN_MIN);
//  ObservePropertyBool(STR_FULL_SCREEN, ID_FULL_SCREEN);
//  ObservePropertyString(STR_FS_SCREEN_NAME, ID_FS_SCREEN_NAME);
//  ObservePropertyBool(STR_ONTOP, ID_ONTOP);
//  ObservePropertyInt64(STR_ONTOP_LEVEL, ID_ONTOP_LEVEL);
//  ObservePropertyDouble(STR_WIN_SCALE, ID_WIN_SCALE);
  ProcessCmdLine(False);
end;

procedure TMPVBasePlayer.Lock;
begin
  m_cLock.Enter;
end;

procedure TMPVBasePlayer.Log(const sMsg: string; bError: Boolean);
begin
  // NULL
end;

procedure TMPVBasePlayer.MPVFreeMem(P: Pointer);
begin
  FreeMem(P);
end;

procedure TMPVBasePlayer.MPVGetMem(var P: Pointer; nSize: Integer);
begin
  GetMem(P, nSize);
end;

procedure TMPVBasePlayer.NotifyFree;
begin
  if m_cEventThrd<>nil then m_cEventThrd.Terminate;
  m_cLock.Enter;
  m_eOnFileOpen := nil;
  m_eOnProgress := nil;
  m_eOnPropChged := nil;
  m_eOnErrMsg := nil;
  m_eOnStateChged := nil;
  m_cLock.Leave;
end;

function TMPVBasePlayer.ObserveProperty(const sName: string;
  nID: UInt64; nFmt: MPVEnum): TMPVErrorCode;
var
  sNm: UTF8String;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  Result := HandleError(mpv_observe_property(m_hMPV, nID, PMPVChar(sNm), nFmt),
    'mpv_observe_property');
end;

function TMPVBasePlayer.ObservePropertyBool(const sName: string;
  nID: UInt64): TMPVErrorCode;
begin
  Result := ObserveProperty(sName, nID, MPV_FORMAT_FLAG);
end;

function TMPVBasePlayer.ObservePropertyDouble(const sName: string;
  nID: UInt64): TMPVErrorCode;
begin
  Result := ObserveProperty(sName, nID, MPV_FORMAT_DOUBLE);
end;

function TMPVBasePlayer.ObservePropertyInt64(const sName: string;
  nID: UInt64): TMPVErrorCode;
begin
  Result := ObserveProperty(sName, nID, MPV_FORMAT_INT64);
end;

function TMPVBasePlayer.ObservePropertyString(const sName: string;
  nID: UInt64): TMPVErrorCode;
begin
  Result := ObserveProperty(sName, nID, MPV_FORMAT_STRING);
end;

procedure TMPVBasePlayer.OnStateChanged;
var
  eSC: TMPVStateChanged;
  eState: TMPVPlayerState;
begin
  m_cLock.Enter;
  eSC := m_eOnStateChged;
  eState := m_eState;
  // update current position
  GetPropertyDouble(STR_TIME_POS, m_fCurSec, False);
  m_cLock.Leave;
  if Assigned(eSC) then
  try
    eSC(Self, eState);
  except
  end;
end;

function TMPVBasePlayer.OpenFile(const sFullName: string): TMPVErrorCode;
begin
  SetState(mpsLoading);
  Result := Command([CMD_LOAD_FILE, sFullName]);
  SetPropertyBool(STR_PAUSE, False);
end;

function TMPVBasePlayer.Pause: TMPVErrorCode;
begin
  Result := SetPropertyBool(STR_PAUSE, True);
end;

procedure TMPVBasePlayer.ProcessCmdLine(bBeforeInit: Boolean);
begin
  // NULL
end;

function TMPVBasePlayer.RemoveSubTitle(const sFile: string): TMPVErrorCode;
begin
  Result := CommandStr(CMD_SUB_REMOVE+' '+sFile);
end;

function TMPVBasePlayer.Resume: TMPVErrorCode;
begin
  Result := SetPropertyBool(STR_PAUSE, False);
end;

function TMPVBasePlayer.HandleError(nCode: Integer; const sFunc: string;
  bRaise: Boolean): TMPVErrorCode;
var
  sErr: string;
begin
  Result := nCode;
  if nCode = MPV_ERROR_SUCCESS then Exit;

  sErr := Format('%s=%d', [sFunc, nCode]);
  Log(sErr, True);
  if bRaise then
  begin
    raise TMPVException.Create(sErr);
  end;
end;

procedure TMPVBasePlayer.SetATrack(const Value: string);
begin
  SetAudioTrack(Value);
end;

procedure TMPVBasePlayer.SetAudioDelay(fSec: Double);
begin
  SetPropertyDouble(STR_AUDIO_DELAY, fSec);
end;

procedure TMPVBasePlayer.SetAudioDev(const Value: string);
begin
  SetPropertyString(STR_AUDIO_DEV, Value);
end;

function TMPVBasePlayer.SetAudioTrack(const sID: string): TMPVErrorCode;
begin
  Result := SetTrack(trkAudio, sID, STR_AID);
end;

procedure TMPVBasePlayer.SetCurSec(const Value: Double);
begin
  Seek(Value, False);
end;

function TMPVBasePlayer.SetMute(bMute: Boolean): TMPVErrorCode;
begin
  Result := SetPropertyBool(STR_MUTE, bMute);
end;

procedure TMPVBasePlayer.SetOnErrMsg(const Value: TMPVErrorMessage);
begin
  m_cLock.Enter;
  m_eOnErrMsg := Value;
  m_cLock.Leave;
end;

procedure TMPVBasePlayer.SetOnFileOpen(const Value: TMPVFileOpen);
begin
  m_cLock.Enter;
  m_eOnFileOpen := Value;
  m_cLock.Leave;
end;

procedure TMPVBasePlayer.SetOnProgChg(const Value: TMPVPropertyChangedEvent);
begin
  m_cLock.Enter;
  m_eOnPropChged := Value;
  m_cLock.Leave;
end;

procedure TMPVBasePlayer.SetOnProgress(const Value: TMPVProgressEvent);
begin
  m_cLock.Enter;
  m_eOnProgress := Value;
  m_cLock.Leave;
end;

procedure TMPVBasePlayer.SetOnStateChg(const Value: TMPVStateChanged);
begin
  m_cLock.Enter;
  m_eOnStateChged := Value;
  m_cLock.Leave;
end;

function TMPVBasePlayer.SetPropertyBool(const sName: string;
  Value: Boolean; nID: MPVUInt64): TMPVErrorCode;
var
  sNm: UTF8String;
  n: MPVInt;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  if Value then n := 1 else n := 0;

  if nID>0 then
    Result := HandleError(mpv_set_property_async(m_hMPV, nID, PMPVChar(sNm),
      MPV_FORMAT_FLAG, @n), 'mpv_set_property_async(bool)')
  else
    Result := HandleError(mpv_set_property(m_hMPV, PMPVChar(sNm),
      MPV_FORMAT_FLAG, @n), 'mpv_set_property(bool)');
end;

function TMPVBasePlayer.SetPropertyDouble(const sName: string;
  Value: Double; nID: MPVUInt64): TMPVErrorCode;
var
  sNm: UTF8String;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);

  if nID>0 then
    Result := HandleError(mpv_set_property_async(m_hMPV, nID, PMPVChar(sNm),
      MPV_FORMAT_DOUBLE, @Value), 'mpv_set_property_async(dbl)')
  else
    Result := HandleError(mpv_set_property(m_hMPV, PMPVChar(sNm),
      MPV_FORMAT_DOUBLE, @Value), 'mpv_set_property(dbl)');
end;

function TMPVBasePlayer.SetPropertyInt64(const sName: string;
  Value: Int64; nID: MPVUInt64): TMPVErrorCode;
var
  sNm: UTF8String;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);

  if nID>0 then
    Result := HandleError(mpv_set_property_async(m_hMPV, nID, PMPVChar(sNm),
      MPV_FORMAT_INT64, @Value), 'mpv_set_property_async(i64)')
  else
    Result := HandleError(mpv_set_property(m_hMPV, PMPVChar(sNm),
      MPV_FORMAT_INT64, @Value), 'mpv_set_property(i64)');
end;

function TMPVBasePlayer.SetPropertyString(const sName,
  sValue: string; nID: MPVUInt64): TMPVErrorCode;
var
  sNm, sVal: UTF8String;
  p: Pointer;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  sNm := UTF8Encode(sName);
  sVal := UTF8Encode(sValue);
  p := Pointer(sVal); // address of PAnsiChar

  if nID>0 then
    Result := HandleError(mpv_set_property_async(m_hMPV, nID, PMPVChar(sNm),
      MPV_FORMAT_STRING, @p), 'mpv_set_property_async(str)')
 else
    Result := HandleError(mpv_set_property(m_hMPV, PMPVChar(sNm),
      MPV_FORMAT_STRING, @p), 'mpv_set_property(str)');
end;

procedure TMPVBasePlayer.SetSpeed(const Value: Double);
begin
  if (Value>0) and (Value<=100) then
  begin
    SetPropertyDouble(STR_SPEED, Value);
  end;
end;

procedure TMPVBasePlayer.SetState(eNewState: TMPVPlayerState);
var
  bChg: Boolean;
begin
  bChg := False;
  m_cLock.Enter;
  if m_eState<>eNewState then
  begin
    m_eState := eNewState;
    bChg := True;
  end;
  m_cLock.Leave;
  if bChg then OnStateChanged();
end;

procedure TMPVBasePlayer.SetSTrack(const Value: string);
begin
  SetSubTitle(Value);
end;

function TMPVBasePlayer.SetSubTitle(const sIDFile: string; bExternal: Boolean): TMPVErrorCode;
begin
  if bExternal then
  begin
    if FileExists(sIDFile) then
    begin
      Result := CommandStr(CMD_SUB_ADD+' '+sIDFile);
    end else
      Result := MPV_ERROR_INVALID_PARAMETER;
  end else
    Result := SetTrack(trkSub, sIDFile, STR_SID);
end;

procedure TMPVBasePlayer.SetSubTitleDelay(fSec: Double);
begin
  SetPropertyDouble(STR_SUB_DELAY, fSec);
end;

function TMPVBasePlayer.SetTrack(eType: TMPVTrackType;
  const sID, sPropName: string): TMPVErrorCode;
var
  m: Integer;
  i: Integer;
  n: Int64;
  cTrk: TMPVTrackInfo;
begin
  n := -1;
  m := StrToIntDef(sID, -2);
  // find title/id
  m_cLock.Enter;
  for i := 0 to m_cTrackList.Count-1 do
  begin
    cTrk := m_cTrackList[i];
    if cTrk.TrackType=eType then
    begin
      if (cTrk.Title=sID) or (cTrk.TrackID=m) then
      begin
        n := cTrk.TrackID;
        Break;
      end;
    end;
  end;
  m_cLock.Leave;
  if n>=0 then
  begin
    Result := SetPropertyInt64(sPropName, n);
  end else
    Result := SetPropertyString(sPropName, sID); // MPV_ERROR_INVALID_PARAMETER
end;

function TMPVBasePlayer.SetVideoTrack(const sID: string): TMPVErrorCode;
begin
  Result := SetTrack(trkVideo, sID, STR_VID);
end;

procedure TMPVBasePlayer.SetVol(const Value: Double);
begin
  if (Value>0) and (Value<=1000) then
  begin
    SetPropertyDouble(STR_VOLUME, Value);
  end;
end;

procedure TMPVBasePlayer.SetVolMute(const Value: Boolean);
begin
  SetMute(Value);
end;

function TMPVBasePlayer.SetVolume(fVol: Double): TMPVErrorCode;
begin
  if m_bMute and (fVol>0) then SetMute(False);
  Result := SetPropertyDouble(STR_VOLUME, fVol);
end;

procedure TMPVBasePlayer.SetVTrack(const Value: string);
begin
  SetVideoTrack(Value);
end;

function TMPVBasePlayer.Stop: TMPVErrorCode;
begin
  Result := CommandStr(CMD_STOP);
end;

procedure TMPVBasePlayer.Unlock;
begin
  m_cLock.Leave;
end;

function TMPVBasePlayer.UnobserveProperty(nID: UInt64): TMPVErrorCode;
begin
  if m_hMPV=nil then
  begin
    Result := MPV_ERROR_UNINITIALIZED;
    Exit;
  end;
  Result := HandleError(mpv_unobserve_property(m_hMPV, nID),
    'mpv_unobserve_property');
end;

{$IFDEF MPV_DYNAMIC_LOAD}
var
  g_hMPVLib: HMODULE = 0;

procedure MPVLibFree;
begin
  if g_hMPVLib<>0 then
  begin
    FreeLibrary(g_hMPVLib);
    g_hMPVLib := 0;
  end;
end;

function MPVLibLoaded(const sLibPath: string): Boolean;
var
  sLib: string;
begin
  Result := Assigned(mpv_client_api_version);
  if not Result then
  begin
    if sLibPath='' then sLib := ExtractFilePath(ParamStr(0))+MPVDLL
      else sLib := IncludeTrailingPathDelimiter(sLibPath)+MPVDLL;
    MPVLibFree;
    g_hMPVLib := SysUtils.SafeLoadLibrary(sLib);
    if g_hMPVLib<>0 then
    begin
      mpv_client_api_version := T_mpv_client_api_version(GetProcAddress(g_hMPVLib, fn_mpv_client_api_version));
      mpv_error_string := T_mpv_error_string(GetProcAddress(g_hMPVLib, fn_mpv_error_string));
      mpv_free := T_mpv_free(GetProcAddress(g_hMPVLib, fn_mpv_free));
      mpv_client_name := T_mpv_client_name(GetProcAddress(g_hMPVLib, fn_mpv_client_name));
      mpv_client_id := T_mpv_client_id(GetProcAddress(g_hMPVLib, fn_mpv_client_id));
      mpv_create := T_mpv_create(GetProcAddress(g_hMPVLib, fn_mpv_create));
      mpv_initialize := T_mpv_initialize(GetProcAddress(g_hMPVLib, fn_mpv_initialize));
      mpv_destroy := T_mpv_destroy(GetProcAddress(g_hMPVLib, fn_mpv_destroy));
      mpv_terminate_destroy := T_mpv_terminate_destroy(GetProcAddress(g_hMPVLib, fn_mpv_terminate_destroy));
      mpv_create_client := T_mpv_create_client(GetProcAddress(g_hMPVLib, fn_mpv_create_client));
      mpv_create_weak_client := T_mpv_create_weak_client(GetProcAddress(g_hMPVLib, fn_mpv_create_weak_client));
      mpv_load_config_file := T_mpv_load_config_file(GetProcAddress(g_hMPVLib, fn_mpv_load_config_file));
      mpv_get_time_us := T_mpv_get_time_us(GetProcAddress(g_hMPVLib, fn_mpv_get_time_us));
      mpv_free_node_contents := T_mpv_free_node_contents(GetProcAddress(g_hMPVLib, fn_mpv_free_node_contents));
      mpv_set_option := T_mpv_set_option(GetProcAddress(g_hMPVLib, fn_mpv_set_option));
      mpv_set_option_string := T_mpv_set_option_string(GetProcAddress(g_hMPVLib, fn_mpv_set_option_string));
      mpv_command := T_mpv_command(GetProcAddress(g_hMPVLib, fn_mpv_command));
      mpv_command_node := T_mpv_command_node(GetProcAddress(g_hMPVLib, fn_mpv_command_node));
      mpv_command_ret := T_mpv_command_ret(GetProcAddress(g_hMPVLib, fn_mpv_command_ret));
      mpv_command_string := T_mpv_command_string(GetProcAddress(g_hMPVLib, fn_mpv_command_string));
      mpv_command_async := T_mpv_command_async(GetProcAddress(g_hMPVLib, fn_mpv_command_async));
      mpv_command_node_async := T_mpv_command_node_async(GetProcAddress(g_hMPVLib, fn_mpv_command_node_async));
      mpv_abort_async_command := T_mpv_abort_async_command(GetProcAddress(g_hMPVLib, fn_mpv_abort_async_command));
      mpv_set_property := T_mpv_set_property(GetProcAddress(g_hMPVLib, fn_mpv_set_property));
      mpv_set_property_string := T_mpv_set_property_string(GetProcAddress(g_hMPVLib, fn_mpv_set_property_string));
      mpv_set_property_async := T_mpv_set_property_async(GetProcAddress(g_hMPVLib, fn_mpv_set_property_async));
      mpv_get_property := T_mpv_get_property(GetProcAddress(g_hMPVLib, fn_mpv_get_property));
      mpv_get_property_string := T_mpv_get_property_string(GetProcAddress(g_hMPVLib, fn_mpv_get_property_string));
      mpv_get_property_osd_string := T_mpv_get_property_osd_string(GetProcAddress(g_hMPVLib, fn_mpv_get_property_osd_string));
      mpv_get_property_async := T_mpv_get_property_async(GetProcAddress(g_hMPVLib, fn_mpv_get_property_async));
      mpv_observe_property := T_mpv_observe_property(GetProcAddress(g_hMPVLib, fn_mpv_observe_property));
      mpv_unobserve_property := T_mpv_unobserve_property(GetProcAddress(g_hMPVLib, fn_mpv_unobserve_property));
      mpv_event_name := T_mpv_event_name(GetProcAddress(g_hMPVLib, fn_mpv_event_name));
      mpv_event_to_node := T_mpv_event_to_node(GetProcAddress(g_hMPVLib, fn_mpv_event_to_node));
      mpv_request_event := T_mpv_request_event(GetProcAddress(g_hMPVLib, fn_mpv_request_event));
      mpv_request_log_messages := T_mpv_request_log_messages(GetProcAddress(g_hMPVLib, fn_mpv_request_log_messages));
      mpv_wait_event := T_mpv_wait_event(GetProcAddress(g_hMPVLib, fn_mpv_wait_event));
      mpv_wakeup := T_mpv_wakeup(GetProcAddress(g_hMPVLib, fn_mpv_wakeup));
      mpv_set_wakeup_callback := T_mpv_set_wakeup_callback(GetProcAddress(g_hMPVLib, fn_mpv_set_wakeup_callback));
      mpv_wait_async_requests := T_mpv_wait_async_requests(GetProcAddress(g_hMPVLib, fn_mpv_wait_async_requests));
      mpv_hook_add := T_mpv_hook_add(GetProcAddress(g_hMPVLib, fn_mpv_hook_add));
      mpv_hook_continue := T_mpv_hook_continue(GetProcAddress(g_hMPVLib, fn_mpv_hook_continue));
      {$IFDEF MPV_ENABLE_DEPRECATED}
      mpv_get_wakeup_pipe := T_mpv_get_wakeup_pipe(GetProcAddress(g_hMPVLib, fn_mpv_get_wakeup_pipe));
      {$ENDIF MPV_ENABLE_DEPRECATED}
      Result := Assigned(mpv_client_api_version);
    end;
  end;
end;
{$ELSE MPV_DYNAMIC_LOAD}
function MPVLibLoaded(const sLibPath: string): Boolean;
begin
  Result := True;
end;
{$ENDIF MPV_DYNAMIC_LOAD}

initialization
finalization
{$IFDEF MPV_DYNAMIC_LOAD}
  MPVLibFree;
{$ENDIF MPV_DYNAMIC_LOAD}

end.
