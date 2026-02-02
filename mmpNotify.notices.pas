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
unit mmpNotify.notices;

interface

uses
  winApi.messages, winApi.windows,
  system.classes,
  vcl.forms, vcl.graphics,
  mmpAction, mmpConsts;

type
  TNoticeEvent = (evNone,

    evAboutFormShow, evAboutGNULicenceShow, evAboutReleaseNotesFormShow, evAboutPreviousReleaseNotes,

    evGSActiveTasks, evGSActiveTaskPercent, evGSAppWnd, evGSAutoCenter, evGSHasCoverArt, evGSIDDms, evGSImagesPaused,
    evGSMainForm, evGSMaxSize, evGSMediaType, evGSMPVScreenshotDirectory,
    evGSRepeatDelayMs,
    evGSShowingAbout, evGSShowingHelp, evGSShowingPlaylist, evGSShowingStreamlist, evGSShowingThumbs, evGSShowingTimeline,
    evGSTimelineHeight, evGSUserInput, evGSIgnoreEscape,
    evGSWidthHelp, evGSWidthPlaylist, evGSWidthStreamlist, evGSOpeningURL, evGSShowingConfig, evGSNoPlaylist, evGSCleanup, evGSRenameFile, evGSSkipExcluded, evGSDuration,
    evGSShuffle, evGSArrangeAll, evGSSuspended, evGSShowingHelpFull, evGSMonitor, evGSMonitorCount, evGSMonitorIx,

    evHelpMoveHelp, evHelpShutHelp, evHelpShowHelp,

    evMCBrighter, evMCCaption, evMCDarker, evMCReset, evMCReshowCaption,

    evMIFillMetaData, evMIGetMediaInfo, evMIReqHasCoverArt, evMIReqDuration,

    evMPBrightnessDn, evMPBrightnessReset, evMPBrightnessUp,
    evMPCycleAudio, evMPCycleSubs,
    evMPContrastDn, evMPContrastReset, evMPContrastUp, evMPDetachStates,
    evMPDuration,
    evMPFrameBackwards, evMPFrameForwards,
    evMPGammaDn, evMPGammaReset, evMPGammaUp,
    evMPKeepOpen,
    evMPMuteUnmute, evMPNextChapter, evMPPrevChapter, evMPOpenUrl, evMPPanDn, evMPPanLeft, evMPPanReset, evMPPanRight, evMPPanUp,
    evMPPause, evMPPausePlay, evMPPosition, evMPResetAll, evMPResume,
    evMPRotateLeft, evMPRotateReset, evMPRotateRight,
    evMPSaturationDn, evMPSaturationUp, evMPSaturationReset, evMPScreenshot, evMPSpeedDn, evMPSpeedUp, evMPSpeedReset, evMPStartOver, evMPStop,
    evMPToggleRepeat, evMPToggleSubtitles,
    evMPVolDn, evMPVolUp, evMPZoomIn, evMPZoomOut, evMPZoomReset, evMPSeek, evMPSyncAudioUp, evMPSyncAudioDn,

    evMPStateEnd, evMPStateLoading, evMPStatePlay,

    {evMPReqDuration,} evMPReqFileName, evMPReqIDDms, evMPReqPlaying, evMPReqPosition, evMPReqPrecisePos, evMPReqVideoHeight, evMPReqVideoWidth,

    evMXSysVolMax,

    evPAPostToAll, evPAPostToAllEx, evPAPostToEvery, evPAPostToEveryEx, evPAReqCount,

    evPBBackgroundColor, evPBBrighter, evPBClick, evPBDarker, evPBMax, evPBPosition, evPBReset, evPBSetNewPosition, evPBToggleProgressBar,
    evPBReqMax, evPBReqPosition, {evPBKeyFrames,}

    evPLCopyToClipboard, evPLDeleteIx, evPLFillListBox, evPLFillPlaylist, evPLFind, evPLFirst, evPLLast, evPLNewPlaylist, evPLNext, evPLPrev, evPLReplaceCurrentItem,
    evPLReqCurrentFolder, evPLReqCurrentItem, evPLReqCurrentIx, evPLReqFormattedItem, evPLReqHasItems, evPLReqIsLast, evPLReqIsSpecialImage, evPLReqThisItem,
    evPLAddItem, evPLSetNoItem, evPLReqLastIx,

    evPLFormHighlight, evPLFormLoadBox, evPLFormMove, evPLFormShow, evPLFormShutForm, evPLFormFillNumbers,

    evSTBlankInTimeCaption, evSTBlankOutTimeCaption, evSTBrighter, evSTDarker, evSTDisplayTime, evSTDisplayXY, evSTOpInfo, evSTReset, evSTToggleCaptions, evSTUpdateMetaData,
    evSTForceCaptions, evSTOpInfo2,

    evTickTimer,

    evTimelineHeight, evTLMax, evTLPosition, evTLRename,

    evVMAdjustAspectRatio, evVMArrangeAll, evVMCenterWindow, evVMCleanup, evVMDeleteCurrentItem, evVMDoEscapeKey, evVMImageInBrowser, evVMKeepCurrentItem, evVMKeepDelete,
    evVMKeepCatF1, evVMKeepCatF2, evVMKeepCatF3, evVMKeepCatF4, evVMKeepMove, evVMKeepSave,
    evVMMinimize, evVMMoveHelp, evVMMovePlaylist, evVMMoveTimeline, evVMShowThumbs,
    evVMMPPlayCurrent, evVMPlayNextFolder, evVMPlayPrevFolder, evVMReloadPlaylist, evVMResizeWindow, evVMToggleFullscreen, evVMToggleHelp, evVMTogglePlaylist, evVMToggleEditMode,
    evVMToggleFiltering,
    evVMMPOnOpen, evVMMPPlayFirst, evVMMPPlayLast, evVMMPPlayNext, evVMMPPlayPrev, evVMShutTimeline, evVMPlaySomething, evVMRenameCurrentItem, evVMReInitTimeline, evVMConfig,
    evVMMPPlayEdited, evVMRenameCleanFile, evVMToggleSkipExcluded, evVMToggleShuffle, evVMHelpFull, evVMSkipSeconds, evConfigReload,

    evAppClose, evAppCloseAll,

    evWheelDn, evWheelUp,

    evWndResize

    );

  TNoticeEvents = set of TNoticeEvent; // not currently used

  INotice = interface
    function  getEvent:       TNoticeEvent;
    function  getBoolean:     boolean;
    function  getComponent:   TComponent;
    function  getInteger:     integer;
    function  getMediaType:   TMediaType;
    function  getMessage:     TMessage;
    function  getMonitor:     TMonitor;
    function  getPoint:       TPoint;
    function  getPointer:     pointer;
    function  getReasonType:  TReasonType;
    function  getShiftState:  TShiftState;
    function  getSizeOf:      integer;
    function  getText:        string;
    function  getWndRec:      TWndRec;

    function  asRecord(out aDest):  TVoid;

    procedure setEvent(const aValue: TNoticeEvent);
    procedure setBoolean(const aValue: boolean);
    procedure setComponent(const aValue: TComponent);
    procedure setInteger(const aValue: integer);
    procedure setMediaType(const aValue: TMediaType);
    procedure setMessage(const aValue: TMessage);
    procedure setMonitor(const aValue: TMonitor);
    procedure setPoint(const aValue: TPoint);
    procedure setPointer(const aValue: pointer);
    procedure setReasonType(const aValue: TReasonType);
    procedure setShiftState(const aValue: TShiftState);
    procedure setSizeOf(const aValue: integer);
    procedure setText(const aValue: string);
    procedure setWndRec(const aValue: TWndRec);

    property  event:          TNoticeEvent        read getEvent          write setEvent;
    property  component:      TComponent          read getComponent      write setComponent;
    property  integer:        integer             read getInteger        write setInteger;
    property  mediaType:      TMediaType          read getMediaType      write setMediaType;
    property  monitor:        TMonitor            read getMonitor        write setMonitor;
    property  msg:            TMessage            read getMessage        write setMessage;
    property  point:          TPoint              read getPoint          write setPoint;
    property  pointer:        pointer             read getPointer        write setPointer;
    property  reasonType:     TReasonType         read getReasonType     write setReasonType;
    property  shiftState:     TShiftState         read getShiftState     write setShiftState;
    property  sizeOf:         integer             read getSizeOf         write setSizeOf;
    property  text:           string              read getText           write setText;
    property  tf:             boolean             read getBoolean        write setBoolean;
    property  wndRec:         TWndRec             read getWndRec         write setWndRec;
  end;

  TNotifyMethod = function(const aNotice: INotice): INotice of object;

  ISubscriber = interface
    function  notifySubscriber(const aNotice: INotice): INotice;
    procedure setNotifyMethod(const aNotifyMethod: TNotifyMethod);
    property  notifyMethod: TNotifyMethod write setNotifyMethod;
  end;

  ISubscribable = interface
    function  subscribe(const aSubscriber: ISubscriber): ISubscriber;
    procedure unsubscribe(const aSubscriber: ISubscriber);
    procedure unsubscribeAll;
  end;

  INotifier = interface(ISubscribable)
    procedure notifySubscribers(const aNotice: INotice);
  end;

type
  TNoticeType = (ntNone, ntBoolean, ntByte, ntWord, ntCardinal, ntInteger, ntInt64, ntSingle, ntDouble, ntCurrency, ntDateTime, ntString, ntWideString, ntUnicodeString, ntChar, ntWideChar, ntPointer,
                 ntTComponent, ntRecord);
  PComponent = ^TComponent;
  baz = record
    class function  newNotice: INotice; overload; static;
    class function  newNotice<T>(const  aEvent: TNoticeEvent; const  aNoticeType: TNoticeType; const  aParam: T): INotice; overload; static;
    // ... any multi-parameter overloads that can't be generic ...
  end;

function newNotice:                                                                                           INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aBoolean:    boolean):                                  INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aComponent:  TComponent):                               INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aInteger:    integer):                                  INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aMediaType:  TMediaType):                               INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aMsg:        TMessage):                                 INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aPt:         TPoint):                                   INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aReasonType: TReasonType):                              INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aShiftState: TShiftState):                              INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aText:       string = EMPTY):                           INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aText:       string; const aMediaType: TMediaType):     INotice; overload;
function newNotice(const aEvent:  TNoticeEvent; const aWndRec:     TWndRec):                                  INotice; overload;
function newNotice(const aString: string;       const aMediaType:  TMediaType):                               INotice; overload;

implementation

type
  TNotice = class(TInterfacedObject, INotice)
  private
    FEvent:         TNoticeEvent;

    FBoolean:       boolean;
    FComponent:     TComponent;
    // FDouble:        double;
    FInteger:       integer;
    FMediaType:     TMediaType;
    FMessage:       TMessage;
    FMonitor:       TMonitor;
    FPoint:         TPoint;
    FReasonType:    TReasonType;
    FShiftState:    TShiftState;
    FText:          string;
    FWndRec:        TWndRec;

    FPointer:       pointer;
  protected
    FSizeOf:        integer;
    function  getEvent:       TNoticeEvent;

    function  getBoolean:     boolean;
    function  getComponent:   TComponent;
    function  getInteger:     integer;
    function  getMediaType:   TMediaType;
    function  getMessage:     TMessage;
    function  getMonitor:     TMonitor;
    function  getPoint:       TPoint;
    function  getPointer:     pointer;
    function  getReasonType:  TReasonType;
    function  getShiftState:  TShiftState;
    function  getSizeOf:      integer;
    function  getText:        string;
    function  getWndRec:      TWndRec;


    function  asRecord(out aDest): TVoid;

    procedure setEvent      (const aValue: TNoticeEvent);

    procedure setBoolean    (const aValue: boolean);
    procedure setComponent  (const aValue: TComponent);
    procedure setInteger    (const aValue: integer);
    procedure setMediaType  (const aValue: TMediaType);
    procedure setMessage    (const aValue: TMessage);
    procedure setMonitor    (const aValue: TMonitor);
    procedure setPoint      (const aValue: TPoint);
    procedure setPointer    (const aValue: pointer);
    procedure setReasonType (const aValue: TReasonType);
    procedure setShiftState (const aValue: TShiftState);
    procedure setSizeOf     (const aValue: integer);
    procedure setText       (const aValue: string);
    procedure setWndRec     (const aValue: TWndRec);
  public
  end;

function newNotice: INotice;
begin
  result := TNotice.create;
end;

function newNotice(const aEvent: TNoticeEvent; const aText: string = EMPTY): INotice;
begin
  result        := newNotice;
  result.event  := aEvent;
  result.text   := aText;
end;

function newNotice(const aEvent: TNoticeEvent; const aText: string; const aMediaType: TMediaType): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.mediaType  := aMediaType;
  result.text       := aText;
end;

function newNotice(const aEvent: TNoticeEvent; const aBoolean: boolean): INotice; overload;
begin
  result          := newNotice;
  result.event    := aEvent;
  result.tf       := aboolean;
end;

function newNotice(const aEvent: TNoticeEvent; const aComponent: TComponent): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.component  := aComponent;
end;

function newNotice(const aEvent: TNoticeEvent; const aInteger: integer): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.integer    := aInteger;
end;

function newNotice(const aEvent: TNoticeEvent; const aMediaType: TMediaType): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.mediaType  := aMediaType;
end;

function newNotice(const aEvent: TNoticeEvent; const aMsg: TMessage): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.msg        := aMsg;
end;

function newNotice(const aEvent: TNoticeEvent; const aPt: TPoint): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.point      := aPt;
end;

function newNotice(const aEvent: TNoticeEvent; const aReasonType: TReasonType): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.reasonType := aReasonType;
end;

function newNotice(const aEvent: TNoticeEvent; const aShiftState: TShiftState): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.shiftState := aShiftState;
end;

function newNotice(const aEvent: TNoticeEvent; const aWndRec: TWndRec): INotice; overload;
begin
  result            := newNotice;
  result.event      := aEvent;
  result.wndRec     := aWndRec;
end;

function newNotice(const aString: string; const aMediaType:  TMediaType): INotice; overload;
begin
  result            := newNotice;
  result.text       := aString;
  result.mediaType  := aMediaType;
end;


{ TNotice }

function TNotice.asRecord(out aDest): TVoid;
begin
  move(FPointer^, aDest, FSizeOf);
end;

function TNotice.getBoolean: boolean;
begin
  result := FBoolean;
end;

//function TNotice.getCardinal: cardinal;
//begin
//  result := FCardinal;
//end;

function TNotice.getComponent: TComponent;
begin
  result := FComponent;
end;

function TNotice.getEvent: TNoticeEvent;
begin
  result := FEvent;
end;

function TNotice.getInteger: integer;
begin
  result := FInteger;
end;

function TNotice.getMediaType: TMediaType;
begin
  result := FMediaType;
end;

function TNotice.getMessage: TMessage;
begin
  result := FMessage;
end;

function TNotice.getMonitor: TMonitor;
begin
  result := FMonitor;
end;

function TNotice.getPoint: TPoint;
begin
  result := FPoint;
end;

function TNotice.getPointer: pointer;
begin
  result := FPointer;
end;

function TNotice.getReasonType: TReasonType;
begin
  result := FReasonType;
end;

function TNotice.getShiftState: TShiftState;
begin
  result := FShiftState;
end;

function TNotice.getSizeOf: integer;
begin
  result := FSizeOf;
end;

function TNotice.getText: string;
begin
  result := FText;
end;

function TNotice.getWndRec: TWndRec;
begin
  result := FWndRec;
end;

procedure TNotice.setBoolean(const aValue: boolean);
begin
  FBoolean := aValue;
end;

//procedure TNotice.setCardinal(const aValue: cardinal);
//begin
//  FCardinal := aValue;
//end;

procedure TNotice.setComponent(const aValue: TComponent);
begin
  FComponent := aValue;
end;

procedure TNotice.setEvent(const aValue: TNoticeEvent);
begin
  FEvent := aValue;
end;

procedure TNotice.setInteger(const aValue: integer);
begin
  FInteger := aValue;
end;

procedure TNotice.setMediaType(const aValue: TMediaType);
begin
  FMediaType := aValue;
end;

procedure TNotice.setMessage(const aValue: TMessage);
begin
  FMessage := aValue;
end;

procedure TNotice.setMonitor(const aValue: TMonitor);
begin
  FMonitor := aValue;
end;

procedure TNotice.setPoint(const aValue: TPoint);
begin
  FPoint := aValue;
end;

procedure TNotice.setPointer(const aValue: pointer);
begin
  FPointer := aValue;
end;

procedure TNotice.setReasonType(const aValue: TReasonType);
begin
  FReasonType := aValue;
end;

procedure TNotice.setShiftState(const aValue: TShiftState);
begin
  FShiftState := aValue;
end;

procedure TNotice.setSizeOf(const aValue: integer);
begin
  FSizeOf := aValue;
end;

procedure TNotice.setText(const aValue: string);
begin
  FText := aValue;
end;

procedure TNotice.setWndRec(const aValue: TWndRec);
begin
  FWndRec := aValue;
end;

{ baz }

class function baz.newNotice: INotice;
begin
  result        := TNotice.Create;
end;

class function  baz.newNotice<T>(const  aEvent: TNoticeEvent; const  aNoticeType: TNoticeType; const  aParam: T): INotice;
begin
  result        := newNotice;
  result.event  := aEvent;
  result.sizeOf := sizeOf(T);

  case aNoticeType = ntBoolean        of TRUE: result.tf        := PBoolean   (@aParam)^;       end;
  case aNoticeType = ntInteger        of TRUE: result.integer   := PInteger   (@aParam)^;       end;
  case aNoticeType = ntString         of TRUE: result.text      := PString    (@aParam)^;       end;
  case aNoticeType = ntTComponent     of TRUE: result.component := PComponent (@aParam)^;       end;
  case aNoticeType = ntRecord         of TRUE: result.pointer   := PPointer   (@aParam)^;       end;

//  case aNoticeType = ntByte          of TRUE: result.vByte           := PByte(@aParam)^; end;
//  case aNoticeType = ntWord          of TRUE: result.vWord           := PWord(@aParam)^; end;
//  case aNoticeType = ntCardinal      of TRUE: result.vCardinal       := PCardinal(@aParam)^; end;
//  case aNoticeType = ntInt64         of TRUE: result.vInt64          := PInt64(@aParam)^; end;
//  case aNoticeType = ntSingle        of TRUE: result.vSingle         := PSingle(@aParam)^; end;
//  case aNoticeType = ntDouble        of TRUE: result.vDouble         := PDouble(@aParam)^; end;
//  case aNoticeType = ntCurrency      of TRUE: result.vCurrency       := PCurrency(@aParam)^; end;
//  case aNoticeType = ntDateTime      of TRUE: result.dateTime        := PDateTime(@aParam)^; end;
//  case aNoticeType = ntWideString    of TRUE: result.vWideString     := PWideString(@aParam)^; end;
//  case aNoticeType = ntUnicodeString of TRUE: result.vUnicodeString  := PUnicodeString(@aParam)^; end;
//  case aNoticeType = ntChar          of TRUE: result.vChar           := PChar(@aParam)^; end;
//  case aNoticeType = ntWideChar      of TRUE: result.vWideChar       := PWideChar(@aParam)^; end;
//  case aNoticeType = ntPointer       of TRUE: result.vPointer        := PPointer(@aParam)^; end
  end;

end.

