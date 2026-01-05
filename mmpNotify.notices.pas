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
  vcl.graphics,
  mmpConsts;

type
  TNoticeEvent = (evNone,

    evAboutFormShow, evAboutGNULicenceShow, evAboutReleaseNotesFormShow, evAboutPreviousReleaseNotes,

    evGSActiveTasks, evGSActiveTaskPercent, evGSAppWnd, evGSAutoCenter, evGSHasCoverArt, evGSIDDms, evGSImagesPaused,
    evGSMainForm, evGSMaxSize, evGSMediaType, evGSMPVScreenshotDirectory,
    evGSRepeatDelayMs,
    evGSShowingAbout, evGSShowingHelp, evGSShowingPlaylist, evGSShowingStreamlist, evGSShowingThumbs, evGSShowingTimeline,
    evGSTimelineHeight, evGSUserInput,
    evGSWidthHelp, evGSWidthPlaylist, evGSWidthStreamlist, evGSOpeningURL, evGSShowingConfig, evGSNoPlaylist, evGSCleanup, evGSRenameFile, evGSSkipExcluded, evGSDuration,
    evGSShuffle, evGSArrangeAll, evGSSuspended,

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
    evMPVolDn, evMPVolUp, evMPZoomIn, evMPZoomOut, evMPZoomReset, evMPSeek,

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
    evVMMPPlayEdited, evVMRenameCleanFile, evVMToggleSkipExcluded, evVMToggleShuffle,

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
    function  getPoint:       TPoint;
    function  getReasonType:  TReasonType;
    function  getShiftState:  TShiftState;
    function  getText:        string;
    function  getWndRec:      TWndRec;

    procedure setEvent(const aValue: TNoticeEvent);
    procedure setBoolean(const aValue: boolean);
    procedure setComponent(const aValue: TComponent);
    procedure setInteger(const aValue: integer);
    procedure setMediaType(const aValue: TMediaType);
    procedure setMessage(const aValue: TMessage);
    procedure setPoint(const aValue: TPoint);
    procedure setReasonType(const aValue: TReasonType);
    procedure setShiftState(const aValue: TShiftState);
    procedure setText(const aValue: string);
    procedure setWndRec(const aValue: TWndRec);

    property  event:          TNoticeEvent        read getEvent          write setEvent;
    property  component:      TComponent          read getComponent      write setComponent;
    property  integer:        integer             read getInteger        write setInteger;
    property  mediaType:      TMediaType          read getMediaType      write setMediaType;
    property  msg:            TMessage            read getMessage        write setMessage;
    property  point:          TPoint              read getPoint          write setPoint;
    property  reasonType:     TReasonType         read getReasonType     write setReasonType;
    property  shiftState:     TShiftState         read getShiftState     write setShiftState;
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

function newNotice:                                                                                         INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aBoolean:    boolean):                                 INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aComponent:  TComponent):                              INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aInteger:    integer):                                 INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aMediaType:  TMediaType):                              INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aMsg:        TMessage):                                INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aPt:         TPoint):                                  INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aReasonType: TReasonType):                             INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aShiftState: TShiftState):                             INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aText:       string = EMPTY):                          INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aText:       string; const aMediaType: TMediaType):    INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aWndRec:     TWndRec):                                 INotice; overload;

implementation

type
  TNotice = class(TInterfacedObject, INotice)
  private
    FEvent:         TNoticeEvent;

    FBoolean:       boolean;
    FComponent:     TComponent;
    FDouble:        double;
    FInteger:       integer;
    FMediaType:     TMediaType;
    FMessage:       TMessage;
    FPoint:         TPoint;
    FReasonType:    TReasonType;
    FShiftState:    TShiftState;
    FText:          string;
    FWndRec:        TWndRec;
  protected
    function  getEvent:       TNoticeEvent;

    function  getBoolean:     boolean;
    function  getComponent:   TComponent;
    function  getInteger:     integer;
    function  getMediaType:   TMediaType;
    function  getMessage:     TMessage;
    function  getPoint:       TPoint;
    function  getReasonType:  TReasonType;
    function  getShiftState:  TShiftState;
    function  getText:        string;
    function  getWndRec:      TWndRec;

    procedure setEvent      (const aValue: TNoticeEvent);

    procedure setBoolean    (const aValue: boolean);
    procedure setComponent  (const aValue: TComponent);
    procedure setInteger    (const aValue: integer);
    procedure setMediaType  (const aValue: TMediaType);
    procedure setMessage    (const aValue: TMessage);
    procedure setPoint      (const aValue: TPoint);
    procedure setReasonType (const aValue: TReasonType);
    procedure setShiftState (const aValue: TShiftState);
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

{ TNotice }

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

function TNotice.getPoint: TPoint;
begin
  result := FPoint;
end;

function TNotice.getReasonType: TReasonType;
begin
  result := FReasonType;
end;

function TNotice.getShiftState: TShiftState;
begin
  result := FShiftState;
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

procedure TNotice.setPoint(const aValue: TPoint);
begin
  FPoint := aValue;
end;

procedure TNotice.setReasonType(const aValue: TReasonType);
begin
  FReasonType := aValue;
end;

procedure TNotice.setShiftState(const aValue: TShiftState);
begin
  FShiftState := aValue;
end;

procedure TNotice.setText(const aValue: string);
begin
  FText := aValue;
end;

procedure TNotice.setWndRec(const aValue: TWndRec);
begin
  FWndRec := aValue;
end;

end.
