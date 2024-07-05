{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
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
  mmpConsts;

type
  TNoticeEvent = (evNone,
    evAboutFormShow, evAboutReleaseNotesFormShow,

    evGSAppWnd, evGSAutoCenter, evGSHasCoverArt,
    evGSMainForm, evGSMaxSize, evGSMediaType, evGSMPVScreenshotDirectory,
    evGSShowingAbout, evGSShowingHelp, evGSShowingPlaylist, evGSShowingStreamlist, evGSShowingThumbs, evGSShowingTimeline,
    evGSTimelineHeight, evGSUserInput,
    evGSWidthHelp, evGSWidthPlaylist, evGSWidthStreamlist,

    evHelpMoveHelp, evHelpShutHelp, evHelpShowHelp,

    evMCBrighter, evMCCaption, evMCDarker, evMCReset, evMCReshowCaption,

    evMIFillMetaData, evMIGetMediaInfo, evMIReqHasCoverArt,

    evMPBrightnessDn, evMPBrightnessReset, evMPBrightnessUp,
    evMPCycleAudio, evMPCycleSubs,
    evMPContrastDn, evMPContrastReset, evMPContrastUp,
    evMPDuration,
    evMPFrameBackwards, evMPFrameForwards,
    evMPGammaDn, evMPGammaReset, evMPGammaUp,
    evMPKeepOpen,
    evMPMuteUnmute, evMPNextChapter, evMPPrevChapter, evMPOpenUrl, evMPPanDn, evMPPanLeft, evMPPanReset, evMPPanRight, evMPPanUp,
    evMPPause, evMPPausePlay, evMPPosition, evMPResetAll,
    evMPRotateLeft, evMPRotateReset, evMPRotateRight,
    evMPSaturationDn, evMPSaturationUp, evMPSaturationReset, evMPScreenshot, evMPSpeedDn, evMPSpeedUp, evMPSpeedReset, evMPStartOver, evMPStop,
    evMPToggleRepeat, evMPToggleSubtitles,
    evMPVolDn, evMPVolUp, evMPZoomIn, evMPZoomOut, evMPZoomReset,

    evMPStateEnd, evMPStatePlay,

    evMPReqDuration, evMPReqFileName, evMPReqIDD, evMPReqImagesPaused, evMPReqPosition, evMPReqVideoHeight, evMPReqVideoWidth,

    evMXSysVolMax,

    evPAPostToAll, evPAPostToAllEx, evPAPostToEvery, evPAPostToEveryEx, evPAReqCount,

    evPBBrighter, evPBClick, evPBDarker, evPBMax, evPBPosition, evPBReset, evPBSetNewPosition, evPBToggleProgressBar,
    evPBReqMax, evPBReqPosition,

    evPLCopyToClipboard, evPLDeleteIx, evPLFillListBox, evPLFillPlaylist, evPLFind, evPLFirst, evPLLast, evPLNext, evPLPrev, evPLReplaceCurrentItem,
    evPLReqCurrentFolder, evPLReqCurrentItem, evPLReqCurrentIx, evPLReqFormattedItem, evPLReqHasItems, evPLReqIsLast, evPLReqThisItem,

    evPLFormHighlight, evPLFormLoadBox, evPLFormMove, evPLFormShow, evPLFormShutForm,

    evSTBlankInTimeCaption, evSTBlankOutTimeCaption, evSTBrighter, evSTDarker, evSTDisplayTime, evSTDisplayXY, evSTOpInfo, evSTReset, evSTToggleCaptions, evSTUpdateMetaData,

    evTickTimer,

    evTimelineHeight,

    evVMAdjustAspectRatio, evVMArrangeAll, evVMCenterWindow, evVMDeleteCurrentItem, evVMDoEscapeKey, evVMImageInBrowser, evVMKeepCurrentItem, evVMKeepDelete,
    evVMMinimize, evVMMoveHelp, evVMMovePlaylist, evVMMoveTimeline, evVMOnOpen, evVMShowThumbs,
    evVMMPPlayCurrent, evVMPlayNextFolder, evVMPlayPrevFolder, evVMReloadPlaylist, evVMResizeWindow, evVMToggleFullscreen, evVMToggleHelp, evVMTogglePlaylist, evVMToggleEditMode,
    evVMMPPlayFirst, evVMMPPlayLast, evVMMPPlayNext, evVMMPPlayPrev, evVMShutTimeline, evVMPlaySomething, evVMRenameCurrentItem,

    evAppClose, evAppCloseAll,

    evWheelDn, evWheelUp,

    evWndResize

    );

  TNoticeEvents = set of TNoticeEvent; // not currently used

  INotice = interface
    ['{2BB04DBB-6D61-4E4F-8C70-8BCC8E36FDE4}']
    function  getEvent:       TNoticeEvent;
    function  getBoolean:     boolean;
    function  getComponent:   TComponent;
    function  getInteger:     integer;
    function  getMediaType:   TMediaType;
    function  getMessage:     TMessage;
    function  getPoint:       TPoint;
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
    procedure setShiftState(const aValue: TShiftState);
    procedure setText(const aValue: string);
    procedure setWndRec(const aValue: TWndRec);

    property  event:          TNoticeEvent        read getEvent          write setEvent;
    property  component:      TComponent          read getComponent      write setComponent;
    property  integer:        integer             read getInteger        write setInteger;
    property  mediaType:      TMediaType          read getMediaType      write setMediaType;
    property  msg:            TMessage            read getMessage        write setMessage;
    property  point:          TPoint              read getPoint          write setPoint;
    property  shiftState:     TShiftState         read getShiftState     write setShiftState;
    property  text:           string              read getText           write setText;
    property  tf:             boolean             read getBoolean        write setBoolean;
    property  wndRec:         TWndRec             read getWndRec         write setWndRec;
  end;

  TNotifyMethod = function(const aNotice: INotice): INotice of object;

  ISubscriber = interface
    ['{955BF992-F4FA-4141-9C0F-04600C582C00}']
    function  notifySubscriber(const aNotice: INotice): INotice;
    procedure setNotifyMethod(const aNotifyMethod: TNotifyMethod);
    property  notifyMethod: TNotifyMethod write setNotifyMethod;
  end;

  INotifier = interface
    ['{DD326AE1-5049-43AA-9215-DF53DB5FC958}']
    procedure subscribe(const aSubscriber: ISubscriber);
    procedure unsubscribe(const aSubscriber: ISubscriber);
    procedure notifySubscribers(const aNotice: INotice);
  end;

function newNotice: INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aBoolean: boolean):                      INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aComponent: TComponent):                 INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aInteger: integer):                      INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aMediaType: TMediaType):                 INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aMsg: TMessage):                         INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aPt: TPoint):                            INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aShiftState: TShiftState):               INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aText: string = ''):                     INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aText: string; aMediaType: TMediaType):  INotice; overload;
function newNotice(const aEvent: TNoticeEvent; const aWndRec: TWndRec):                       INotice; overload;

implementation

type
  TNotice = class(TInterfacedObject, INotice)
  private
    FEvent:         TNoticeEvent;
    FBoolean:       boolean;
    FComponent:     TComponent;
    FInteger:       integer;
    FMediaType:     TMediaType;
    FMessage:       TMessage;
    FPoint:         TPoint;
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
    procedure setShiftState(const aValue: TShiftState);
    procedure setText(const aValue: string);
    procedure setWndRec(const aValue: TWndRec);
  public
  end;

function newNotice: INotice;
begin
  result := TNotice.create;
end;

function newNotice(const aEvent: TNoticeEvent; const aText: string = ''): INotice;
begin
  result        := newNotice;
  result.event  := aEvent;
  result.text   := aText;
end;

function newNotice(const aEvent: TNoticeEvent; const aText: string; aMediaType: TMediaType):  INotice; overload;
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
