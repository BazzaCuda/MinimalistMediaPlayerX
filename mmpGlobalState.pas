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
unit mmpGlobalState;

interface

uses
  winAPI.windows,
  vcl.forms,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  IGlobalState = interface
    function getActiveTasks:              integer;
    function getActiveTaskPercent:        integer;
    function getArrangeAll:               boolean;
    function getAutoCenter:               boolean;
    function getCleanup:                  boolean;
    function getDuration:                 integer;
    function getHelpFull:                 boolean;
    function getIDDms:                    integer;
    function getIgnoreEscape:             boolean;
    function getImagesPaused:             boolean;
    function getMainForm:                 TForm;
    function getMaxSize:                  boolean;
    function getMediaType:                TMediaType;
    function getMonitor:                  TMonitor;
    function getMonitorCount:             integer;
    function getMonitorIx:                integer;
    function getMPVScreenshotDirectory:   string;
    function getNoPlaylist:               boolean;
    function getOpeningURL:               boolean;
    function getRenameFile:               boolean;
    function getRepeatDelayMs:            integer;
    function getShowingAbout:             boolean;
    function getShowingConfig:            boolean;
    function getShowingHelp:              boolean;
    function getShowingPlaylist:          boolean;
    function getShowingStreamlist:        boolean;
    function getShowingThumbs:            boolean;
    function getShowingTimeline:          boolean;
    function getShuffle:                  boolean;
    function getSkipExcluded:             boolean;
    function getSuspended:                boolean;
    function getTimelineHeight:           integer;
    function getUserInput:                boolean;
    function getWidthHelp:                integer;
    function getWidthPlaylist:            integer;
    function getWidthStreamlist:          integer;

    procedure setMaxSize(const aValue: boolean);

    function notify(const aNotice: INotice): INotice;

    property activeTasks:               integer             read getActiveTasks; // Unfortunately, Delphi requires getters and setters for interface properties :(
    property activeTaskPercent:         integer             read getActiveTaskPercent;
    property arrangeAll:                boolean             read getArrangeAll;
    property autoCenter:                boolean             read getAutoCenter;
    property cleanup:                   boolean             read getCleanup;
    property duration:                  integer             read getDuration;
    property helpFull:                  boolean             read getHelpFull;
    property IDDms:                     integer             read getIDDms;       // image-display-duration in milliseconds
    property ignoreEscape:              boolean             read getIgnoreEscape;
    property imagesPaused:              boolean             read getImagesPaused;
    property mainForm:                  TForm               read getMainForm;
    property maxSize:                   boolean             read getMaxSize;
    property mediaType:                 TMediaType          read getMediaType;
    property monitor:                   TMonitor            read getMonitor;
    property monitorCount:              integer             read getMonitorCount;
    property monitorIx:                 integer             read getMonitorIx;
    property MPVScreenshotDirectory:    string              read getMPVScreenshotDirectory;
    property noPlaylist:                boolean             read getNoPlaylist;
    property openingURL:                boolean             read getOpeningURL;
    property repeatDelayMs:             integer             read getRepeatDelayMs;
    property renameFile:                boolean             read getRenameFile;
    property showingAbout:              boolean             read getShowingAbout;
    property showingConfig:             boolean             read getShowingConfig;
    property showingHelp:               boolean             read getShowingHelp;
    property showingPlaylist:           boolean             read getShowingPlaylist;
    property showingStreamlist:         boolean             read getShowingStreamlist;
    property showingThumbs:             boolean             read getShowingThumbs;
    property showingTimeline:           boolean             read getShowingTimeline;
    property shuffle:                   boolean             read getShuffle;
    property skipExcluded:              boolean             read getSkipExcluded;
    property suspended:                 boolean             read getSuspended;
    property timelineHeight:            integer             read getTimelineHeight;
    property userInput:                 boolean             read getUserInput;
    property widthHelp:                 integer             read getWidthHelp;
    property widthPlaylist:             integer             read getWidthPlaylist;
    property widthStreamlist:           integer             read getWidthStreamlist;
  end;

function GS:IGlobalState;

implementation

uses
  System.Generics.Collections,
  _debugWindow;

type
  TGlobalState = class(TInterfacedObject, IGlobalState)
  strict private
    FActiveTasks:             integer;
    FActiveTaskPercent:       integer;
    FArrangeAll:              boolean;
    FAutoCenter:              boolean;
    FCleanup:                 boolean;
    FDuration:                integer;
    FHelpFull:                boolean;
    FIDDms:                   integer;
    FIgnoreEscape:            boolean;
    FImagesPaused:            boolean;
    FMainForm:                TForm;
    FMaxSize:                 boolean;
    FMediaType:               TMediaType;
    FMonitor:                 TMonitor;
    FMonitorCount:            integer;
    FMonitorIx:               integer;
    FMPVScreenshotDirectory:  string;
    FNoPlaylist:              boolean;
    FOpeningURL:              boolean;
    FRenameFile:              boolean;
    FRepeatDelayMs:           integer;
    FShowingAbout:            boolean;
    FShowingConfig:           boolean;
    FShowingHelp:             boolean;
    FShowingPlaylist:         boolean;
    FShowingStreamlist:       boolean;
    FShowingThumbs:           boolean;
    FShowingTimeline:         boolean;
    FShuffle:                 boolean;
    FSkipExcluded:            boolean;
    FSuspended:               boolean;
    FWidthHelp:               integer;
    FWidthPlaylist:           integer;
    FWidthStreamlist:         integer;
    FSubscriber:              ISubscriber;
    FTimelineHeight:          integer;
    FUserInput:               boolean;
  private
    function onNotify(const aNotice: INotice): INotice;
  public
    constructor Create;
    destructor  Destroy; override;
    function  getActiveTasks:               integer;
    function  getActiveTaskPercent:         integer;
    function  getArrangeAll:                boolean;
    function  getAutoCenter:                boolean;
    function  getCleanup:                   boolean;
    function  getDuration:                  integer;
    function  getHelpFull:                  boolean;
    function  getIDDms:                     integer;
    function  getIgnoreEscape:              boolean;
    function  getImagesPaused:              boolean;
    function  getMainForm:                  TForm;
    function  getMaxSize:                   boolean;
    function  getMediaType:                 TMediaType;
    function  getMonitor:                   TMonitor;
    function  getMonitorCount:              integer;
    function  getMonitorIx:                 integer;
    function  getMPVScreenshotDirectory:    string;
    function  getNoPlaylist:                boolean;
    function  getOpeningURL:                boolean;
    function  getRenameFile:                boolean;
    function  getRepeatDelayMs:             integer;
    function  getShowingAbout:              boolean;
    function  getShowingConfig:             boolean;
    function  getShowingHelp:               boolean;
    function  getShowingPlaylist:           boolean;
    function  getShowingStreamlist:         boolean;
    function  getShowingThumbs:             boolean;
    function  getShowingTimeline:           boolean;
    function  getShuffle:                   boolean;
    function  getSkipExcluded:              boolean;
    function  getSuspended:                 boolean;
    function  getTimelineHeight:            integer;
    function  getUserInput:                 boolean;
    function  getWidthHelp:                 integer;
    function  getWidthPlaylist:             integer;
    function  getWidthStreamlist:           integer;

    procedure   setMaxSize(const aValue: boolean);

    function    notify(const aNotice: INotice): INotice;
  end;

function GS: IGlobalState;
{$J+} const gGS: IGlobalState = NIL; {$J-}
begin
  case gGS = NIL of TRUE: gGS := TGlobalState.create; end;
  result := gGS;
end;

{ TGlobalState }

constructor TGlobalState.Create;
begin
  inherited;
  FSubscriber := appEvents.subscribe(newSubscriber(onNotify));
  FActiveTaskPercent := -1;
end;

destructor TGlobalState.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
  inherited;
end;

function TGlobalState.getActiveTaskPercent: integer;
begin
  result := FActiveTaskPercent;
end;

function TGlobalState.getActiveTasks: integer;
begin
  result := FActiveTasks;
end;

function TGlobalState.getArrangeAll: boolean;
begin
  result := FArrangeAll;
end;

function TGlobalState.getAutoCenter: boolean;
begin
  result := FAutoCenter;
end;

function TGlobalState.getCleanup: boolean;
begin
  result := FCleanup;
end;

function TGlobalState.getDuration: integer;
begin
  result := FDuration;
end;

function TGlobalState.getHelpFull: boolean;
begin
  result := FHelpFull;
end;

function TGlobalState.getIDDms: integer;
begin
  result := FIDDms;
end;

function TGlobalState.getIgnoreEscape: boolean;
begin
  result := FIgnoreEscape;
end;

function TGlobalState.getImagesPaused: boolean;
begin
  result := FImagesPaused;
end;

function TGlobalState.getMainForm: TForm;
begin
  result := FMainForm;
end;

function TGlobalState.getMaxSize: boolean;
begin
  result := FMaxSize;
end;

function TGlobalState.getMediaType: TMediaType;
begin
  result := FMediaType;
end;

function TGlobalState.getMonitor: TMonitor;
begin
  result := FMonitor;
end;

function TGlobalState.getMonitorCount: integer;
begin
  result := FMonitorCount;
end;

function TGlobalState.getMonitorIx: integer;
begin
  result := FMonitorIx;
end;

function TGlobalState.getMPVScreenshotDirectory: string;
begin
  result := FMPVScreenshotDirectory;
end;

function TGlobalState.getNoPlaylist: boolean;
begin
  result := FNoPlaylist;
end;

function TGlobalState.getOpeningURL: boolean;
begin
  result := FOpeningURL;
end;

function TGlobalState.getRenameFile: boolean;
begin
  result := FRenameFile;
end;

function TGlobalState.getRepeatDelayMs: integer;
begin
  result := FRepeatDelayMs;
end;

function TGlobalState.getShowingAbout: boolean;
begin
  result := FShowingAbout;
end;

function TGlobalState.getShowingConfig: boolean;
begin
  result := FShowingConfig;
end;

function TGlobalState.getShowingHelp: boolean;
begin
  result := FShowingHelp;
end;

function TGlobalState.getShowingPlaylist: boolean;
begin
  result := FShowingPlaylist;
end;

function TGlobalState.getShowingStreamlist: boolean;
begin
  result := FShowingStreamlist;
end;

function TGlobalState.getShowingThumbs: boolean;
begin
  result := FShowingThumbs;
end;

function TGlobalState.getShowingTimeline: boolean;
begin
  result := FShowingTimeline;
end;

function TGlobalState.getShuffle: boolean;
begin
  result := FShuffle;
end;

function TGlobalState.getSkipExcluded: boolean;
begin
  result := FSkipExcluded;
end;

function TGlobalState.getSuspended: boolean;
begin
  result := FSuspended;
end;

function TGlobalState.getTimelineHeight: integer;
begin
  result := FTimelineHeight;
end;

function TGlobalState.getUserInput: boolean;
begin
  result := FUserInput;
end;

function TGlobalState.getWidthHelp: integer;
begin
  result := FWidthHelp;
end;

function TGlobalState.getWidthPlaylist: integer;
begin
  result := FWidthPlaylist;
end;

function TGlobalState.getWidthStreamlist: integer;
begin
  result := FWidthStreamlist;
end;

function TGlobalState.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TGlobalState.onNotify(const aNotice: INotice): INotice;
begin
  result := aNotice;
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evGSActiveTasks:              FActiveTasks            := aNotice.integer;
    evGSActiveTaskPercent:        FActiveTaskPercent      := aNotice.integer;
    evGSArrangeAll:               FArrangeAll             := aNotice.tf;
    evGSAutoCenter:               FAutoCenter             := aNotice.tf;
    evGSCleanup:                  FCleanup                := aNotice.tf;
    evGSDuration:                 FDuration               := aNotice.integer;
    evGSHelpFull:                 FHelpFull               := aNotice.tf;
    evGSIDDms:                    FIDDms                  := aNotice.integer;
    evGSIgnoreEscape:             FIgnoreEscape           := aNotice.tf;
    evGSImagesPaused:             FImagesPaused           := aNotice.tf;
    evGSMainForm:                 FMainForm               := aNotice.component as TForm;
    evGSMaxSize:                  FMaxSize                := aNotice.tf;
    evGSMediaType:                FMediaType              := aNotice.mediaType;
    evGSMonitor:                  FMonitor                := aNotice.monitor;
    evGSMonitorCount:             FMonitorCount           := aNotice.integer;
    evGSMonitorIx:                FMonitorIx              := aNotice.integer;
    evGSMPVScreenshotDirectory:   FMPVScreenshotDirectory := aNotice.text;
    evGSNoPlaylist:               FNoPlaylist             := aNotice.tf;
    evGSOpeningURL:               FOpeningURL             := aNotice.tf;
    evGSRenameFile:               FRenameFile             := aNotice.tf;
    evGSRepeatDelayMs:            FRepeatDelayMs          := aNotice.integer;
    evGSShowingAbout:             FShowingAbout           := aNotice.tf;
    evGSShowingConfig:            FShowingConfig          := aNotice.tf;
    evGSShowingHelp:              FShowingHelp            := aNotice.tf;
    evGSShowingPlaylist:          FShowingPlaylist        := aNotice.tf;
    evGSShowingStreamlist:        FShowingStreamlist      := aNotice.tf;
    evGSShowingThumbs:            FShowingThumbs          := aNotice.tf;
    evGSShowingTimeline:          FShowingTimeline        := aNotice.tf;
    evGSShuffle:                  FShuffle                := aNotice.tf;
    evGSSkipExcluded:             FSkipExcluded           := aNotice.tf;
    evGSSuspended:                FSuspended              := aNotice.tf;
    evGSTimelineHeight:           FTimelineHeight         := aNotice.integer;
    evGSUserInput:                FUserInput              := aNotice.tf;
    evGSWidthHelp:                FWidthHelp              := aNotice.integer;
    evGSWidthPlaylist:            FWidthPlaylist          := aNotice.integer;
    evGSWidthStreamlist:          FWidthStreamlist        := aNotice.integer;
  end;
end;

procedure TGlobalState.setMaxSize(const aValue: boolean);
begin
  FMaxSize := aValue;
end;

initialization
  GS;

end.
