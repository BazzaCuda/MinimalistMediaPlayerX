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
unit mmpGlobalState;

interface

uses
  winAPI.windows,
  vcl.forms,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  IGlobalState = interface
    ['{0DA7E51A-C0BC-4872-9A7E-9BD14E0DBB62}']
    function getAutoCenter:               boolean;
    function getIDD:                      integer;
    function getImagesPaused:             boolean;
    function getMainForm:                 TForm;
    function getMaxSize:                  boolean;
    function getMediaType:                TMediaType;
    function getMPVScreenshotDirectory:   string;
    function getRepeatDelayMs:            integer;
    function getShowingAbout:             boolean;
    function getShowingHelp:              boolean;
    function getShowingPlaylist:          boolean;
    function getShowingStreamlist:        boolean;
    function getShowingThumbs:            boolean;
    function getShowingTimeline:          boolean;
    function getTimelineHeight:           integer;
    function getUserInput:                boolean;
    function getWidthHelp:                integer;
    function getWidthPlaylist:            integer;
    function getWidthStreamlist:          integer;

    procedure setMaxSize(const aValue: boolean);

    function notify(const aNotice: INotice): INotice;

    property autoCenter:                boolean             read getAutoCenter;
    property IDD:                       integer             read getIDD;
    property imagesPaused:              boolean             read getImagesPaused;
    property mainForm:                  TForm               read getMainForm;
    property maxSize:                   boolean             read getMaxSize;
    property mediaType:                 TMediaType          read getMediaType;
    property repeatDelayMs:             integer             read getRepeatDelayMs;
    property MPVScreenshotDirectory:    string              read getMPVScreenshotDirectory;
    property showingAbout:              boolean             read getShowingAbout;
    property showingHelp:               boolean             read getShowingHelp;
    property showingPlaylist:           boolean             read getShowingPlaylist;
    property showingStreamlist:         boolean             read getShowingStreamlist;
    property showingThumbs:             boolean             read getShowingThumbs;
    property showingTimeline:           boolean             read getShowingTimeline;
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
    FAutoCenter:              boolean;
    FIDD:                     integer;
    FImagesPaused:            boolean;
    FMainForm:                TForm;
    FMaxSize:                 boolean;
    FMediaType:               TMediaType;
    FMPVScreenshotDirectory:  string;
    FRepeatDelayMs:           integer;
    FShowingAbout:            boolean;
    FShowingHelp:             boolean;
    FShowingPlaylist:         boolean;
    FShowingStreamlist:       boolean;
    FShowingThumbs:           boolean;
    FShowingTimeline:         boolean;
    FWidthHelp:               integer;
    FWidthPlaylist:           integer;
    FWidthStreamlist:         integer;
    FSubscriber:              ISubscriber;
    FTimelineHeight:          integer;
    FUserInput:               boolean;
  private
    function onNotify(const aNotice: INotice): INotice;
  public
    constructor create;
    destructor  Destroy; override;
    function    getAutoCenter:               boolean;
    function    getIDD:                      integer;
    function    getImagesPaused:             boolean;
    function    getMainForm:                 TForm;
    function    getMaxSize:                  boolean;
    function    getMediaType:                TMediaType;
    function    getMPVScreenshotDirectory:   string;
    function    getRepeatDelayMs:            integer;
    function    getShowingAbout:             boolean;
    function    getShowingHelp:              boolean;
    function    getShowingPlaylist:          boolean;
    function    getShowingStreamlist:        boolean;
    function    getShowingThumbs:            boolean;
    function    getShowingTimeline:          boolean;
    function    getTimelineHeight:           integer;
    function    getUserInput:                boolean;
    function    getWidthHelp:                integer;
    function    getWidthPlaylist:            integer;
    function    getWidthStreamlist:          integer;

    procedure   setMaxSize(const aValue: boolean);

    function    notify(const aNotice: INotice): INotice;
  end;

var gGS: IGlobalState = NIL;
function GS: IGlobalState;
begin
  case gGS = NIL of TRUE: gGS := TGlobalState.create; end;
  result := gGS;
end;

{ TGlobalState }

constructor TGlobalState.create;
begin
  inherited;
  FSubscriber := appNotifier.subscribe(newSubscriber(onNotify));
end;

destructor TGlobalState.Destroy;
begin
  appNotifier.unsubscribe(FSubscriber);
  inherited;
end;

function TGlobalState.getAutoCenter: boolean;
begin
  result := FAutoCenter;
end;

function TGlobalState.getIDD: integer;
begin
  result := FIDD;
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

function TGlobalState.getMPVScreenshotDirectory: string;
begin
  result := FMPVScreenshotDirectory;
end;

function TGlobalState.getRepeatDelayMs: integer;
begin
  result := FRepeatDelayMs;
end;

function TGlobalState.getShowingAbout: boolean;
begin
  result := FShowingAbout;
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
    evGSAutoCenter:               FAutoCenter             := aNotice.tf;
    evGSIDD:                      FIDD                    := aNotice.integer;
    evGSImagesPaused:             FImagesPaused           := aNotice.tf;
    evGSMainForm:                 FMainForm               := aNotice.component as TForm;
    evGSMaxSize:                  FMaxSize                := aNotice.tf;
    evGSMediaType:                FMediaType              := aNotice.mediaType;
    evGSMPVScreenshotDirectory:   FMPVScreenshotDirectory := aNotice.text;
    evGSRepeatDelayMs:            FRepeatDelayMs          := aNotice.integer;
    evGSShowingAbout:             FShowingAbout           := aNotice.tf;
    evGSShowingHelp:              FShowingHelp            := aNotice.tf;
    evGSShowingPlaylist:          FShowingPlaylist        := aNotice.tf;
    evGSShowingStreamlist:        FShowingStreamlist      := aNotice.tf;
    evGSShowingThumbs:            FShowingThumbs          := aNotice.tf;
    evGSShowingTimeline:          FShowingTimeline        := aNotice.tf;
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
