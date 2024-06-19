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
unit TGlobalVarsClass;

interface

uses
  winAPI.windows,
  vcl.forms;

type
  TGlobalVars = class(TObject)
  strict private
    FAppWnd:            HWND;
    FAltKeyDown:        boolean;
    FAutoCentre:        boolean;
    FCloseApp:          boolean;
    FMainForm:          TForm;
    FMaxSize:           boolean;
    FPlayingSlideshow:  boolean;
    FShowingHelp:       boolean;
    FShowingPlaylist:   boolean;
    FShowingStreamList: boolean;
    FShowingThumbs:     boolean;
    FShowingTimeline:   boolean;
    FTimelineHeight:    integer;
    FUserInput:         boolean;
  private
  public
    property altKeyDown:        boolean read FAltKeyDown        write FAltKeyDown;
    property appWnd:            HWND    read FAppWnd            write FAppWnd;
    property autoCentre:        boolean read FAutoCentre        write FAutoCentre;
    property closeApp:          boolean read FCloseApp          write FCloseApp;
    property mainForm:          TForm   read FMainForm          write FMainForm;
    property maxSize:           boolean read FMaxSize           write FMaxSize;
    property playingSlideshow:  boolean read FPlayingSlideshow  write FPlayingSlideshow;
    property showingHelp:       boolean read FShowingHelp       write FShowingHelp;
    property showingPlaylist:   boolean read FShowingPlaylist   write FShowingPlaylist;
    property showingStreamList: boolean read FShowingStreamList write FShowingStreamList;
    property showingThumbs:     boolean read FShowingThumbs     write FShowingThumbs;
    property showingTimeline:   boolean read FShowingTimeline   write FShowingTimeline;
    property timelineHeight:    integer read FTimelineHeight    write FTimelineHeight;
    property userInput:         boolean read FUserInput         write FUserInput;
  end;

implementation

end.
