{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit consts;

interface

uses
  winApi.messages;

const
  MENU_ABOUT_ID   = WM_USER + 2001;
  MENU_HELP_ID    = WM_USER + 2002;
  WIN_CLOSEAPP    = WM_USER + 2003;
  WIN_RESIZE      = WM_USER + 2004;
  WIN_POSITION    = WM_USER + 2005;
  WIN_CONTROLS    = WM_USER + 2006;
  WIN_RESTART     = WM_USER + 2007;
  WIN_TAB         = WM_USER + 2008;
  WIN_CAPTION     = WM_USER + 2009;
  WIN_PAUSE_PLAY  = WM_USER + 2010;

  WM_PARENTCHANGED = WM_USER + 1023;
  WM_TIMERUPDATE   = WM_USER + 1024;


implementation

end.
