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

  WM_PROGRESSBAR_CLICK   = WM_USER + 2011;
  WM_TICK                = WM_USER + 2012;
  WM_ADJUST_ASPECT_RATIO = WM_USER + 2013;
  WM_ADJUST_WINDOW_WIDTH = WM_USER + 2014;
  WM_CENTRE_WINDOW       = WM_USER + 2015;
  WM_KEY_UP              = WM_USER + 2016;

  POT_PLAYER   = 'C:\Program Files\DAUM\PotPlayer\PotPlayerMini64.exe';
  LOSSLESS_CUT = 'B:\Tools\LosslessCut-win-x64\LosslessCut.exe';
  SHOTCUT      = 'C:\Program Files\Shotcut\shotcut.exe';

  EXTS_1 = '.txt.ini.jpg.jpeg.bmp.ico.png.url.srt.sub.sami.ssa.xml.exe.html.mthml.htm.snz.jfif.gif.dll.pdf.doc.docx.xls.xlsx.jp2.ods.djvu.epub.zip.rar.7z.dic';
  EXTS_2 = '.rtf.md.bz2.arj.cab.gz.iso.jar.lz.lzh.tar.uue.xz.z.zipx.zst.cbr.cbz.pl.py.pas.dfm.scpt.dmg.plist.css.body.offsets.index.data.css.dist.bom.slip.msg.bat.go';
  EXTS_3 = '.conf.sh.dpk.res.dfm.identcache.local.dcu.yml.csv.sub';
  EXTS_FILTER = EXTS_1 + EXTS_2 + EXTS_3 + '.';


implementation

end.
