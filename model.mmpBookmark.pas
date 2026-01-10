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
unit model.mmpBookmark;

interface

uses
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  IBookmark = interface
    function delete   (const aURL: string): string;
    function position (const aURL: string): integer;
    function save     (const aURL: string; const aPosition: integer): string;
  end;

function newBookmark: IBookmark;

implementation

uses
  system.sysUtils,
  bazCmd,
  model.mmpConfigFile,
  _debugWindow;

type
  TBookmark = class(TInterfacedObject, IBookmark)
  strict private
  protected
  private
  public
    function delete   (const aURL: string): string;
    function position (const aURL: string): integer;
    function save     (const aURL: string; const aPosition: integer): string;
  end;

function newBookmark: IBookmark;
begin
  result := TBookmark.create;
end;

{ TBookmark }

function TBookmark.delete(const aURL: string): string;
begin
  CF.deleteConfig(aURL);
  result := 'Bookmark deleted';
  mmp.cmd(evSTOpInfo, result);
end;

function TBookmark.position(const aURL: string): integer;
begin
  result := CF.asInteger[aURL];
  mmp.cmd(evSTOpInfo, 'From bookmark');
end;

function TBookmark.save(const aURL: string; const aPosition: integer): string;
begin
  CF[aURL]  := intToStr(aPosition);
  result    := 'Bookmarked';
  mmp.cmd(evSTOpInfo, result);
end;

end.
