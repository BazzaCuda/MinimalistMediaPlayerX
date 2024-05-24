{   Minimalist Media Player
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
unit TThumbsClass;

interface

uses
  vcl.extCtrls, vcl.forms;

type
  TThumbs = class(TObject)
  strict private
    FPanel: TPanel;
  public
    constructor create;
    destructor destroy; override;
    function initThumbnails(const aFilePath: string; const aThumbsHost: TPanel): boolean;
  end;

//function TN: TThumbnails;

implementation

uses
  vcl.controls, vcl.graphics;

//var
//  gTN: TThumbnails;

//function TN: TThumbnails;
//begin
//  case gTN = NIL of TRUE: gTN := TThumbnails.create; end;
//  result := gTN;
//end;

{ TThumbnails }

constructor TThumbs.create;
begin
  inherited;
  FPanel := TPanel.create(NIL);
  FPanel.styleElements := [];
end;

destructor TThumbs.destroy;
begin
  case FPanel = NIL of FALSE: FPanel.free; end;
  inherited;
end;

function TThumbs.initThumbnails(const aFilePath: string; const aThumbsHost: TPanel): boolean;
begin

end;

//initialization
//  gTN := NIL;

//finalization
//  case gTN <> NIL of TRUE: gTN.free; end;

end.
