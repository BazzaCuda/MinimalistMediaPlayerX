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
unit TThumbClass;

interface

uses
  system.classes,
  vcl.controls, vcl.extCtrls;

type
  IThumb = interface
    ['{AE24966D-C533-4F73-84D9-B095706CB665}']

    function  getOnThumbClick: TNotifyEvent;
    procedure setOnThumbClick(const Value: TNotifyEvent);
    function  getThumbHint: string;
    procedure setThumbHint(const Value: string);
    function  getThumbLeft: integer;
    procedure setThumbLeft(const Value: integer);
    function  getThumbParent: TWinControl;
    procedure setThumbParent(const Value: TWinControl);
    function  getThumbTag: integer;
    procedure setThumbTag(const Value: integer);
    function  getThumbTop: integer;
    procedure setThumbTop(const Value: integer);

    property onThumbClick: TNotifyEvent read getOnThumbClick write setOnThumbClick;
    property thumbHint: string read getThumbHint write setThumbHint;
    property thumbLeft: integer read getThumbLeft write setThumbLeft;
    property thumbParent: TWinControl read getThumbParent write setThumbParent;
    property thumbTag: integer read getThumbTag write setThumbTag;
    property thumbTop: integer read getThumbTop write setThumbTop;
  end;

function newThumb(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120): IThumb;

implementation

uses
  mmpThumbUtils;

type
  TThumb = class(TInterfacedObject, IThumb)
  strict private
    FThumb: TImage;
  private
    function  getThumbTop: integer;
    procedure setThumbTop(const Value: integer);
    function  getThumbLeft: integer;
    procedure setThumbLeft(const Value: integer);
    function  getThumbTag: integer;
    procedure setThumbTag(const Value: integer);
    function  getOnThumbClick: TNotifyEvent;
    procedure setOnThumbClick(const Value: TNotifyEvent);
    function  getThumbHint: string;
    procedure setThumbHint(const Value: string);
    function  getThumbParent: TWinControl;
    procedure setThumbParent(const Value: TWinControl);
  public
    constructor create(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
    destructor  Destroy; override;

    property onThumbClick: TNotifyEvent read getOnThumbClick write setOnThumbClick;
    property thumbHint: string read getThumbHint write setThumbHint;
    property thumbLeft: integer read getThumbLeft write setThumbLeft;
    property thumbParent: TWinControl read getThumbParent write setThumbParent;
    property thumbTag: integer read getThumbTag write setThumbTag;
    property thumbTop: integer read getThumbTop write setThumbTop;
  end;

function newThumb(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120): IThumb;
begin
  result := TThumb.create(aFilePath, aDesiredWidth, aDesiredHeight);
end;


{ TThumb }

constructor TThumb.create(const aFilePath: string; const aDesiredWidth: integer = 120; const aDesiredHeight: integer = 120);
begin
  inherited create;

  FThumb := TImage.create(NIL);

  FThumb.width   := aDesiredWidth;
  FThumb.height  := aDesiredHeight;
  FThumb.stretch := TRUE;

  mmpExtractThumb(FThumb.picture.bitmap, aFilePath, aDesiredWidth, aDesiredHeight);
end;

destructor TThumb.Destroy;
begin
  case FThumb = NIL of FALSE: FThumb.free; end;
  inherited;
end;

function TThumb.getOnThumbClick: TNotifyEvent;
begin
  result := FThumb.onClick;
end;

function TThumb.getThumbHint: string;
begin
  result := FThumb.hint;
end;

function TThumb.getThumbLeft: integer;
begin
  result := FThumb.left;
end;

function TThumb.getThumbParent: TWinControl;
begin
  result := FThumb.parent;
end;

function TThumb.getThumbTag: integer;
begin
  result := FThumb.tag;
end;

function TThumb.getThumbTop: integer;
begin
  result := FThumb.top;
end;

procedure TThumb.setOnThumbClick(const Value: TNotifyEvent);
begin
  FThumb.onClick := value;
end;

procedure TThumb.setThumbHint(const Value: string);
begin
  FThumb.hint := value;
end;

procedure TThumb.setThumbLeft(const Value: integer);
begin
  FThumb.left := value;
end;

procedure TThumb.setThumbParent(const Value: TWinControl);
begin
  FThumb.parent := value;
end;

procedure TThumb.setThumbTag(const Value: integer);
begin
  FThumb.tag := value;
end;

procedure TThumb.setThumbTop(const Value: integer);
begin
  FThumb.top := value;
end;

end.
