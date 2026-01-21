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
unit mmpExporter;

interface

function mmpExport(const aMediaFilePath: string): boolean;

implementation

uses
  mmpExportExec,
  TSegmentClass;

type
  IExporter = interface
    function exportEdits: boolean;
  end;

  TExporter = class(TInterfacedObject, IExporter)
  strict private
    FMediaFilePath: string;
  public
    constructor Create(const aMediaFilePath: string);
    function exportEdits: boolean;
  end;

function mmpExport(const aMediaFilePath: string): boolean;
begin
  var vExporter := TExporter.create(aMediaFilePath);
  result := vExporter.exportEdits;
end;


{ TExporter }

constructor TExporter.Create(const aMediaFilePath: string);
begin
  inherited Create;

  FMediaFilePath := aMediaFilePath;
end;

function TExporter.exportEdits: boolean;
begin


end;

end.
