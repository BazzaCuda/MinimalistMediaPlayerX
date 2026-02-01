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
unit model.mmpUndoMove;

interface

uses
  system.generics.collections,
  mmpAction;

type
  IUndoMove = interface
    function recordUndo(const aSrcFilePath: string; const aDstFilePath: string): TVoid;
    function undoPop(var aSrcFilePath: string; var aDstFilePath: string): boolean;
  end;

  TUndoRec = class(TObject)
    urSrcFilePath: string;
    urDstFilePath: string;
  end;

  TUndoMove = class(TInterfacedObject, IUndoMove)
  strict private
    FUndoRecs: TObjectStack<TUndoRec>;
  protected
  public
    constructor Create;
    destructor  Destroy; override;
    function    recordUndo(const aSrcFilePath: string; const aDstFilePath: string): TVoid;
    function    undoPop(var aSrcFilePath: string; var aDstFilePath: string): boolean;
    property    undoRecs: TObjectStack<TUndoRec> read FUndoRecs;
  end;

function UM: IUndoMove;

implementation

uses
  mmpConsts,
  _debugWindow;

function UM: IUndoMove;
{$J+} const gUM: IUndoMove = NIL; {$J-}
begin
  case gUM = NIL of TRUE: gUM := TUndoMove.create; end;
  result := gUM;
end;

{ TUndoMove }

constructor TUndoMove.Create;
begin
  inherited;
  FUndoRecs := TObjectStack<TUndoRec>.create;
  FUndoRecs.OwnsObjects := TRUE;
end;

destructor TUndoMove.Destroy;
begin
  case FUndoRecs <> NIL of TRUE: FUndoRecs.free; end;
  inherited;
end;

function TUndoMove.recordUndo(const aSrcFilePath: string; const aDstFilePath: string): TVoid;
begin
  var vUndoRec           := TUndoRec.create;
  vUndoRec.urSrcFilePath := aSrcFilePath;
  vUndoRec.urDstFilePath := aDstFilePath;
  FUndoRecs.push(vUndoRec);
end;

function TUndoMove.undoPop(var aSrcFilePath: string; var aDstFilePath: string): boolean;
begin
  aSrcFilePath  := EMPTY;
  aDstFilePath  := EMPTY;
  result        := FALSE;
  case FUndoRecs.count = 0 of TRUE: EXIT; end;

  var vUndoRec  := FUndoRecs.extract;
  aSrcFilePath  := vUndoRec.urDstFilePath;
  aDstFilePath  := vUndoRec.urSrcFilePath;
  vUndoRec.free;

  result        := TRUE;
end;

end.
