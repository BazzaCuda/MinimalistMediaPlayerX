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
unit mmpAction;

interface

type
  TVoid = record end;

  TFuncNoParam          <TResult> = function:                                                      TResult of object;
  TFuncString           <TResult> = function(const aString: string):                               TResult of object;
  TFuncInteger          <TResult> = function(const aInteer: integer):                              TResult of object;
  TFuncStringInteger    <TResult> = function(const aString: string; const aInteger: Integer):      TResult of object;

  IAction<TResult> = interface
    function perform:                                                   TResult; overload;
    function perform(const aValue: string):                             TResult; overload;
    function perform(const aValue: Integer):                            TResult; overload;
    function perform(const aString: string; const aInteger: Integer):   TResult; overload;

    function getAssigned: boolean;
    property assigned:    boolean   read getAssigned;
  end;

  TAction<TResult> = class(TInterfacedObject, IAction<TResult>)
  strict private
    FFuncAssigned:      boolean;
    FFuncNoParam:       TFuncNoParam          <TResult>;
    FFuncString:        TFuncString           <TResult>;
    FFuncInteger:       TFuncInteger          <TResult>;
    FFuncStringInteger: TFuncStringInteger    <TResult>;

    constructor Create;                           overload;
    constructor Create(const aFuncNIL: pointer);  overload;

    constructor Create(const aFuncNoParam:        TFuncNoParam          <TResult>);     overload;
    constructor Create(const aFuncString:         TFuncString           <TResult>);     overload;
    constructor Create(const aFuncInteger:        TFuncInteger          <TResult>);     overload;
    constructor Create(const aFuncStringInteger:  TFuncStringInteger    <TResult>);     overload;
  public
    function perform: TResult; overload;
    function perform(const aString: string):      TResult; overload;
    function perform(const aInteger: Integer):    TResult; overload;
    function perform(const aString: string; const aInteger: integer): TResult; overload;

    function getAssigned: boolean;

    class function pick(const aBoolean: boolean; const aTrueFuncNoParam:        TFuncNoParam<TResult>):           IAction<TResult>; overload;
    class function pick(const aBoolean: boolean; const aTrueFuncString:         TFuncString<TResult>):            IAction<TResult>; overload;
    class function pick(const aBoolean: boolean; const aTrueFuncInteger:        TFuncInteger<TResult>):           IAction<TResult>; overload;
    class function pick(const aBoolean: boolean; const aTrueFuncStringInteger:  TFuncStringInteger<TResult>):     IAction<TResult>; overload;
  end;

implementation

uses
  mmpRTL;

{ TAction<TResult> }

constructor TAction<TResult>.Create(const aFuncNoParam: TFuncNoParam<TResult>);
begin
  FFuncNoParam  := aFuncNoParam;
  FFuncAssigned := assigned(aFuncNoParam);
end;

constructor TAction<TResult>.Create(const aFuncString: TFuncString<TResult>);
begin
  FFuncString   := aFuncString;
  FFuncAssigned := assigned(aFuncString);
end;

constructor TAction<TResult>.Create(const aFuncInteger: TFuncInteger<TResult>);
begin
  FFuncInteger  := aFuncInteger;
  FFuncAssigned := assigned(aFuncInteger);
end;

constructor TAction<TResult>.Create(const aFuncStringInteger: TFuncStringInteger<TResult>);
begin
  FFuncStringInteger  := aFuncStringInteger;
  FFuncAssigned       := assigned(aFuncStringInteger);
end;

constructor TAction<TResult>.Create;
begin
 raise exception.create('Don''t call TAction.create');
end;

constructor TAction<TResult>.Create(const aFuncNIL: pointer);
begin
  case aFuncNIL = NIL of   TRUE: EXIT;
                          FALSE: raise exception.Create('Functionless constructor must be called with NIL'); end;
end;

function TAction<TResult>.perform: TResult;
begin
  case assigned(FFuncNoParam) of
    TRUE:   result := FFuncNoParam();
    FALSE:  result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aString: string): TResult;
begin
  case assigned(FFuncString) of
    TRUE:   result := FFuncString(aString);
    FALSE:  result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aInteger: Integer): TResult;
begin
  case assigned(FFuncInteger) of
    TRUE:   result := FFuncInteger(aInteger);
    FALSE:  result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aString: string; const aInteger: Integer): TResult;
begin
  case assigned(FFuncStringInteger) of
    TRUE:   result := FFuncStringInteger(aString, aInteger);
    FALSE:  result := default(TResult);
  end;
end;

function TAction<TResult>.getAssigned: boolean;
begin
  result := FFuncAssigned;
end;

class function TAction<TResult>.pick(const aBoolean: boolean; const aTrueFuncNoParam: TFuncNoParam<TResult>): IAction<TResult>;
begin
  case aBoolean of
    TRUE:   result := TAction<TResult>.Create(aTrueFuncNoParam);
    FALSE:  result := TAction<TResult>.Create(NIL);
  end;
end;

class function TAction<TResult>.pick(const aBoolean: boolean; const aTrueFuncString: TFuncString<TResult>): IAction<TResult>;
begin
  case aBoolean of
    TRUE:   result := TAction<TResult>.Create(aTrueFuncString);
    FALSE:  result := TAction<TResult>.Create(NIL);
  end;
end;

class function TAction<TResult>.pick(const aBoolean: boolean; const aTrueFuncInteger: TFuncInteger<TResult>): IAction<TResult>;
begin
  case aBoolean of
    TRUE:   result := TAction<TResult>.Create(aTrueFuncInteger);
    FALSE:  result := TAction<TResult>.Create(NIL);
  end;
end;

class function TAction<TResult>.pick(const aBoolean: boolean; const aTrueFuncStringInteger: TFuncStringInteger<TResult>): IAction<TResult>;
begin
  case aBoolean of
    TRUE:   result := TAction<TResult>.Create(aTrueFuncStringInteger);
    FALSE:  result := TAction<TResult>.Create(NIL);
  end;
end;

end.
