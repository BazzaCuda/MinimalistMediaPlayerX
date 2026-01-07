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

  TFuncNoParam<TResult>       = function: TResult of object;
  TFuncString<TResult>        = function(const aValue: string): TResult of object;
  TFuncInteger<TResult>       = function(const aValue: Integer): TResult of object;
  TFuncStringInteger<TResult> = function(const aString: string; const aInteger: Integer): TResult of object;
  TVoid                       = record end;

type
  IAction<TResult> = interface
    function perform: TResult; overload;
    function perform(const aValue: string): TResult; overload;
    function perform(const aValue: integer): TResult; overload;
    function perform(const aString: string; const aInteger: integer): TResult; overload;
  end;

  TAction<TResult> = class(TInterfacedObject, IAction<TResult>)
  strict private
    FFuncNoParam:       TFuncNoParam<TResult>;
    FFuncString:        TFuncString<TResult>;
    FFuncInteger:       TFuncInteger<TResult>;
    FFuncStringInteger: TFuncStringInteger<TResult>;
  public
    function setFunc(const aFunc: TFuncNoParam<TResult>):       TAction<TResult>; overload;
    function setFunc(const aFunc: TFuncString<TResult>):        TAction<TResult>; overload;
    function setFunc(const aFunc: TFuncInteger<TResult>):       TAction<TResult>; overload;
    function setFunc(const aFunc: TFuncStringInteger<TResult>): TAction<TResult>; overload;

    function perform: TResult; overload;
    function perform(const aValue: string): TResult; overload;
    function perform(const aValue: integer): TResult; overload;
    function perform(const aString: string; const aInteger: integer): TResult; overload;
  end;

implementation

function TAction<TResult>.setFunc(const aFunc: TFuncNoParam<TResult>): TAction<TResult>;
begin
  result := SELF;
  FFuncNoParam := aFunc;
end;

function TAction<TResult>.setFunc(const aFunc: TFuncString<TResult>): TAction<TResult>;
begin
  result := SELF;
  FFuncString := aFunc;
end;

function TAction<TResult>.setFunc(const aFunc: TFuncInteger<TResult>): TAction<TResult>;
begin
  result := SELF;
  FFuncInteger := aFunc;
end;

function TAction<TResult>.setFunc(const aFunc: TFuncStringInteger<TResult>): TAction<TResult>;
begin
  result := SELF;
  FFuncStringInteger := aFunc;
end;

function TAction<TResult>.perform: TResult;
begin
  case assigned(FFuncNoParam) of
    TRUE:  result := FFuncNoParam();
    FALSE: result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aValue: string): TResult;
begin
  case assigned(FFuncString) of
    TRUE:  result := FFuncString(aValue);
    FALSE: result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aValue: Integer): TResult;
begin
  case assigned(FFuncInteger) of
    TRUE:  result := FFuncInteger(aValue);
    FALSE: result := default(TResult);
  end;
end;

function TAction<TResult>.perform(const aString: string; const aInteger: Integer): TResult;
begin
  case assigned(FFuncStringInteger) of
    TRUE:  result := FFuncStringInteger(aString, aInteger);
    FALSE: result := default(TResult);
  end;
end;


end.
