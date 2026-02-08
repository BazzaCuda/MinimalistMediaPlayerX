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

uses
  bazAction, bazFuncDefs;

type
  TVoid = bazFuncDefs.TVoid;
//
//  TFunc<T> = reference to function: T;
//  TAFuncNoParam             <TResult> = reference to function():                                                        TResult;
//  TAFuncString              <TResult> = reference to function(const aString: string):                                   TResult;
//  TAFuncInteger             <TResult> = reference to function(const aInteger: integer):                                 TResult;
//  TAFuncIntegerString       <TResult> = reference to function(const aInteger: integer; const aString: string):          TResult;
//  TAFuncStringInteger       <TResult> = reference to function(const aString: string; const aInteger: integer):          TResult;
//  TAFuncBoolean             <TResult> = reference to function(const aBoolean: boolean):                                 TResult;
//  TAFuncWord                <TResult> = reference to function(const aWORD: WORD):                                       TResult;
//  TAFuncCardinal            <TResult> = reference to function(const aCardinal: cardinal):                               TResult;
//  TAFuncStringString        <TResult> = reference to function(const aString1: string; const aString2: string):          TResult;
//  TAFuncStringBoolean       <TResult> = reference to function(const aString: string; const aBoolean: boolean):          TResult;

  TAction<TResult> = class(bazAction.TAction<TResult>)
  end;

  TAction = class(bazAction.TAction)
  end;

implementation

end.
