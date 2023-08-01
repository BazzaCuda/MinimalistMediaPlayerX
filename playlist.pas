{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit playlist;

interface

uses system.generics.collections, system.generics.defaults, system.classes;

type
  TPlaylist = class(TObject)
  strict private
    FPlayIx: integer;
    FPlaylist: TList<string>;
  private
    constructor create;
    destructor  Destroy; override;
    function extractNumericPart(const S: string): Integer;
  public
    function add(anItem: string): boolean;
    function clear: boolean;
    function count: integer;
    function copyToClipboard: boolean;
    function currentItem: string;
    function currentIx: integer;
    function delete(anIx: integer = -1): boolean;
    function displayItem: string;
    function find(anItem: string): boolean;
    function first: boolean;
    function formattedItem: string;
    function hasItems: boolean;
    function isFirst: boolean;
    function isLast: boolean;
    function last: boolean;
    function next: boolean;
    function prev: boolean;
    function replaceCurrentItem(aNewItem: string): boolean;
    function sort: boolean;
    function thisItem(anIx: integer): string;
    function validIx(anIx: integer): boolean;
  end;

function PL: TPlaylist;

var
  gPL: TPlaylist;

implementation

uses
  system.sysUtils, sysCommands, globalVars, clipbrd, formSubtitles, commonUtils, regularExpressions, math;

function PL: TPlaylist;
begin
  case gPL = NIL of TRUE: gPL := TPlaylist.create; end;
  result := gPL;
end;

{ TPlaylist }

function TPlaylist.add(anItem: string): boolean;
begin
  FPlayList.add(anItem);
//  FPlayIx := FPlayList.count - 1; // NO!
end;

function TPlaylist.clear: boolean;
begin
  FPlaylist.clear;
  FPlayIx := -1;
end;

function TPlaylist.copyToClipboard: boolean;
begin
  result := FALSE;
  clipboard.AsText := CU.getFileNameWithoutExtension(currentItem);
  ST.opInfo := 'Copied to clipboard';
  result := TRUE;
end;

function TPlaylist.count: integer;
begin
  result := FPlaylist.count;
end;

constructor TPlaylist.create;
begin
  inherited;
  FPlaylist := TList<string>.create;
  FPLaylist.sort;
end;

function TPlaylist.currentItem: string;
begin
  result := '';
  case FPlayIx = -1 of TRUE: EXIT; end;
  result := FPlaylist[FPlayIx];
end;

function TPlaylist.currentIx: integer;
begin
  result := FPlayIx;
end;

function TPlaylist.delete(anIx: integer = -1): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case anIx = -1 of  TRUE:  begin
                              FPlaylist.delete(FPlayIx);
                              dec(FPlayIx); end;
                    FALSE:  begin
                              case (anIx < 0) or (anIx > FPlaylist.count - 1) of TRUE: EXIT; end;
                              FPlaylist.delete(anIx);
                              dec(FPlayIx); end;end;
  result := TRUE;
end;

destructor TPlaylist.Destroy;
begin
  case FPlaylist <> NIL of TRUE: FPlaylist.free; end;
  inherited;
end;

function TPlaylist.displayItem: string;
begin
  result := format('[%d/%d] %s', [FPlayIx, count, extractFileName(currentItem)]);
end;

function TPlaylist.find(anItem: string): boolean;
begin
  FPlayIx := FPlaylist.indexOf(anItem);
  result  := FPlayIx <> -1;
end;

function TPlaylist.first: boolean;
begin
  result := FALSE;
  case hasItems of TRUE: FPlayIx := 0; end;
  result := TRUE;
end;

function TPlaylist.formattedItem: string;
begin
  case hasItems of FALSE: EXIT; end;
  result := format('[%d/%d] %s', [FPlayIx + 1, FPlaylist.count, ExtractFileName(currentItem)]);
end;

function TPlaylist.hasItems: boolean;
begin
  result := FPlaylist.count > 0;
end;

function TPlaylist.isFirst: boolean;
begin
  result := FPlayIx = 0;
end;

function TPlaylist.isLast: boolean;
begin
  result := FPlayIx = FPlaylist.count - 1;
end;

function TPlaylist.last: boolean;
begin
  result := FALSE;
  case hasItems of TRUE: FPlayIx := FPlaylist.count - 1; end;
  result := TRUE;
end;

function TPlaylist.next: boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case isLast of TRUE: EXIT; end;
  inc(FPlayIx);
  result := TRUE;
end;

function TPlaylist.prev: boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case isFirst of TRUE: EXIT; end;
  dec(FPlayIx);
  result := TRUE;
end;

function TPlaylist.replaceCurrentItem(aNewItem: string): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  FPlaylist[FPlayIx] := aNewItem;
  result := TRUE;
end;

function TPlaylist.extractNumericPart(const S: string): Integer;
var
  Match: TMatch;
begin
  // Use a regular expression to extract the numeric part from the string
  Match := TRegEx.Match(S, '\d+');
  if Match.Success then
    Result := StrToIntDef(Match.Value, 0)
  else
    Result := 0;
end;

function compareStr(Str1,Str2: string):Integer;
// implements "Natural Sort Order" - author: dinilud 16/01/2008 on experts-exchange.com
var Num1,Num2:Double;
    pStr1,pStr2:PChar;
  Function IsNumber(ch:Char):Boolean;
  begin
     Result:=ch in ['0'..'9'];
  end;
  Function GetNumber(var pch:PChar):Double;
    var FoundPeriod:Boolean;
        Count:Integer;
  begin
     FoundPeriod:=False;
     Result:=0;
     While (pch^<>#0) and (IsNumber(pch^) or ((NOT FoundPeriod) and (pch^='.'))) do
     begin
        if pch^='.' then
        begin
          FoundPeriod:=True;
          Count:=0;
        end
        else
        begin
           if FoundPeriod then
           begin
             Inc(Count);
             Result:=Result+(ord(pch^)-ord('0'))*Power(10,-Count);
           end
           else Result:=Result*10+ord(pch^)-ord('0');
        end;
        Inc(pch);
     end;
  end;
begin
    pStr1:=@Str1[1]; pStr2:=@Str2[1];
    Result:=0;
    While NOT ((pStr1^=#0) or (pStr2^=#0)) do
    begin
       if IsNumber(pStr1^) and IsNumber(pStr2^) then
       begin
          Num1:=GetNumber(pStr1); Num2:=GetNumber(pStr2);
          if Num1<Num2 then Result:=-1
          else if Num1>Num2 then Result:=1;
          Dec(pStr1);Dec(pStr2);
       end
       else if pStr1^<>pStr2^ then
       begin
          if pStr1^<pStr2^ then Result:=-1 else Result:=1;
       end;
       if Result<>0 then Break;
       Inc(pStr1); Inc(pStr2);
    end;
    Num1:=length(Str1); Num2:= length(Str2);
    if (Result=0) and (Num1<>Num2) then
    begin
       if Num1<Num2 then Result:=-1 else Result:=1;
    end;
end;

function TPlaylist.sort: boolean;
begin
  result := FALSE;

  FPlaylist.Sort(
                 TComparer<string>.construct(
                                              function(const a, b: string): integer
                                              begin
                                                result := compareStr(CU.getFileNameWithoutExtension(a), CU.getFileNameWithoutExtension(b));
                                              end
                                            )
                );
  result := TRUE;
end;

function TPlaylist.thisItem(anIx: integer): string;
begin
  result := '';
  case hasItems of FALSE: EXIT; end;
  case (anIx < 0) or (anIx > FPlaylist.count - 1) of TRUE: EXIT; end;
  result := FPlaylist[anIx];
end;

function TPlaylist.validIx(anIx: integer): boolean;
begin
  result := FALSE;
  case hasItems of FALSE: EXIT; end;
  case (anIx < 0) or (anIx > FPlaylist.count - 1) of TRUE: EXIT; end;
  result := TRUE;
end;

initialization
  gPL := NIL;

finalization
  case gPL <> NIL of TRUE: gPL.free; end;

end.
