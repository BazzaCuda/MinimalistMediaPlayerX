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
unit mmpUtils;

interface

uses
  winApi.windows,
  system.classes,
  vcl.forms,
  mmpAction, mmpConsts;

function mmpCancelDelay: boolean;
function mmpCompareStr(const aStr1: string; const aStr2: string): integer;
function mmpDelay(const dwMilliseconds: cardinal): TVoid;
function mmpFnnKeyAppToString(const aFnnKeyApp: TFnnKeyApp): string;
function mmpIfThenElse(const bBoolean: boolean; aTrue: string;  aFalse: string): string; overload;
function mmpIfThenElse(const bBoolean: boolean; aTrue: integer; aFalse: integer): integer; overload;
function mmpPlaylistFolderContains(const aFilePath: string; const aSetOfMediaType: TSetOfMediaType = [mtUnk]): boolean;
function mmpProcessMessages: TVoid;
function mmpQuoted(const aString: string): string;
function mmpWrapText(const aText: string; const aTextWidth: integer; const aMaxWidth: integer; bFileName: boolean = FALSE): string;

implementation

uses
  system.math, system.sysUtils,
  bazCmd,
  model.mmpPlaylist,
  _debugWindow;

var
  gCancel: boolean = FALSE;

function mmpCancelDelay: boolean;
begin
  gCancel := TRUE;
  result := gCancel;
end;

function mmpPlaylistFolderContains(const aFilePath: string; const aSetOfMediaType: TSetOfMediaType = [mtUnk]): boolean;
begin
  var vPlaylist := newPlaylist;
  vPlaylist.fillPlaylist(extractFilePath(aFilePath), aSetOfMediaType);
  result := vPlaylist.hasItems;
end;

function mmpCompareStr(const aStr1: string; const aStr2: string): integer;
// implements "Natural Sort Order" - author: dinilud 16/01/2008 on experts-exchange.com
// normal sort = image12.jpg, image2.jpg, image20.jpg
// natural sort = image2.jpg, image12.jpg, image20.jpg
var vNum1,
    vNum2:  double;
    pStr1,
    pStr2:  PChar;

  function isDigit(ch: char):Boolean;
  begin
    result := charInSet(ch, ['0'..'9']);
  end;

  function getNumber(var pch: PChar): double;
  var
    vFoundPeriod: boolean;
    vCount:       integer;
  begin
    vFoundPeriod := FALSE;
    result := 0;
    vCount := 0;

    while (pch^ <> #0) and (isDigit(pch^) or ((NOT vFoundPeriod) and (pch^ = '.'))) do
    begin
      case pch^ = '.' of   TRUE:  begin
                                    vFoundPeriod  := TRUE;
                                    vCount        := 0;
                                  end;
                          FALSE:  begin
                                    case vFoundPeriod of   TRUE:  begin
                                                                    inc(vCount);
                                                                    result := result + (ord(pch^) - ord('0')) * power(10, -vCount);
                                                                  end;
                                                          FALSE:  result := result * 10 + ord(pch^) - ord('0'); end;
                                  end;end;
      inc(pch);
    end;
  end;

begin
  case (length(aStr1) = 0) or (length(aStr2) = 0) of TRUE: begin result := -1; EXIT; end;end;  // e.g. files with just an extension and no name

  result := 0;
  pStr1 := @aStr1[1]; pStr2 := @aStr2[1];

  while NOT ((pStr1^ = #0) or (pStr2^ = #0)) do
  begin

     case isDigit(pStr1^) and isDigit(pStr2^) of     TRUE:  begin
                                                              vNum1 := getNumber(pStr1); vNum2 := getNumber(pStr2);
                                                              case vNum1 < vNum2 of TRUE: result := -1; end;
                                                              case vNum1 > vNum2 of TRUE: result :=  1; end;
                                                              dec(pStr1); dec(pStr2);
                                                            end;
                                                    FALSE:  begin
                                                              case pStr1^ < pStr2^ of  TRUE: result := -1; end;
                                                              case pStr1^ > pStr2^ of  TRUE: result :=  1; end;
                                                            end;end;

     case result <> 0 of TRUE: BREAK; end;
     inc(pStr1); inc(pStr2);
  end;

  vNum1 := length(aStr1); vNum2 := length(aStr2);
  case (result = 0) and (vNum1 <> vNum2) of TRUE: case vNum1 < vNum2 of  TRUE: result := -1;
                                                                        FALSE: result :=  1; end;end;
end;

function mmpDelay(const dwMilliseconds: cardinal): TVoid;
// Used to delay an operation; "sleep()" would suspend the thread, which is not what is required
var
  iStart, iStop: cardinal;
begin
  gCancel := FALSE;
  iStart  := getTickCount;
  repeat
    iStop := getTickCount;
    mmpProcessMessages;
  until gCancel or ((iStop  -  iStart) >= dwMilliseconds);
end;

function mmpFnnKeyAppToString(const aFnnKeyApp: TFnnKeyApp): string;
begin
  result := FnnKeyApps[aFnnKeyApp];
end;

function mmpIfThenElse(const bBoolean: boolean; aTrue: string; aFalse: string): string;
begin
  case bBoolean of   TRUE: result := aTrue;
                    FALSE: result := aFalse; end;
end;

function mmpIfThenElse(const bBoolean: boolean; aTrue: integer; aFalse: integer): integer; overload;
begin
  case bBoolean of   TRUE: result := aTrue;
                    FALSE: result := aFalse; end;
end;

function mmpProcessMessages: TVoid;
begin
  application.processMessages;
end;

function mmpQuoted(const aString: string): string;
begin
  result := format('"%s"', [aString]);
end;

function mmpWrapText(const aText: string; const aTextWidth: integer; const aMaxWidth: integer; bFileName: boolean = FALSE): string;
var TF, FF: TSFunc;
begin
  TF := function:string begin
                          var vChop := length(aText) div 2;
                          repeat
                            inc(vChop);
                          until bFileName or (vChop > length(aText)) or (aText[vChop] = '\');
                          var vLine1 := copy(aText, 1, vChop) + '...';
                          var vBackslash:string := '\';
                          case bFileName of TRUE: vBackSlash := EMPTY; end;
                          var vLine2 := '...' + vBackslash + copy(aText, vChop + 1);
                          result := vLine1 + #13#10 + vLine2; end;

  FF := function:string begin result := aText; end;

  result := mmp.cmd(aTextWidth > aMaxWidth, TF, FF);
end;


end.
