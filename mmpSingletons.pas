{   MMP: Minimalist Media Player
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
unit mmpSingletons;

interface

uses
  formCaptions, formMediaCaption,
  TBookmarkClass, TConfigFileClass, TGlobalVarsClass, TMediaInfoClass, TMediaPlayerClass, TMediaTypesClass, TParamStringsClass, TPlaylistClass, TProgramUpdatesClass, TProgressBarClass, TSendAllClass,
  TUICtrlsClass, TUndoMoveClass;

function BM: TBookmark;
function CF: TConfigFile;
function GV: TGlobalVars;
function MC: TMediaCaptionForm;
function MI: TMediaInfo;
function MP: TMediaPlayer;
function MT: TMediaTypes;
function PB: TProgressBar;
function PL: TPlaylist;
function PS: TPS;
function PU: TProgramUpdates;
function SA: TSendAll;
function ST: TCaptionsForm; // was originally for SubTitles, hence ST
function UI: TUI;
function UM: TUndoMove;

implementation

var
  gBM: TBookmark;
  gCF: TConfigFile;
  gGV: TGlobalVars;
  gMC: TMediaCaptionForm;
  gMI: TMediaInfo;
  gMP: TMediaPlayer;
  gMT: TMediaTypes;
  gPB: TProgressBar;
  gPL: TPlaylist;
  gPS: TPS;
  gPU: TProgramUpdates;
  gSA: TSendAll;
  gST: TCaptionsForm;
  gUI: TUI;
  gUM: TUndoMove;

function BM: TBookmark;
begin
  case gBM = NIL of TRUE: gBM := TBookmark.Create; end;
  result := gBM;
end;

function CF: TConfigFile;
begin
  case gCF = NIL of TRUE: gCF := TConfigFile.create; end;
  result := gCF;
end;

function GV: TGlobalVars;
begin
  case gGV = NIL of TRUE: gGV := TGlobalVars.create; end;
  result := gGV;
end;

function MC: TMediaCaptionForm;
begin
  case gMC = NIL of TRUE: gMC := TMediaCaptionForm.create; end;
  result := gMC;
end;

function MI: TMediaInfo;
begin
  case gMI = NIL of TRUE: gMI := TMediaInfo.create; end;
  result := gMI;
end;

function MP: TMediaPlayer;
begin
  case gMP = NIL of TRUE: gMP := TMediaPlayer.create; end;
  result := gMP;
end;

function MT: TMediaTypes;
begin
  case gMT = NIL of TRUE: gMT := TMediaTypes.Create; end;
  result := gMT;
end;

function PB: TProgressBar;
begin
  case gPB = NIL of TRUE: gPB := TProgressBar.create; end;
  result := gPB;
end;

function PL: TPlaylist;
begin
  case gPL = NIL of TRUE: gPL := TPlaylist.create; end;
  result := gPL;
end;

function PS: TPS;
begin
  case gPS = NIL of TRUE: gPS := TPS.create; end;
  result := gPS;
end;

function PU: TProgramUpdates;
begin
  case gPU = NIL of TRUE: gPU := TProgramUpdates.create; end;
  result := gPU;
end;

function SA: TSendAll;
begin
  case gSA = NIL of TRUE: gSA := TSendAll.Create; end;
  result := gSA;
end;

function ST: TCaptionsForm;
begin
  case gST = NIL of TRUE: gST := TCaptionsForm.create; end;
  result := gST;
end;

function UI: TUI;
begin
  case gUI = NIL of TRUE: gUI := TUI.create; end;
  result := gUI;
end;

function UM: TUndoMove;
begin
  case gUM = NIL of TRUE: gUM := TUndoMove.create; end;
  result := gUM;
end;

initialization
  gBM := NIL;
  gCF := NIL;
  gGV := NIL;
  gMC := NIL;
  gMI := NIL;
  gMP := NIL;
  gMT := NIL;
  gPB := NIL;
  gPL := NIL;
  gPS := NIL;
  gPU := NIL;
  gSA := NIL;
  gST := NIL;
  gUI := NIL;
  gUM := NIL;

finalization
  case gBM <> NIL of TRUE: gBM.free; end;
  case gCF <> NIL of TRUE: gCF.free; end;
  case gGV <> NIL of TRUE: gGV.free; end;
//  case gMC <> NIL of TRUE: gMC.free; end; // owned by GV.mainForm
  case gMI <> NIL of TRUE: gMI.free; end;
  case gMP <> NIL of TRUE: gMP.free; end;
  case gMT <> NIL of TRUE: gMT.free; end;
  case gPB <> NIL of TRUE: gPB.free; end;
  case gPL <> NIL of TRUE: gPL.free; end;
  case gPS <> NIL of TRUE: gPS.free; end;
  case gPU <> NIL of TRUE: gPU.free; end;
  case gSA <> NIL of TRUE: gSA.free; end;
//  case gST <> NIL of TRUE: gST.free; end; // owned by GV.mainForm
  case gUI <> NIL of TRUE: gUI.free; end;
  case gUM <> NIL of TRUE: gUM.free; end;

end.
