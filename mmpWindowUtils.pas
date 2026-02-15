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
unit mmpWindowUtils;

interface

uses
  winApi.windows,
  system.classes, system.SyncObjs,
  vcl.forms,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpAction, mmpConsts, mmpGlobalState;

function mmpAdjustAspectRatio (const aWND: HWND; const aHeight: integer): TPoint;
function mmpAnimateResize(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aCenter: boolean; const aDurationMs: integer): TVoid; overload;
function mmpAnimateResize(const aWND: HWND;         const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aCenter: boolean; const aDurationMs: integer): TVoid; overload;
function mmpArrangeAll        (const aWND: HWND): boolean;
function mmpCalcGreaterWindow (const aWND: HWND; const aShiftState: TShiftState; const aThumbSize: integer; const aHostType: THostType): TPoint;
function mmpCalcWindowSize    (const aStartingHeight: integer; const bMaxSize: boolean): TPoint;
function mmpCenterWindow      (const aWND: HWND; const aPt: TPoint; const aFlags: Cardinal = SWP_NOSIZE): TVoid;
function mmpGreaterWindow     (const aWND: HWND; aShiftState: TShiftState): integer; overload;
function mmpGreaterWindow     (const aWND: HWND; const aShiftState: TShiftState; const aThumbSize: integer; const aHostType: THostType): TVoid; overload;
function mmpPosWinXY          (const aWND: HWND; const x: integer; const y: integer): TVoid;
function mmpSetWindowPos      (const aWND: HWND; aPt: TPoint; const aWidth: integer = 0; const aHeight: integer = 0; const aFlags: cardinal = SWP_NOSIZE): TVoid;
function mmpSetWindowTop      (const aWND: HWND): TVoid;
function mmpSetWindowTopmost  (const aWND: HWND): TVoid;
function mmpSetWindowSize     (const aWND: HWND; aPt: TPoint): TVoid;
function mmpWinXY             (const aWND: HWND): TPoint;

implementation

uses
  winApi.messages,
  system.math, system.types,
  bazCmd,
  mmpDesktopUtils, mmpPostToAllUtils, mmpUtils,
  _debugWindow;

function mmpAdjustAspectRatio(const aWND: HWND; const aHeight: integer): TPoint;
var
  vWidth:  integer;

  MPmediaType:      TMediaType;
  MPvideoWidth:     integer;
  MPvideoHeight:    integer;

  function adjustWidthForAspectRatio: integer;
  begin
    result := -1;
    case (MPvideoWidth <= 0) OR (MPvideoHeight <= 0) of TRUE: EXIT; end;
    result := round(aHeight / MPvideoHeight * MPvideoWidth);
  end;

  function getMediaInfo: TVoid;
  begin
    MPmediaType     := GS.mediaType;
    MPvideoWidth    := mmp.cmd(evMPReqVideoWidth).integer;
    MPvideoHeight   := mmp.cmd(evMPReqVideoHeight).integer;
//    debugFormat('MP.x:%d, MP.y:%d', [MPvideoWidth, MPvideoHeight]);
  end;

begin
//  FUserJ := TRUE;
  result := point(0, 0);
  getMediaInfo;

  vWidth := adjustWidthForAspectRatio;

  vWidth  := vWidth  + 2;   // allow for the mysterious 1-pixel border that Windows insists on drawing around a borderless window

  result.x := vWidth;
  result.y := aHeight + 2;
end;

function mmpAnimateResize(const aWND: HWND; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aCenter: boolean; const aDurationMs: integer): TVoid;
begin
  var vWorkArea: tRect := screen.workareaRect;
  var vLogicalHeight: integer := vWorkArea.height - aHeightDelta;

  var vDesktopCenterX: integer := vWorkArea.left + (vWorkArea.width div 2);
  var vDesktopCenterY: integer := vWorkArea.top + (vLogicalHeight div 2);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;

  var vR: tRect;
  getWindowRect(aWND, vR);
  var vInitialWidth: integer := vR.width;
  var vInitialHeight: integer := vR.height;
  var vInitialLeft: integer := vR.left;
  var vInitialTop: integer := vR.top;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    var vCurrentL: integer := vInitialLeft;
    var vCurrentT: integer := vInitialTop;

    case aCenter of
      TRUE:
        begin
          // Calculate 'Ideal' position centered on the desktop for the video window
          var vIdealL: integer := vDesktopCenterX - (vCurrentW div 2);
          var vIdealT: integer := vDesktopCenterY - (vCurrentH div 2);

          var vMaxAllowedR: integer := vWorkArea.left + vWorkArea.width;

          // Check for overhang on Right and Bottom caused by Deltas
          var vOverhangX: integer := max(0, (vIdealL + vCurrentW + aWidthDelta) - (vMaxAllowedR - 1));
          var vOverhangY: integer := max(0, (vIdealT + vCurrentH) - (vWorkArea.top + vLogicalHeight));

          // Subtract overhang to shift window only as much as necessary
          vCurrentL := vIdealL - vOverhangX;
          vCurrentT := vIdealT - vOverhangY;

          // Prevent shifting into negative space/off-monitor
          vCurrentL := max(vWorkArea.left, vCurrentL);
          vCurrentT := max(vWorkArea.top, vCurrentT);
        end;
    end;

    setWindowPos(aWND, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    updateWindow(aWND);
    application.processMessages();
  end;
end;

function mmpAnimateResize(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aCenter: boolean; const aDurationMs: integer): TVoid;
begin
  mmpAnimateResize(aTargetForm.HANDLE, aTargetWidth, aTargetHeight, aWidthDelta, aHeightDelta, aCenter, aDurationMs);
end;

function mmpAnimateResize_v7(const aWND: HWND; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aCenter: boolean; const aDurationMs: integer): TVoid;
begin
  var vWorkArea: tRect := screen.workareaRect;

  // Logical screen height matches mmpScreenHeight calculation
  var vLogicalHeight: integer := vWorkArea.height - aHeightDelta;

  var vDesktopCenterX: integer := vWorkArea.left + (vWorkArea.width div 2);
  var vDesktopCenterY: integer := vWorkArea.top + (vLogicalHeight div 2);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;

  var vR: tRect;
  getWindowRect(aWND, vR);
  var vInitialWidth:  integer := vR.width;
  var vInitialHeight: integer := vR.height;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth   + round((aTargetWidth   - vInitialWidth)  * vProgress);
    var vCurrentH: integer := vInitialHeight  + round((aTargetHeight  - vInitialHeight) * vProgress);

    // Calculate ideal position based on the Logical Height
    var vIdealL: integer := vDesktopCenterX - (vCurrentW div 2);
    var vIdealT: integer := vDesktopCenterY - (vCurrentH div 2);

    var vMaxAllowedR: integer := vWorkArea.left + vWorkArea.width;

    var vOverhangX: integer := max(0, (vIdealL + vCurrentW + aWidthDelta) - (vMaxAllowedR - 1));
    var vOverhangY: integer := max(0, (vIdealT + vCurrentH) - (vWorkArea.top + vLogicalHeight));

    var vCurrentL: integer := vIdealL - vOverhangX;
    var vCurrentT: integer := vIdealT - vOverhangY;

    vCurrentL := max(vWorkArea.left, vCurrentL);
    vCurrentT := max(vWorkArea.top, vCurrentT);

    setWindowPos(aWND, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH, SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    updateWindow(aWND);
    application.processMessages();
  end;
end;

function mmpAnimateResize_v6(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aDurationMs: integer): TVoid;
const
  Y_OFFSET_ADJUSTMENT = 2; // Increase this value to move the window higher (reduces Y)
begin
  var vWorkArea: tRect := screen.workareaRect;

  var vDesktopCenterX: integer := vWorkArea.left + (vWorkArea.width div 2);
  var vDesktopCenterY: integer := vWorkArea.top + (vWorkArea.height div 2);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    // 1. Calculate the 'Ideal' position centered on the desktop
    var vIdealL: integer := vDesktopCenterX - (vCurrentW div 2);
    var vIdealT: integer := vDesktopCenterY - (vCurrentH div 2) - Y_OFFSET_ADJUSTMENT;

    // 2. Determine if the attached Delta would push past the right/bottom screen edges
    var vMaxAllowedR: integer := vWorkArea.left + vWorkArea.width;
    var vMaxAllowedB: integer := vWorkArea.top + vWorkArea.height;

    var vOverhangX: integer := max(0, (vIdealL + vCurrentW + aWidthDelta) - vMaxAllowedR);
    var vOverhangY: integer := max(0, (vIdealT + vCurrentH + aHeightDelta) - vMaxAllowedB);

    // 3. Shift the window left/up only by the amount of the overhang
    var vCurrentL: integer := vIdealL - vOverhangX;
    var vCurrentT: integer := vIdealT - vOverhangY;

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpAnimateResize_beforeConstant(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aDurationMs: integer): TVoid;
begin
  var vWorkArea: tRect := screen.workareaRect;

  var vDesktopCenterX: integer := vWorkArea.left + (vWorkArea.width div 2);
  var vDesktopCenterY: integer := vWorkArea.top + (vWorkArea.height div 2);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    // Calculate the 'Ideal' position where the video window itself is centered
    var vIdealL: integer := vDesktopCenterX - (vCurrentW div 2);
    var vIdealT: integer := vDesktopCenterY - (vCurrentH div 2);

    // Determine if the attached Delta would push past the right/bottom screen edges
    var vMaxAllowedR: integer := vWorkArea.left + vWorkArea.width;
    var vMaxAllowedB: integer := vWorkArea.top + vWorkArea.height;

    var vOverhangX: integer := max(0, (vIdealL + vCurrentW + aWidthDelta) - vMaxAllowedR);
    var vOverhangY: integer := max(0, (vIdealT + vCurrentH + aHeightDelta) - vMaxAllowedB);

    // Shift the window left/up only by the amount of the overhang
    var vCurrentL: integer := vIdealL - vOverhangX;
    var vCurrentT: integer := vIdealT - vOverhangY;

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpAnimateResize_v4(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aDurationMs: integer): TVoid;
begin
  var vWorkArea: tRect := screen.workareaRect;

  var vAvailableW: integer := vWorkArea.width - aWidthDelta;
  var vAvailableH: integer := vWorkArea.height - aHeightDelta;

  var vCenterX: integer := vWorkArea.left + (vAvailableW div 2);
  var vCenterY: integer := vWorkArea.top + (vAvailableH div 2);

  var vStartL: integer := vCenterX - (aTargetForm.width div 2);
  var vStartT: integer := vCenterY - (aTargetForm.height div 2);

  setWindowPos(aTargetForm.handle, 0, vStartL, vStartT, aTargetForm.width, aTargetForm.height,
    SWP_NOZORDER or SWP_NOACTIVATE);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    var vCurrentL: integer := vCenterX - (vCurrentW div 2);
    var vCurrentT: integer := vCenterY - (vCurrentH div 2);

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpAnimateResize_v3(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aWidthDelta: integer; const aHeightDelta: integer; const aDurationMs: integer): TVoid;
begin
  var vWorkArea: tRect  := screen.workareaRect;
  var vScreenW: integer := vWorkArea.width;
  var vScreenH: integer := vWorkArea.height;

  var vCenteredL: integer := vWorkArea.left + (vScreenW div 2) - (aTargetForm.width div 2);
  var vCenteredT: integer := vWorkArea.top + (vScreenH div 2) - (aTargetForm.height div 2);

  setWindowPos(aTargetForm.handle, 0, vCenteredL, vCenteredT, aTargetForm.width, aTargetForm.height, SWP_NOZORDER or SWP_NOACTIVATE);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;

  var vCenterX: integer := vWorkArea.left + (vScreenW div 2);
  var vCenterY: integer := vWorkArea.top + (vScreenH div 2);

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    var vCurrentL: integer := vCenterX - (vCurrentW div 2);
    var vCurrentT: integer := vCenterY - (vCurrentH div 2);

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH, SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpAnimateResize_v2(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aDurationMs: integer): TVoid;
begin
  var vScreenW: integer := screen.width;
  var vScreenH: integer := screen.height;

  var vCenteredL: integer := (vScreenW div 2) - (aTargetForm.width div 2);
  var vCenteredT: integer := (vScreenH div 2) - (aTargetForm.height div 2);

  setWindowPos(aTargetForm.handle, 0, vCenteredL, vCenteredT, aTargetForm.width, aTargetForm.height,
    SWP_NOZORDER or SWP_NOACTIVATE);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;

  // Replacement lines: Use screen center as the fixed anchor
  // var vInitialLeft: integer := aTargetForm.left;
  // var vInitialTop: integer := aTargetForm.top;
  // var vCenterX: integer := vInitialLeft + (vInitialWidth div 2);
  // var vCenterY: integer := vInitialTop + (vInitialHeight div 2);
  var vCenterX: integer := vScreenW div 2;
  var vCenterY: integer := vScreenH div 2;

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    var vCurrentL: integer := vCenterX - (vCurrentW div 2);
    var vCurrentT: integer := vCenterY - (vCurrentH div 2);

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpAnimateResize_v1(const aTargetForm: TForm; const aTargetWidth: integer; const aTargetHeight: integer; const aDurationMs: integer): TVoid;
begin
  var vScreenW: integer := screen.width;
  var vScreenH: integer := screen.height;

  var vCenteredL: integer := (vScreenW div 2) - (aTargetForm.width div 2);
  var vCenteredT: integer := (vScreenH div 2) - (aTargetForm.height div 2);

  setWindowPos(aTargetForm.handle, 0, vCenteredL, vCenteredT, aTargetForm.width, aTargetForm.height,
    SWP_NOZORDER or SWP_NOACTIVATE);

  var vFreq: int64;
  queryPerformanceFrequency(vFreq);

  var vStartTick: int64;
  queryPerformanceCounter(vStartTick);

  var vTotalDurationSeconds: double := aDurationMs / 1000;
  var vInitialWidth: integer := aTargetForm.width;
  var vInitialHeight: integer := aTargetForm.height;
  var vInitialLeft: integer := aTargetForm.left;
  var vInitialTop: integer := aTargetForm.top;

  var vCenterX: integer := vInitialLeft + (vInitialWidth div 2);
  var vCenterY: integer := vInitialTop + (vInitialHeight div 2);

  var vProgress: double := 0;

  while vProgress < 1.0 do
  begin
    var vCurrentTick: int64;
    queryPerformanceCounter(vCurrentTick);

    var vElapsedSeconds: double := (vCurrentTick - vStartTick) / vFreq;

    // vProgress := vElapsedSeconds / vTotalDurationSeconds;
    // vEasedProgress := abs(sin(vProgress * 1.57079));
    vProgress := min(1.0, max(0.0, vElapsedSeconds / vTotalDurationSeconds));

    // var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vEasedProgress);
    var vCurrentW: integer := vInitialWidth + round((aTargetWidth - vInitialWidth) * vProgress);

    // var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vEasedProgress);
    var vCurrentH: integer := vInitialHeight + round((aTargetHeight - vInitialHeight) * vProgress);

    var vCurrentL: integer := vCenterX - (vCurrentW div 2);
    var vCurrentT: integer := vCenterY - (vCurrentH div 2);

    setWindowPos(aTargetForm.handle, 0, vCurrentL, vCurrentT, vCurrentW, vCurrentH,
      SWP_NOZORDER or SWP_NOACTIVATE or SWP_ASYNCWINDOWPOS);

    aTargetForm.update();
    application.processMessages();
  end;
end;

function mmpCalcWindowSize(const aStartingHeight: integer; const bMaxSize: boolean): TPoint;
{$J+} const vPrevVideoHeight: integer = 0; vPrevImageHeight: integer = 0; {$J-}
var
  vWidth:           integer;
  vHeight:          integer;
  vStartingHeight:  integer;

  MPmediaType:      TMediaType;
  MPvideoWidth:     integer;
  MPvideoHeight:    integer;
  MIhasCoverArt:    boolean;

  function adjustWidthForAspectRatio: integer;
  begin
    result := -1;
    case (MPvideoWidth = 0) or (MPvideoHeight = 0) of TRUE: EXIT; end;
    result := trunc(vHeight / MPvideoHeight * MPvideoWidth);
  end;

  function getMediaInfo: TVoid;
  begin
    MPmediaType     := GS.mediaType;
    MPvideoWidth    := mmp.cmd(evMPReqVideoWidth).integer;
    MPvideoHeight   := mmp.cmd(evMPReqVideoHeight).integer;
    MIhasCoverArt   := mmp.cmd(evMIReqHasCoverArt).tf;
//    debugFormat('getMediaInfo MP.x:%d, MP.y:%d', [MPvideoWidth, MPvideoHeight]);
  end;

  function withinScreenLimits: boolean;
  begin
    var vDelta  := mmpIfThenElse(GS.showingTimeline, GS.widthStreamlist, GS.widthHelp + GS.widthPlaylist); // at least one of widthHelp and widthPlaylist will be zero
    result      := (vWidth + vDelta <= mmpScreenWidth) and (vHeight <= mmpScreenHeight);
  end;

begin
//  debugBoolean('bMaxSize', bMaxsize);
//  debugInteger('aStartingHeight', aStartingHeight);
//  debugInteger('WorkAreaWidth', Screen.WorkAreaWidth);    // requires vcl.forms
//  debugInteger('WorkAreaHeight', Screen.WorkAreaHeight);  // ditto

  MPvideoWidth := 0; MPvideoHeight := 0;
  getMediaInfo;

  case MPmediaType of   mtAudio:  begin case MIhasCoverArt of  TRUE: vWidth  := 600;
                                                              FALSE: vWidth  := 600; end;
                                        case MIhasCoverArt of  TRUE: vHeight := 400;
                                                              FALSE: vHeight := UI_DEFAULT_AUDIO_HEIGHT; end;
                                        case MIhasCoverArt of  TRUE: vWidth := adjustWidthForAspectRatio; end;
                                  end;

                        mtVideo:  begin
                                        vStartingHeight := aStartingHeight;
                                        case bMaxSize of TRUE: vStartingHeight := -1; end;
                                        case (vStartingHeight <> -1) and (vStartingHeight <= UI_DEFAULT_AUDIO_HEIGHT) of TRUE: vStartingHeight := vPrevVideoHeight; end;
                                        case (vStartingHeight <> -1) and (vStartingHeight <= UI_DEFAULT_AUDIO_HEIGHT) of TRUE: vStartingHeight := -1; end;

                                        case vStartingHeight = -1 of
                                                                       TRUE: vHeight := mmpScreenHeight - 30;
                                                                      FALSE: vHeight := vStartingHeight; end;

                                        vWidth := adjustWidthForAspectRatio;

                                        while NOT withinScreenLimits do
                                        begin
                                          vHeight := vHeight - 30;
                                          vWidth  := adjustWidthForAspectRatio;
                                        end;

                                        vPrevVideoHeight := vHeight;
                                  end;

                        mtImage:  begin
                                        vStartingHeight := aStartingHeight;
                                        case GS.maxSize of TRUE: vStartingHeight := -1; end;
                                        case (vStartingHeight <> -1) and (vStartingHeight <= UI_DEFAULT_AUDIO_HEIGHT) of TRUE: vStartingHeight := vPrevImageHeight; end;
                                        case (vStartingHeight <> -1) and (vStartingHeight <= UI_DEFAULT_AUDIO_HEIGHT) of TRUE: vStartingHeight := -1; end;

                                        case vStartingHeight = -1 of  TRUE: begin
                                                                              vWidth  := trunc((mmpScreenHeight - 20) * 1.5); // EXPERIMENTAL WAS 100
                                                                              vHeight := mmpScreenHeight - 20; end;           // EXPERIMENTAL WAS 100
                                                                     FALSE: begin
                                                                              vWidth  := trunc(vStartingHeight * 1.5);
                                                                              vHeight := vStartingHeight; end;end;

                                        while NOT withinScreenLimits do
                                        begin
                                          vWidth  := vWidth  - 30;
                                          vHeight := vHeight - 30;
                                        end;

                                        vPrevImageHeight := vHeight;
                                  end;
  end;

  result.x := vWidth;
  result.y := vHeight;
end;

function mmpArrangeAll(const aWND: HWND): boolean;
var
  vCount:             integer;
  vWidth:             integer;
  vHeight:            integer;
  vScreenWidth:       integer;
  vScreenHeight:      integer;
  vZero:              integer;
  vHMiddle:           integer;
  //vVMiddle:           integer;
begin
  result := FALSE;

  mmp.cmd(evGSArrangeAll, TRUE); // ignore MMPs showing the Image & Thumbnail Browser
  vCount := mmp.cmd(evPAReqCount).integer;

  case vCount = 0 of TRUE: EXIT; end; // HOW!?

  mmp.cmd(evGSAutoCenter, vCount = 1);
  case GS.autoCenter of FALSE:  begin
                                  mmp.cmd(evPAPostToEvery, WIN_AUTOCENTER_OFF);
                                  mmp.cmd(evPAPostToEvery, WIN_MAX_SIZE_OFF); end;end;

  var vMsg: TMessage;
  vMsg := default(TMessage);
  case vCount of
    1:       vMsg.WParam := mmpScreenWidth;
    2:       vMsg.WParam := mmpScreenWidth  div 2;
    3, 4:    vMsg.LParam := mmpScreenHeight div 2;
    else     vMsg.WParam := mmpScreenWidth  div vCount;
  end;
  vMsg.msg := WIN_RESIZE;
  mmp.cmd(evPAPostToEveryEx, vMsg); // this rebuilds the list of HWNDs

  mmpProcessMessages; // make sure this window has resized before continuing

  mmpWndWidthHeight(aWND, vWidth, vHeight);
  vScreenWidth  := mmpScreenWidth;
  vScreenHeight := mmpScreenHeight;
  vHMiddle      := vScreenWidth   div 2;
  // vVMiddle      := vScreenHeight  div 2;
  vZero         := vHMiddle - vWidth;

  vCount    := mmp.cmd(evPAReqCount).integer;
  var vHWND := 0;

  mmp.cmd(evGSArrangeAll, FALSE); // ensure that subsequent universal commands go to all MMPs

  case vCount = 2 of TRUE: begin
                             mmpPosWinXY(PA[1], vZero,    (vScreenHeight - vHeight) div 2);
                             mmpPosWinXY(PA[2], vHMiddle, (vScreenHeight - vHeight) div 2);
                             case mmpOffScreen(PA[1]) of TRUE: mmpPosWinXY(PA[1], vZero,    0); end;
                             case mmpOffScreen(PA[2]) of TRUE: mmpPosWinXY(PA[2], vHMiddle, 0); end;
                             vHWND := PA[1];
                           end;end;

  case vCount in [3, 4] of TRUE: begin
                             mmpPosWinXY(PA[1], vZero,     0 + 40);
                             mmpPosWinXY(PA[2], vHMiddle,  0 + 40); end;end;

  case vCount = 3 of TRUE: mmpPosWinXY(PA[3], vHMiddle - (vWidth div 2), vHeight + 40); end;

  case vCount = 4 of TRUE: begin
                              mmpPosWinXY(PA[3], vZero,     vHeight + 40);
                              mmpPosWinXY(PA[4], vHMiddle,  vHeight + 40); end;end;

  case vCount > 4 of TRUE: for var i := 1 to vCount do mmpPosWinXY(PA[i], ((mmpScreenWidth div vCount) * (i - 1)), 100); end;

  case vHWND <> 0 of TRUE: begin mmpDelay(100); mmpPosWinXY(vHWND, mmpScreenCentre - vWidth, mmpWinXY(vHWND).Y); end;end; // hack for tall, narrow, TikTok-type windows

  setForegroundWindow(aWnd);

  result := TRUE;
end;

function mmpCalcGreaterWindow(const aWND: HWND; const aShiftState: TShiftState; const aThumbSize: integer; const aHostType: THostType): TPoint;
var
  dx:   integer;
  dy:   integer;
  newW: integer;
  newH: integer;
  vR:   TRect;

  function calcDeltas: TVoid;
  begin
    case aHostType of
      htMPVHost:    begin
                      dx := 50;
                      dy := 30;
                    end;
      htThumbsHost: begin
                      dx := aThumbSize + THUMB_MARGIN;
                      dy := aThumbSize + THUMB_MARGIN;
                    end;
    end;
  end;

  function checkDesktop: TVoid;
  begin
    case ssCtrl in aShiftState of  TRUE:  begin
                                            case newW - dx < dx of TRUE: dx := 0; end;
                                            case newH - dy < dy of TRUE: dy := 0; end;end;
                                  FALSE:  begin
                                            case newW + dx > mmpScreenWidth  of TRUE: dx := 0; end;
                                            case newH + dy > mmpScreenHeight of TRUE: dy := 0; end;end;end;
  end;

  function calcDimensions: TVoid;
  begin
    case ssCtrl in aShiftState of
      TRUE: begin
              newW := newW - dx;
              newH := newH - dy;
            end;
     FALSE: begin
              newW := newW + dx;
              newH := newH + dy;
            end;
    end;
  end;

begin
  getWindowRect(aWND, vR);
  newW := vR.Width;
  newH := vR.height;

//  debugInteger('greaterWindow vR.height', vR.height);
//  TDebug.debugEnum<THostType>('aHostType', aHostType);

  calcDeltas;
  checkDesktop;
  calcDimensions; // do what the user requested

  result := point(newW, newH);
end;

function mmpCenterWindow(const aWND: HWND; const aPt: TPoint; const aFlags: Cardinal = SWP_NOSIZE): TVoid;
// aPt is optional and provides the calculated dimensions that a window is going to have
var
  vR:     TRect;
  vHPos:  integer;
  vVPos:  integer;

  function alreadyCentred: boolean;
  begin
    var vWidthDelta  := mmpIfThenElse(GS.showingTimeline, GS.widthStreamlist, GS.widthHelp + GS.widthPlaylist); // at least one of either widthHelp or widthPlaylist will be zero

    vHPos       := ((mmpScreenWidth  - vR.width) div 2);
    case vHPos + vR.width + vWidthDelta > mmpScreenWidth of TRUE: vHPos := mmpScreenWidth - vR.width - vWidthDelta - 1; end; // only shift left if necessary

    // ensure we never shift into negative space (left monitor)
    case vHPos < 0 of TRUE: vHPos := 0; end;

    vVPos       := (mmpScreenHeight - vR.height) div 2;
    case vVPos + vR.height > mmpScreenHeight of TRUE: vVPos := mmpScreenHeight - vR.height - 1; end;
    // ensure we never shift into negative space (top monitor)
    case vVPos < 0 of TRUE: vVPos := 0; end;

    result      := (vR.left = vHPos) and (vR.top = vVPos);
  end;

begin
  getWindowRect(aWND, vR);

  case (aPt.x <> 0) and (aPt.y <> 0) of  TRUE:  begin // override the current dimensions with those provided
                                                  vR.width  := aPt.x;
                                                  vR.height := aPt.y; end;end;


  case alreadyCentred of TRUE: EXIT; end;

  case (vHPos >= 0) and (vVPos >= 0) of TRUE: mmpSetWindowPos(aWND, point(vHPos, vVPos), vR.width, vR.height, aFlags); end;

  mmp.cmd(evGSAutoCenter, TRUE);
end;

function mmpGreaterWindow(const aWND: HWND; aShiftState: TShiftState): integer;
var
  vR: TRect;
begin
  getWindowRect(aWND, vR);

  GS.notify(newNotice(evGSMaxSize, FALSE)); // pressing [M] reinstates maxSize

  result := mmp.use<integer>(ssCtrl in aShiftState, vR.height - 30, vR.height + 30);
end;

function mmpGreaterWindow(const aWND: HWND; const aShiftState: TShiftState; const aThumbSize: integer; const aHostType: THostType): TVoid;
var
  dx:   integer;
  dy:   integer;
  newW: integer;
  newH: integer;
  vR:   TRect;

  function calcDeltas: TVoid;
  begin
    case aHostType of
      htMPVHost:    begin
                      dx := 50;
                      dy := 30;
                    end;
      htThumbsHost: begin
                      dx := aThumbSize + THUMB_MARGIN;
                      dy := aThumbSize + THUMB_MARGIN;
                    end;
    end;
  end;

  function checkDesktop: TVoid;
  begin
    case ssCtrl in aShiftState of  TRUE:  begin
                                            case newW - dx < dx of TRUE: dx := 0; end;
                                            case newH - dy < dy of TRUE: dy := 0; end;end;
                                  FALSE:  begin
                                            case newW + dx > mmpScreenWidth  of TRUE: dx := 0; end;
                                            case newH + dy > mmpScreenHeight of TRUE: dy := 0; end;end;end;
  end;

  function calcDimensions: TVoid;
  begin
    case ssCtrl in aShiftState of
      TRUE: begin
              newW := newW - dx;
              newH := newH - dy;
            end;
     FALSE: begin
              newW := newW + dx;
              newH := newH + dy;
            end;
    end;
  end;

begin
  getWindowRect(aWND, vR);
  newW := vR.Width;
  newH := vR.height;

//  debugInteger('greaterWindow vR.height', vR.height);
//  TDebug.debugEnum<THostType>('aHostType', aHostType);

  calcDeltas;
  checkDesktop;
  calcDimensions; // do what the user requested

  mmpSetWindowSize(aWND, point(newW, newH)); // resize the window
end;

function mmpPosWinXY(const aWND: HWND; const x: integer; const y: integer): TVoid;
begin
  mmpSetWindowPos(aWND, point(x, y));
end;

function mmpSetWindowPos(const aWND: HWND; aPt: TPoint; const aWidth: integer = 0; const aHeight: integer = 0; const aFlags: cardinal = SWP_NOSIZE): TVoid;
begin
  setWindowPos(aWND, HWND_TOP, aPt.x, aPt.y, aWidth, aHeight, aFlags);
end;

function mmpSetWindowSize(const aWND: HWND; aPt: TPoint): TVoid;
begin
  setWindowPos(aWND, HWND_TOP, 0, 0, aPt.x, aPt.y, SWP_NOMOVE);
end;

function mmpSetWindowTop(const aWND: HWND): TVoid;
begin
  setWindowPos(aWND, HWND_TOP, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
end;

function mmpSetWindowTopmost(const aWND: HWND): TVoid;
begin
  setWindowPos(aWND, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
end;

function mmpWinXY(const aWND: HWND): TPoint;
var vR: TRect;
begin
  getWindowRect(aWnd, vR);
  result := vR.location;
end;

end.
