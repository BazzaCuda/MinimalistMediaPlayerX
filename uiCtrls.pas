{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda <bazzacuda@gmx.com>
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
unit uiCtrls;

interface

uses
  Forms, winApi.windows, consts, winAPI.shellAPI, vcl.graphics, vcl.controls, vcl.ComCtrls, globalVars, vcl.extCtrls,
  system.classes;

type
  TUI = class(TObject)
  strict private
    FMainForm: TForm;
    FVideoPanel: TPanel;
  private
    function addMenuItems(aForm: TForm): boolean;
    function setCustomTitleBar(aForm: TForm): boolean;
    function setGlassFrame(aForm: TForm): boolean;
    function setWindowStyle(aForm: TForm): boolean;
    function createVideoPanel(aForm: TForm): boolean;
  public
    function deleteCurrentItem(shift: TShiftState): boolean;
    function greaterWindow(shift: TShiftState): boolean;
    function initUI(aForm: TForm): boolean;
    function minimizeWindow: boolean;
    function openExternalApp(anApp: string; aParams: string): boolean;
    function renameFile(aFilePath: string): boolean;
    function toggleBlackout: boolean;
    function toggleControls(shift: TShiftState): boolean;
    function toggleMaximised: boolean;
    property mainForm: TForm read FMainForm;
    property videoPanel: TPanel read FVideoPanel;
  end;

function UI: TUI;

implementation

uses
  formSubtitles, mediaInfo, mediaPlayer, commonUtils, progressBar, winApi.messages, playlist, system.sysUtils, formCaption, _debugWindow;

var
  gUI: TUI;

function UI: TUI;
begin
  case gUI = NIL of TRUE: gUI := TUI.create; end;
  result := gUI;
end;

{ TUI }

function TUI.addMenuItems(aForm: TForm): boolean;
begin
  var vSysMenu := getSystemMenu(aForm.handle, FALSE);
  AppendMenu(vSysMenu, MF_SEPARATOR, 0, '');
  AppendMenu(vSysMenu, MF_STRING, MENU_ABOUT_ID, '&About Minimalist Media Player…');
  AppendMenu(vSysMenu, MF_STRING, MENU_HELP_ID, 'Show &Keyboard functions');
end;

function TUI.createVideoPanel(aForm: TForm): boolean;
begin
  FVideoPanel        := TPanel.create(aForm);
  FVideoPanel.parent := aForm;
  FVideoPanel.align  := alClient;
  FVideoPanel.color  := clBlack;
  FVideoPanel.BevelOuter := bvNone;
end;

function TUI.deleteCurrentItem(shift: TShiftState): boolean;
begin
  case PL.hasItems of FALSE: EXIT; end;
  MP.pause;

  var vMsg := 'DELETE '#13#10#13#10'Folder: ' + extractFilePath(PL.currentItem);
  case ssCtrl in Shift of  TRUE: vMsg := vMsg + '*.*';
                          FALSE: vMsg := vMsg + #13#10#13#10'File: ' + extractFileName(PL.currentItem); end;

  case showOkCancelMsgDlg(vMsg) = IDOK of TRUE: begin
                                                  var vIx := PL.currentIx;  // make a note of this ix because...
                                                  MP.stop;                  // this will automatically do MP.playNext
                                                  deleteThisFile(PL.thisItem(vIx), shift);
                                                  PL.delete(vIx); end;end;
end;

function TUI.greaterWindow(shift: TShiftState): boolean;
// [G]reater  = increase size of window
// Ctrl-G     = decrease size of window
begin
  case ssCtrl in Shift of
     TRUE: SetWindowPos(FMainForm.handle, 0, 0, 0, FMainForm.Width - 100, FMainForm.Height - 60, SWP_NOZORDER + SWP_NOMOVE + SWP_NOREDRAW);
    FALSE: SetWindowPos(FMainForm.handle, 0, 0, 0, FMainForm.Width + 100, FMainForm.Height + 60, SWP_NOZORDER + SWP_NOMOVE + SWP_NOREDRAW);
  end;end;

function TUI.initUI(aForm: TForm): boolean;
begin
  FMainForm := aForm;
  aForm.position      := poScreenCenter;
  aForm.borderIcons   := [biSystemMenu];
  aForm.styleElements := []; // [seFont]; //, seClient];
  setGlassFrame(aForm);
  setCustomTitleBar(aForm);
  setWindowStyle(aForm);
  DragAcceptFiles(aForm.handle, TRUE);
  addMenuItems(aForm);
  aForm.color         := clBlack; // background color of the window's client area, so zooming-out doesn't show the design-time color
  createVideoPanel(aForm);
end;

function TUI.minimizeWindow: boolean;
begin
   application.Minimize;
end;

function TUI.openExternalApp(anApp, aParams: string): boolean;
begin
  debugFormat('%s %s', [anApp, aParams]);
  MP.pause;
  shellExec(anApp, aParams);
end;

function TUI.renameFile(aFilePath: string): boolean;
var
  vNewName: string;
begin
  case PL.hasItems of FALSE: EXIT; end;
  MP.pause;
  vNewName := commonUtils.renameFile(aFilePath);
  case vNewName <> aFilePath of TRUE: PL.replaceCurrentItem(vNewName); end;
  MC.caption := PL.formattedItem;
  MP.resume;
end;

function TUI.setCustomTitleBar(aForm: TForm): boolean;
begin
  aForm.customTitleBar.enabled        := TRUE;
  aForm.customTitleBar.showCaption    := FALSE;
  aForm.customTitleBar.showIcon       := FALSE;
  aForm.customTitleBar.systemButtons  := FALSE;
  aForm.customTitleBar.systemColors   := FALSE;
  aForm.customTitleBar.systemHeight   := FALSE;
  aForm.customTitleBar.height         := 1; // systemHeight=FALSE must be set before this
end;

function TUI.setGlassFrame(aForm: TForm): boolean;
begin
  aForm.glassFrame.enabled  := TRUE;
  aForm.glassFrame.top      := 1;
end;

function TUI.setWindowStyle(aForm: TForm): boolean;
begin
  SetWindowLong(aForm.handle, GWL_STYLE, GetWindowLong(aForm.handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
end;

function TUI.toggleBlackout: boolean;
begin
  PB.showProgressBar := NOT PB.showProgressBar;
end;

function TUI.toggleControls(shift: TShiftState): boolean;
// we call them "controls" but they're actually the media info and the time display at the bottom of the window
// a left-over from this app's pre-minimalist days
begin
 case (ssCtrl in shift) and ST.showTime and (not ST.showData) of TRUE: begin MI.getData(ST.dataMemo); ST.showData := TRUE; EXIT; end;end;

 ST.showTime := NOT ST.showTime;

 case (ssCtrl in shift) and ST.showTime of  TRUE: begin MI.getData(ST.dataMemo); ST.showData := TRUE; end;
                                           FALSE: ST.showData := FALSE; end;
end;

function TUI.toggleMaximised: boolean;
begin
  case FMainForm.WindowState <> wsMaximized of  TRUE: FMainForm.windowState := wsMaximized;
                                               FALSE: FMainForm.windowState := wsNormal; end;
end;

initialization
  gUI := NIL;

finalization
  case gUI <> NIL of TRUE: gUI.free; end;

end.
