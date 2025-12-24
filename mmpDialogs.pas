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
unit mmpDialogs;

interface

uses
  vcl.controls, vcl.dialogs,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

function mmpShowOKCancelMsgDlg(const aMsg: string;
                               const msgDlgType:    TMsgDlgType    = mtConfirmation;
                               const msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                               const defButton:     TMsgDlgBtn     = MBCANCEL;
                               const aCaption:      string         = ''): TModalResult;
function mmpUserOK(const aMsg: string): boolean;

implementation

uses
  winApi.activeX,
  vcl.forms, vcl.stdCtrls,
  mmpFuncProg, mmpGlobalState;

function mmpShowOKCancelMsgDlg(const aMsg: string;
                               const msgDlgType:    TMsgDlgType    = mtConfirmation;
                               const msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                               const defButton:     TMsgDlgBtn     = MBCANCEL;
                               const aCaption:      string         = ''): TModalResult;
// We modify the standard dialog to make everything bigger, especially the width so that long folder names and files display properly
// The standard dialog would unhelpfully truncate them.
begin
  screen.cursor := crDefault;
  coInitialize(NIL);
  with createMessageDialog(aMsg, msgDlgType, msgDlgButtons, defButton) do
  try
    mmp.cmd(evGSUserInput, TRUE);
    font.name := 'Segoe UI';
    font.size := 12;
    height    := height +  50;
    width     := width  + 200;
    case aCaption = '' of FALSE: caption := aCaption; end;

    for var i := 0 to controlCount - 1 do begin
      mmp.cmd(controls[i] is  TLabel, procedure begin with controls[i] do width  := width + 200; end);
      mmp.cmd(controls[i] is TButton, procedure begin with controls[i] do begin
                                                                          top  := top   +  60;
                                                                          left := left  + 100;
                                                                        end;end);
    end;
    result := showModal;
  finally
    free;
    mmp.cmd(evGSUserInput, FALSE);
    coUninitialize;
  end;
end;

function mmpUserOK(const aMsg: string): boolean;
begin
  result := mmpShowOKCancelMsgDlg(aMsg, mtWarning, [mbYes, mbNo], mbNo) = mrYes;
end;

end.
