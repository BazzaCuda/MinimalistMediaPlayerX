{   Minimalist Media Player
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
unit mmpDialogs;

interface

uses
  vcl.controls, vcl.dialogs;

function mmpShowOKCancelMsgDlg(const aMsg: string;
                               const msgDlgType:    TMsgDlgType    = mtConfirmation;
                               const msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                               const defButton:     TMsgDlgBtn     = MBCANCEL): TModalResult;

implementation

uses
  winApi.activeX,
  vcl.forms, vcl.stdCtrls,
  mmpSingletons;

function mmpShowOKCancelMsgDlg(const aMsg: string;
                               const msgDlgType:    TMsgDlgType    = mtConfirmation;
                               const msgDlgButtons: TMsgDlgButtons = MBOKCANCEL;
                               const defButton:     TMsgDlgBtn     = MBCANCEL): TModalResult;
// used for displaying the delete file/folder confirmation dialog
// We modify the standard dialog to make everything bigger, especially the width so that long folder names and files display properly
// The standard dialog would unhelpfully truncate them.#
var vControl: TControl;
begin
  screen.cursor := crDefault;
  coInitialize(NIL);
  with CreateMessageDialog(aMsg, msgDlgType, msgDlgButtons, defButton) do
  try
    GV.userInput := TRUE;
    font.name := 'Segoe UI';
    font.size := 12;
    height    := height + 50;
    width     := width + 200;

    for var i := 0 to controlCount - 1 do begin
      case controls[i] is TLabel  of   TRUE: with Controls[i] as TLabel do Width := Width + 200; end;
      case controls[i] is TButton of   TRUE: with Controls[i] as TButton do begin
                                                                                top  := top  + 60;
                                                                                left := left + 100;
                                                                            end;end;
    end;
    result := ShowModal;
  finally
    free;
    GV.userInput := FALSE;
    coUninitialize;
  end;
end;

end.
