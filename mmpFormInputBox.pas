{   MMP: Minimalist Media Player
    Copyright (C) 2021-2024 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit mmpFormInputBox;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  TInputBoxForm = class(TForm)
    edtInputBox: TEdit;
    btnModalResultmrOK: TButton;
    btnModalResultmrCancel: TButton;
  private
  public
  end;

function mmpInputBoxForm(const aPrompt: string): string;

implementation

uses
  vcl.styles, vcl.themes,
  mmpUtils;

{$R *.dfm}

{ TInputBoxForm }

function mmpInputBoxForm(const aPrompt: string): string;
var
  vInputBoxForm: TInputBoxForm;
begin
  notifyApp(newNotice(evGSUserInput, TRUE));
  vInputBoxForm := TInputBoxForm.Create(NIL);
  try
    with vInputBoxForm do begin
      edtInputBox.Text  := aPrompt;
      result            := aPrompt;
      notifyApp(newNotice(evGSUserInput, TRUE));
      case showModal = mrOK of TRUE: result := edtInputBox.Text; end;
      notifyApp(newNotice(evGSUserInput, FALSE));
    end;
  finally
    vInputBoxForm.free;
//    mmpDelay(500); // CHECK THIS
    notifyApp(newNotice(evGSUserInput, FALSE));
  end;
end;

end.
