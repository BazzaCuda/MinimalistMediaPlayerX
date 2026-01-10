{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
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
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics, vcl.stdCtrls,
  {$endif}
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber;

type
  TInputBoxForm = class(TForm)
    edtInputBox:            TEdit;
    btnModalResultmrOK:     TButton;
    btnModalResultmrCancel: TButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSubscriber: ISubscriber;
    function onNotify(const aNotice: INotice): INotice;
  public
  end;

function mmpInputBoxForm(const aPrompt: string): string;

implementation

uses
  bazCmd,
  mmpGlobalState, mmpTickTimer, mmpUtils,
  _debugWindow;

{$R *.dfm}

{ TInputBoxForm }

function mmpInputBoxForm(const aPrompt: string): string;
var
  vInputBoxForm: TInputBoxForm;
begin
  vInputBoxForm := TInputBoxForm.Create(NIL);
  try
    mmp.cmd(evGSUserInput, TRUE);
    with vInputBoxForm do begin
      edtInputBox.Text  := aPrompt;
      result            := aPrompt;
      case showModal = mrOK of TRUE: result := edtInputBox.Text; end;
    end;
  finally
    vInputBoxForm.free;
    mmpDelay(500); // Vital. It prevents the VK_ENTER keyUp going to the playlist form and restarting the audio/video being renamed
    mmp.cmd(evGSUserInput, FALSE);
  end;
end;

procedure TInputBoxForm.FormActivate(Sender: TObject);
begin
  setForegroundWindow(SELF.handle);
end;

procedure TInputBoxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TT.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
end;

procedure TInputBoxForm.FormCreate(Sender: TObject);
begin
  FSubscriber := newSubscriber(onNotify);
  TT.subscribe(FSubscriber);
end;

function TInputBoxForm.onNotify(const aNotice: INotice): INotice;
begin
  setForegroundWindow(SELF.handle);
end;

end.
