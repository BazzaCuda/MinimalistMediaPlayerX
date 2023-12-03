{   Minimalist Media Player
    Copyright (C) 2021 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayer

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
unit formProgress;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TProgressForm = class(TForm)
    Panel1: TPanel;
    FSubHeading: TLabel;
    FHeading: TLabel;
    btnIgnore: TButton;
    btnCancel: TButton;
    btnRerun: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FOnCancel: TNotifyEvent;
    procedure setModal(const isModal: boolean);
  public
    property heading:       TLabel       read FHeading;
    property subHeading:    TLabel       read FSubHeading;
    property modal:         boolean                         write setModal;
    property onCancel:      TNotifyEvent read FOnCancel     write FOnCancel;
  end;

var
  progressForm: TProgressForm;

implementation

{$R *.dfm}

procedure TProgressForm.btnCancelClick(Sender: TObject);
begin
  case assigned(FOnCancel) of TRUE: FOnCancel(SELF); end;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := $2B2B2B;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  position          := poScreenCenter;

  with panel1 do begin
    styleElements    := []; // don't allow any theme alterations
    align            := alClient;
    bevelInner       := bvNone;
    bevelOuter       := bvNone;
    borderStyle      := bsNone;
    margins.bottom   := 10;
    margins.left     := 10;
    margins.right    := 10;
    margins.top      := 10;
    AlignWithMargins := TRUE;
    panel1.color     := $232323;
  end;
  setModal(FALSE);
end;

procedure TProgressForm.setModal(const isModal: boolean);
begin
  btnRerun.visible  := isModal;
  btnIgnore.visible := isModal;
  btnCancel.visible := NOT isModal;
end;

end.
