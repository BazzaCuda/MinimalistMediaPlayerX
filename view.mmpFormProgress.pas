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
unit view.mmpFormProgress;

interface

uses
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls;

type
  TProgressForm = class(TForm)
    Panel1:     TPanel;
    FSubHeading: TLabel;
    FHeading:   TLabel;
    btnIgnore:  TButton;
    btnCancel:  TButton;
    btnRerun:   TButton;
    FTimer:     TTimer;
    procedure   btnCancelClick(Sender: TObject);
    procedure   FormCreate(Sender: TObject);
  private
    FOnCancel:  TNotifyEvent;
    procedure   setButtons(const value: boolean);
    procedure   setModal(const isModal: boolean);
  public
    property    buttons:      boolean                         write setButtons;
    property    heading:      TLabel        read FHeading;
    property    subHeading:   TLabel        read FSubHeading;
    property    modal:        boolean                         write setModal;
    property    onCancel:     TNotifyEvent  read FOnCancel    write FOnCancel;
    property    timer:        TTimer        read FTimer;
  end;

implementation

uses
  mmpConsts;

{$R *.dfm}

procedure TProgressForm.btnCancelClick(Sender: TObject);
begin
  case assigned(FOnCancel) of TRUE: FOnCancel(SELF); end;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  SetWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
  color := DARK_MODE_DARK;

  styleElements     := []; // don't allow any theme alterations
  borderStyle       := bsNone;
  position          := poScreenCenter;

  FTimer.enabled    := FALSE;

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
    panel1.color     := DARK_MODE_LIGHT;
  end;
  setModal(FALSE);
end;

procedure TProgressForm.setButtons(const value: boolean);
begin
  btnRerun.visible  := value;
  btnIgnore.visible := value;
  btnCancel.visible := value;
  case value of FALSE: SELF.height := SELF.height - btnRerun.height; end;
end;

procedure TProgressForm.setModal(const isModal: boolean);
begin
  btnRerun.visible  := isModal;
  btnIgnore.visible := isModal;
  btnCancel.visible := NOT isModal;
end;

end.
