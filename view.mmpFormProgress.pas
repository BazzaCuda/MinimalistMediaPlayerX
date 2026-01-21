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
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.classes, system.sysUtils, system.variants,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.stdCtrls;
  {$endif}

type
  IProgressForm = interface
    function    getTimer: TTimer;

    procedure   setButtons(const bVisible: boolean);
    procedure   setHeading(const aValue: string);
    procedure   setModal(const bModal: boolean);
    procedure   setOnCancel(const aValue: TNotifyEvent);
    procedure   setSubHeading(const aValue: string);

    property    buttons: boolean write setButtons;
    property    heading: string write setHeading;
    property    modal: boolean write setModal;
    property    onCancel: TNotifyEvent write setOnCancel;
    property    subHeading: string write setSubHeading;
    property    timer: TTimer read getTimer;
  end;

  TProgressForm = class(TForm, IProgressForm)
    Panel1:     TPanel;
    FSubHeading: TLabel;
    FHeading:   TLabel;
    btnIgnore:  TButton;
    btnCancel:  TButton;
    btnRerun:   TButton;
    FTimer:     TTimer;
    dummyLabel: TLabel;
    procedure   btnCancelClick(Sender: TObject);
    procedure   FormCreate(Sender: TObject);
  strict private
    FRefCount: IInterface;

    FOnCancel:  TNotifyEvent;
   protected
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  public
    constructor Create(AOwner: TComponent); override;

    function    getTimer: TTimer;

    procedure   setButtons(const bVisible: boolean);
    procedure   setHeading(const aValue: string);
    procedure   setModal(const bModal: boolean);
    procedure   setOnCancel(const aValue: TNotifyEvent);
    procedure   setSubHeading(const aValue: string);

    property    buttons:      boolean                         write setButtons;
    property    heading:      TLabel        read FHeading;
    property    subHeading:   TLabel        read FSubHeading;
    property    modal:        boolean                         write setModal;
    property    onCancel:     TNotifyEvent  {read FOnCancel}    write FOnCancel;
    property    timer:        TTimer        read FTimer; // TEMPORARY
  end;

function mmpNewProgressForm: IProgressForm;

implementation

uses
  mmpConsts;

{$R *.dfm}

function mmpNewProgressForm: IProgressForm;
begin
  result := TProgressForm.create(NIL);
end;

procedure TProgressForm.btnCancelClick(Sender: TObject);
begin
  case assigned(FOnCancel) of TRUE: FOnCancel(SELF); end;
end;

constructor TProgressForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRefCount := TInterfacedObject.Create;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  setWindowLong(handle, GWL_STYLE, GetWindowLong(handle, GWL_STYLE) OR WS_CAPTION AND (NOT (WS_BORDER)));
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
    alignWithMargins := TRUE;
    panel1.color     := DARK_MODE_LIGHT;
  end;
  setModal(FALSE);
end;

function TProgressForm.getTimer: TTimer;
begin
  result := FTimer;
end;

procedure TProgressForm.setButtons(const bVisible: boolean);
begin
  btnRerun.visible  := bVisible;
  btnIgnore.visible := bVisible;
  btnCancel.visible := bVisible;
  case bVisible of FALSE: SELF.height := SELF.height - btnRerun.height; end;
end;

procedure TProgressForm.setHeading(const aValue: string);
begin
  FHeading.caption := aValue;
end;

procedure TProgressForm.setModal(const bModal: boolean);
begin
  btnRerun.visible  := bModal;
  btnIgnore.visible := bModal;
  btnCancel.visible := NOT bModal;
end;

procedure TProgressForm.setOnCancel(const aValue: TNotifyEvent);
begin
  FOnCancel := aValue;
end;

procedure TProgressForm.setSubHeading(const aValue: string);
begin
  FSubHeading.caption := aValue;
end;

function TProgressForm._AddRef: integer;
begin
  result := FRefCount._AddRef;
end;

function TProgressForm._Release: integer;
begin
  result := FRefCount._Release;
  case result = 0 of TRUE: Destroy; end;
end;

end.
