object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  Caption = 'ProgressForm'
  ClientHeight = 125
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 325
    Height = 125
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 323
    ExplicitHeight = 121
    object FLabel1: TLabel
      Left = 6
      Top = 59
      Width = 313
      Height = 15
      Margins.Top = 10
      Alignment = taCenter
      AutoSize = False
      Caption = 'FLabel1'
    end
    object FHeading: TLabel
      Left = 5
      Top = 29
      Width = 313
      Height = 15
      Margins.Top = 10
      Alignment = taCenter
      AutoSize = False
      Caption = 'FHeading'
    end
    object btnOK: TButton
      Left = 128
      Top = 88
      Width = 75
      Height = 25
      Caption = 'OK'
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 128
      Top = 88
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 1
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
end
