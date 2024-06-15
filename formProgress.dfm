object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  Caption = 'ProgressForm'
  ClientHeight = 90
  ClientWidth = 321
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
    Width = 321
    Height = 90
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 10
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 319
    ExplicitHeight = 86
    object FSubHeading: TLabel
      Left = 6
      Top = 35
      Width = 313
      Height = 31
      Margins.Top = 10
      Alignment = taCenter
      AutoSize = False
      Caption = 'FSubHeading'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      WordWrap = True
    end
    object FHeading: TLabel
      Left = 5
      Top = 8
      Width = 313
      Height = 15
      Margins.Top = 10
      Alignment = taCenter
      AutoSize = False
      Caption = 'FHeading'
    end
    object btnIgnore: TButton
      Left = 201
      Top = 70
      Width = 75
      Height = 25
      Caption = 'Ignore'
      ModalResult = 7
      TabOrder = 2
    end
    object btnCancel: TButton
      Left = 128
      Top = 70
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
    object btnRerun: TButton
      Left = 55
      Top = 70
      Width = 75
      Height = 25
      Hint = 
        'Rerun the failed segment so you can see the FFMpeg error message' +
        's'
      Caption = 'Rerun'
      Default = True
      ModalResult = 6
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
  end
  object FTimer: TTimer
    Left = 32
    Top = 16
  end
end
