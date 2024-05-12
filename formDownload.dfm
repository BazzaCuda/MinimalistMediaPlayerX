object DownloadForm: TDownloadForm
  Left = 0
  Top = 0
  Caption = 'Download'
  ClientHeight = 90
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object Label1: TLabel
    Left = 74
    Top = 59
    Width = 233
    Height = 15
    Alignment = taCenter
    AutoSize = False
  end
  object Label2: TLabel
    Left = 120
    Top = 8
    Width = 150
    Height = 15
    Caption = 'Downloading latest release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 142
    Top = 38
    Width = 97
    Height = 15
    Caption = 'bytes downloaded'
  end
end
