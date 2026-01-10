object HelpFullForm: THelpFullForm
  Left = 0
  Top = 0
  Caption = 'MMP Help'
  ClientHeight = 361
  ClientWidth = 584
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object pageControl: TPageControl
    Left = 0
    Top = 0
    Width = 584
    Height = 361
    Align = alClient
    TabOrder = 0
  end
end
