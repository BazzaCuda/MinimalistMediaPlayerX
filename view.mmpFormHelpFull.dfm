object HelpFullForm: THelpFullForm
  Left = 0
  Top = 0
  Margins.Left = 0
  Margins.Top = 0
  Margins.Right = 0
  Margins.Bottom = 0
  Caption = 'MMP Help'
  ClientHeight = 612
  ClientWidth = 584
  Color = clBtnFace
  CustomTitleBar.Control = titleBar
  CustomTitleBar.Enabled = True
  CustomTitleBar.Height = 31
  CustomTitleBar.SystemColors = False
  CustomTitleBar.BackgroundColor = clBlack
  CustomTitleBar.ForegroundColor = clWhite
  CustomTitleBar.InactiveBackgroundColor = clBlack
  CustomTitleBar.InactiveForegroundColor = clWhite
  CustomTitleBar.ButtonForegroundColor = clWhite
  CustomTitleBar.ButtonBackgroundColor = clBlack
  CustomTitleBar.ButtonHoverForegroundColor = clWhite
  CustomTitleBar.ButtonHoverBackgroundColor = 1381653
  CustomTitleBar.ButtonPressedForegroundColor = clWhite
  CustomTitleBar.ButtonPressedBackgroundColor = 3487029
  CustomTitleBar.ButtonInactiveForegroundColor = clWhite
  CustomTitleBar.ButtonInactiveBackgroundColor = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Enabled = True
  GlassFrame.Top = 31
  KeyPreview = True
  Position = poScreenCenter
  StyleElements = [seFont, seClient]
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object pageControl: TPageControl
    Left = 0
    Top = 30
    Width = 584
    Height = 582
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Align = alClient
    TabOrder = 0
    ExplicitTop = 0
    ExplicitHeight = 361
  end
  object titleBar: TTitleBarPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 30
    CustomButtons = <>
    object SpeedButton1: TSpeedButton
      AlignWithMargins = True
      Left = 500
      Top = 3
      Width = 23
      Height = 24
      Hint = 'Increase Font Size'
      Align = alRight
      Caption = 'up'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton1Click
      ExplicitLeft = 380
      ExplicitTop = 8
      ExplicitHeight = 22
    end
    object SpeedButton2: TSpeedButton
      AlignWithMargins = True
      Left = 529
      Top = 3
      Width = 23
      Height = 24
      Hint = 'Decrease Font Size'
      Margins.Right = 32
      Align = alRight
      Caption = 'dn'
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton2Click
      ExplicitLeft = 420
      ExplicitTop = 8
      ExplicitHeight = 22
    end
  end
end
