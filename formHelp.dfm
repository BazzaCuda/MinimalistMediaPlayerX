object HelpForm: THelpForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Keyboard Functions'
  ClientHeight = 585
  ClientWidth = 742
  Color = 2829099
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object backPanel: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 585
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object buttonPanel: TPanel
      Left = 0
      Top = 572
      Width = 742
      Height = 13
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object shiftLabel: TLabel
        Left = 0
        Top = 0
        Width = 167
        Height = 13
        Align = alLeft
        Caption = 'press [F1] again to close this panel'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object moveLabel: TLabel
        Left = 167
        Top = 0
        Width = 434
        Height = 13
        Align = alLeft
        AutoSize = False
        Caption = 
          '      If the video window is resized or moved, this help panel w' +
          'ill stay attached to it'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitLeft = 223
      end
    end
    object RT: TRichEdit
      Left = 192
      Top = 176
      Width = 185
      Height = 89
      Font.Charset = ANSI_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Lines.Strings = (
        'RT')
      ParentFont = False
      TabOrder = 1
    end
  end
end
