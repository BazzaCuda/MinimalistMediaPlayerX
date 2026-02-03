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
  OnResize = FormResize
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
      Top = 553
      Width = 742
      Height = 32
      Align = alBottom
      BevelOuter = bvNone
      Color = 2302755
      TabOrder = 0
      object shiftLabel: TLabel
        Left = 0
        Top = 18
        Width = 742
        Height = 14
        Align = alBottom
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Ctrl-[H] to close.   If the window is resized or moved, this hel' +
          'p panel will stay attached to it'
        Color = 4079166
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -12
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seFont, seBorder]
        ExplicitTop = 27
        ExplicitWidth = 689
      end
      object helpLabel: TLabel
        Left = 0
        Top = 0
        Width = 742
        Height = 18
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'Keyboard and mouse functions can all be used while this help pan' +
          'el is open.'
        Color = 2302755
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold, fsItalic]
        ParentColor = False
        ParentFont = False
        Transparent = False
        ExplicitWidth = 561
      end
    end
    object md1: TMarkdownViewer
      Left = 40
      Top = 168
      PrintMarginBottom = 2.000000000000000000
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      TabOrder = 1
    end
    object md2: TMarkdownViewer
      Left = 232
      Top = 168
      PrintMarginBottom = 2.000000000000000000
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      TabOrder = 2
    end
    object md3: TMarkdownViewer
      Left = 424
      Top = 168
      PrintMarginBottom = 2.000000000000000000
      PrintMarginLeft = 2.000000000000000000
      PrintMarginRight = 2.000000000000000000
      PrintMarginTop = 2.000000000000000000
      TabOrder = 3
    end
  end
end
