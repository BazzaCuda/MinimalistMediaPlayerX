object HelpForm: THelpForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Keyboard Functions'
  ClientHeight = 880
  ClientWidth = 742
  Color = 2829099
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object backPanel: TPanel
    Left = 0
    Top = 0
    Width = 742
    Height = 880
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object buttonPanel: TPanel
      Left = 0
      Top = 867
      Width = 742
      Height = 13
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        742
        13)
      object shiftLabel: TLabel
        Left = 0
        Top = 0
        Width = 223
        Height = 13
        Caption = 'press a [SHIFT] key again to close this window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object moveLabel: TLabel
        Left = 356
        Top = 0
        Width = 385
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 
          'If the video window is resized or moved, this help window will s' +
          'tay attached to it'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
    object lb: TListBox
      Left = 0
      Top = 0
      Width = 742
      Height = 867
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = 2829099
      ItemHeight = 13
      TabOrder = 1
      TabWidth = 50
      OnDrawItem = lbDrawItem
    end
  end
end
