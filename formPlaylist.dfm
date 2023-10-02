object PlaylistForm: TPlaylistForm
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsNone
  Caption = 'Playlist'
  ClientHeight = 585
  ClientWidth = 556
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
    Width = 556
    Height = 585
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object buttonPanel: TPanel
      Left = 0
      Top = 572
      Width = 556
      Height = 13
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      object shiftLabel: TLabel
        Left = 0
        Top = 0
        Width = 171
        Height = 13
        Align = alLeft
        Caption = 'press [P] again to close this window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object moveLabel: TLabel
        Left = 171
        Top = 0
        Width = 434
        Height = 13
        Align = alLeft
        AutoSize = False
        Caption = 
          '      If the video window is resized or moved, this playlist wil' +
          'l stay attached to it'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitLeft = 223
      end
    end
    object LB: TListBox
      Left = 224
      Top = 248
      Width = 121
      Height = 97
      ItemHeight = 13
      TabOrder = 1
    end
  end
end
