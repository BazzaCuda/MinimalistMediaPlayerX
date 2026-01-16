object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MMP Config'
  ClientHeight = 361
  ClientWidth = 724
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object Label19: TLabel
    Left = 168
    Top = 231
    Width = 353
    Height = 15
    Caption = 
      'some file content might be recoverable until Windows overwrites ' +
      'it'
  end
  object pageControl: TPageControl
    Left = 140
    Top = 0
    Width = 584
    Height = 361
    ActivePage = tsSlideshowIntervalMs
    Align = alClient
    TabOrder = 0
    OnChange = pageControlChange
    object tsGeneral: TTabSheet
      Caption = 'General'
      object Label2: TLabel
        Left = 128
        Top = 22
        Width = 445
        Height = 32
        AutoSize = False
        Caption = 
          'Determines whether the Ctrl-[A] About Box will connect to the in' +
          'ternet and update MinimalistMediaPlayer.exe to the latest versio' +
          'n. See note below**'
        WordWrap = True
      end
      object Label1: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label3: TLabel
        Left = 128
        Top = 70
        Width = 417
        Height = 47
        AutoSize = False
        Caption = 
          'Determines whether, for video files, MMP should launch directly ' +
          'into the Audio'#160'&&'#160'Video'#160'Timeline'#160'Editor. This is useful if you h' +
          'ave a lot of video editing to do for a number of video files ove' +
          'r several MMP sessions'
        WordWrap = True
      end
      object Label6: TLabel
        Left = 128
        Top = 134
        Width = 445
        Height = 44
        AutoSize = False
        Caption = 
          'Determines whether, for image files, MMP should launch directly ' +
          'into the Image'#160'&&'#160'Thumbnail'#160'Browser, rather than display the ima' +
          'ge in the main media window'
        WordWrap = True
      end
      object Label7: TLabel
        Left = 12
        Top = 258
        Width = 552
        Height = 49
        Alignment = taCenter
        AutoSize = False
        Caption = 
          '** when MMP updates the .exe, it'#39's recommended that you also unz' +
          'ip the latest DLLs from the zip file.  However, MMP won'#39't overwr' +
          'ite your existing DLLs or your copy of ffmpeg.exe in case you'#39're' +
          ' deliberately running older versions because they suit the speci' +
          'fications of your Windows machine better.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        StyleElements = [seClient, seBorder]
      end
      object Label11: TLabel
        Left = 128
        Top = 184
        Width = 445
        Height = 62
        AutoSize = False
        Caption = 
          'Determines whether exiting the Image'#160'&&'#160'Thumbnail'#160'Browser also e' +
          'xits MMP. With this setting and Open Image in Browser you can op' +
          'erate MMP as [in effect] two applications in one: the main media' +
          ' window for playing (and editing) audio and video files, and the' +
          ' Image && Thumbnail Browser for managing your image library'
        WordWrap = True
      end
      object chbAutoUpdate: TCheckBox
        Left = 12
        Top = 22
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Auto Update'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chbAutoUpdateClick
      end
      object chbStartInEditor: TCheckBox
        Left = 12
        Top = 70
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Start in Editor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = chbStartInEditorClick
      end
      object chbOpenImage: TCheckBox
        Left = 12
        Top = 128
        Width = 101
        Height = 29
        Alignment = taLeftJustify
        Caption = 'Open Image in Browser'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        WordWrap = True
        OnClick = chbOpenImageClick
      end
      object chbExitBrowser: TCheckBox
        Left = 12
        Top = 184
        Width = 101
        Height = 29
        Alignment = taLeftJustify
        Caption = 'Exit Browser = Exit MMP'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        WordWrap = True
        OnClick = chbExitBrowserClick
      end
    end
    object tsDeletions: TTabSheet
      Caption = 'File Deletion'
      ImageIndex = 1
      object Label5: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label13: TLabel
        Left = 16
        Top = 3
        Width = 545
        Height = 51
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'When you issue commands such as [Del]ete,  Ctrl-[D]elete and Ctr' +
          'l-[K]eep/Delete,'#13#10'MMP checks whether you have enabled that opera' +
          'tion here. '#13#10'If you haven'#39't, MMP will not proceed to the delete ' +
          'confirmation dialog**'
        WordWrap = True
      end
      object Label14: TLabel
        Left = 18
        Top = 62
        Width = 101
        Height = 15
        Caption = '[Del]ete can delete:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 27
        Top = 135
        Width = 521
        Height = 30
        Alignment = taCenter
        AutoSize = False
        Caption = 
          '* deletion operations only ever affect the files in the current ' +
          'playlist folder'#13#10'MMP never touches the contents of any subfolder' +
          's'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        StyleElements = [seClient, seBorder]
      end
      object Label16: TLabel
        Left = 64
        Top = 280
        Width = 468
        Height = 30
        Alignment = taCenter
        Caption = 
          '** Note: Ctrl-Shift-[C]leanup goes straight to the delete confir' +
          'mation dialog'#13#10'because deleting your Timeline Editing files won'#39 +
          't affect your original audio or video files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label17: TLabel
        Left = 160
        Top = 200
        Width = 288
        Height = 15
        AutoSize = False
        Caption = 'move deleted files straight to the Windows Recycle Bin'
      end
      object Label18: TLabel
        Left = 160
        Top = 223
        Width = 353
        Height = 15
        AutoSize = False
        Caption = 
          'some file content might be recoverable until Windows overwrites ' +
          'it'
      end
      object Label20: TLabel
        Left = 160
        Top = 246
        Width = 368
        Height = 15
        AutoSize = False
        Caption = 
          'file name and contents won'#39't be recoverable even with specialist' +
          ' tools'
      end
      object GroupBox1: TGroupBox
        Left = 12
        Top = 50
        Width = 549
        Height = 123
        TabOrder = 9
      end
      object RadioGroup1: TRadioGroup
        Left = 12
        Top = 177
        Width = 549
        Height = 95
        Caption = 'Deletion Method'
        TabOrder = 5
      end
      object chbAudio: TCheckBox
        Left = 123
        Top = 62
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Audio files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chbAudioClick
      end
      object chbVideo: TCheckBox
        Left = 123
        Top = 85
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Video files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = chbVideoClick
      end
      object chbImage: TCheckBox
        Left = 123
        Top = 108
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Image files'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = chbImageClick
      end
      object chbFolderDelete: TCheckBox
        Left = 251
        Top = 62
        Width = 297
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Ctrl-[Del]ete can delete all the files in a folder*'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = chbFolderDeleteClick
      end
      object chbKeepDelete: TCheckBox
        Left = 251
        Top = 85
        Width = 297
        Height = 40
        Alignment = taLeftJustify
        Caption = 
          'Ctrl-[K]eep/Delete can delete all the files in a folder*'#13#10'except' +
          ' those marked as [K]eep files (start with a "!")'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 4
        OnClick = chbKeepDeleteClick
      end
      object rbRecycle: TRadioButton
        Left = 27
        Top = 200
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Recycle'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 6
        TabStop = True
        OnClick = rbRecycleClick
      end
      object rbStandard: TRadioButton
        Left = 27
        Top = 223
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Standard Delete'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 7
        TabStop = True
        OnClick = rbStandardClick
      end
      object rbShred: TRadioButton
        Left = 27
        Top = 246
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Shred'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 8
        TabStop = True
        OnClick = rbShredClick
      end
    end
    object tsPlaylist: TTabSheet
      Caption = 'Playlist'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ImageIndex = 2
      ParentFont = False
      object Label4: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label21: TLabel
        Left = 49
        Top = 2
        Width = 478
        Height = 30
        Alignment = taCenter
        Caption = 
          'When you launch MMP by [double-]clicking on a file in your file ' +
          'explorer software'#13#10'MMP builds a playlist from all the supported ' +
          'file extensions in that folder and starts playing'
      end
      object Label23: TLabel
        Left = 28
        Top = 185
        Width = 521
        Height = 30
        Alignment = taCenter
        AutoSize = False
        Caption = 
          '* deletion operations only ever affect the files in the current ' +
          'playlist folder'#13#10'MMP never touches the contents of any subfolder' +
          's'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        StyleElements = [seClient, seBorder]
      end
      object Label24: TLabel
        Left = 27
        Top = 274
        Width = 521
        Height = 24
        Alignment = taCenter
        AutoSize = False
        Caption = 'Note: MMP will always ignore <drive>:\Windows\WinSxS\'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        StyleElements = [seClient, seBorder]
      end
      object Bevel1: TBevel
        Left = 92
        Top = 220
        Width = 391
        Height = 1
      end
      object Label47: TLabel
        Left = 10
        Top = 45
        Width = 106
        Height = 15
        Caption = 'Next Folder on End'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label22: TLabel
        Left = 10
        Top = 84
        Width = 121
        Height = 15
        Caption = 'Next Folder on Empty'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label48: TLabel
        Left = 10
        Top = 235
        Width = 110
        Height = 15
        Caption = 'Allow into Windows'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label49: TLabel
        Left = 159
        Top = 84
        Width = 359
        Height = 30
        Caption = 
          'MMP should move to the next folder and start playing a new playl' +
          'ist'#13#10'when you empty the current playlist folder using one of:'
      end
      object chbNextFolderOnEnd: TCheckBox
        Left = 156
        Top = 45
        Width = 391
        Height = 33
        Alignment = taLeftJustify
        Caption = 
          'MMP should move to the next folder and start playing a new playl' +
          'ist'#13#10'at the end of playing the current playlist'
        TabOrder = 0
        OnClick = chbNextFolderOnEndClick
      end
      object chbNextFolderOnEmpty: TCheckBox
        Left = 199
        Top = 117
        Width = 348
        Height = 63
        Alignment = taLeftJustify
        Caption = 
          '   a) Ctrl-[Del]ete all the remaining files in a folder* '#13#10'   b)' +
          ' Ctrl-[K]eep/Delete all except those marked as [K]eep files'#13#10'   ' +
          '         ([K]eep files start with a "!")'#13#10'   c) [Del]ete the las' +
          't remaining file in a folder'
        TabOrder = 1
        OnClick = chbNextFolderOnEmptyClick
      end
      object chbAllowIntoWindows: TCheckBox
        Left = 156
        Top = 235
        Width = 391
        Height = 33
        Alignment = taLeftJustify
        Caption = 
          'MMP is allowed to look for media files in <drive>:\Windows\'#13#10'Not' +
          ' recommended except for one-off occasions'
        TabOrder = 2
        OnClick = chbAllowIntoWindowsClick
      end
    end
    object tsPlaylistFilter: TTabSheet
      Caption = 'Playlist Filter'
      ImageIndex = 7
      object Label25: TLabel
        Left = 76
        Top = 2
        Width = 424
        Height = 30
        Alignment = taCenter
        Caption = 
          'You can set MMP'#39's main media window to only play media files of ' +
          'a certain type'#13#10'Ctrl-[P] turns Playlist Filtering on and off'
      end
      object Label27: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label28: TLabel
        Left = 170
        Top = 72
        Width = 393
        Height = 201
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'When you first launch MMP'#13#10'it will build a playlist containing a' +
          'll the supported media files in that folder'#13#10#13#10'MMP will always p' +
          'lay the media file you clicked to launch it'#13#10#13#10'If you then turn ' +
          'on Playlist Filtering'#13#10'MMP will ignore any files in the playlist' +
          ' that don'#39't match your filter'#13#10#13#10'If you [or MMP] move to another' +
          ' folder'#13#10'the new playlist will only contain files that match you' +
          'r filter'#13#10#13#10'With the filter format set to Image'#13#10'Ctrl-[P]laylist' +
          ' Filter on/off is the same as [Spacebar] slideshow on/off'
        WordWrap = True
      end
      object Label26: TLabel
        Left = 353
        Top = 51
        Width = 26
        Height = 15
        Caption = 'Note'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsUnderline]
        ParentFont = False
      end
      object RadioGroup2: TRadioGroup
        Left = 15
        Top = 105
        Width = 137
        Height = 138
        Caption = ' Playlist  Filter  Format '
        TabOrder = 0
      end
      object rbFilterAll: TRadioButton
        Left = 31
        Top = 129
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'All'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        TabStop = True
        OnClick = rbFilterAllClick
      end
      object rbFilterAudio: TRadioButton
        Left = 31
        Top = 152
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Audio'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        TabStop = True
        OnClick = rbFilterAudioClick
      end
      object rbFilterVideo: TRadioButton
        Left = 31
        Top = 175
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Video'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
        TabStop = True
        OnClick = rbFilterVideoClick
      end
      object rbFilterImage: TRadioButton
        Left = 31
        Top = 221
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Image'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        TabStop = True
        OnClick = rbFilterImageClick
      end
      object rbFilterAudioVideo: TRadioButton
        Left = 31
        Top = 198
        Width = 113
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Audio && Video'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        TabStop = True
        OnClick = rbFilterAudioVideoClick
      end
    end
    object tsUserFolders: TTabSheet
      Caption = 'User Folders'
      ImageIndex = 3
      object Label8: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label30: TLabel
        Left = 55
        Top = 17
        Width = 466
        Height = 75
        Alignment = taCenter
        Caption = 
          'Up to 15 user folders can be set for moving your files with one ' +
          'keystroke'#13#10'Each folder can be specified as either the fully-qual' +
          'ified path to a folder, e.g. C:\Pictures\'#13#10'or it can be the name' +
          ' of a subfolder within the Base Folder'#13#10'You can mix-and-match th' +
          'ese settings according to your requirements'#13#10'MMP will create a f' +
          'older for you the first time you use its corresponding key'
      end
      object Label31: TLabel
        Left = 209
        Top = 1
        Width = 159
        Height = 15
        Caption = 'Image && Thumbnail Browser'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object btnBaseFolder: TSpeedButton
        Tag = 1
        Left = 523
        Top = 104
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnSaved: TSpeedButton
        Tag = 4
        Left = 523
        Top = 133
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnMoved: TSpeedButton
        Tag = 3
        Left = 344
        Top = 133
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnCopied: TSpeedButton
        Tag = 2
        Left = 162
        Top = 133
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object Label32: TLabel
        Left = 97
        Top = 158
        Width = 381
        Height = 15
        Caption = 
          '* also used by Ctrl/Shift-[M] and Shift-[S] in MMP'#39's main media ' +
          'window'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label33: TLabel
        Left = 20
        Top = 286
        Width = 536
        Height = 30
        Alignment = taCenter
        Caption = 
          'If you use one of these folders without configuring it'#13#10'MMP will' +
          ' assume that it'#39's a subfolder of the Base Folder and will use a ' +
          'default name for that subfolder'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object btnF1: TSpeedButton
        Tag = 5
        Left = 162
        Top = 178
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF2: TSpeedButton
        Tag = 6
        Left = 162
        Top = 206
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF3: TSpeedButton
        Tag = 7
        Left = 162
        Top = 234
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF4: TSpeedButton
        Tag = 8
        Left = 163
        Top = 263
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF5: TSpeedButton
        Tag = 9
        Left = 345
        Top = 178
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF7: TSpeedButton
        Tag = 11
        Left = 347
        Top = 234
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF8: TSpeedButton
        Tag = 12
        Left = 347
        Top = 263
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF9: TSpeedButton
        Tag = 13
        Left = 523
        Top = 178
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF6: TSpeedButton
        Tag = 10
        Left = 346
        Top = 206
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF10: TSpeedButton
        Tag = 14
        Left = 523
        Top = 206
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF11: TSpeedButton
        Tag = 15
        Left = 523
        Top = 234
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object btnF12: TSpeedButton
        Tag = 16
        Left = 523
        Top = 263
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnBaseFolderClick
      end
      object edtBaseFolder: TLabeledEdit
        Tag = 1
        Left = 176
        Top = 103
        Width = 341
        Height = 23
        EditLabel.Width = 152
        EditLabel.Height = 23
        EditLabel.Caption = 'Base Folder (fully-qualified)'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        TabOrder = 0
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtBaseFolderChange
      end
      object edtSaved: TLabeledEdit
        Tag = 4
        Left = 421
        Top = 133
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "Saved"'
        EditLabel.Width = 39
        EditLabel.Height = 23
        EditLabel.Caption = 'Saved*'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtMoved: TLabeledEdit
        Tag = 3
        Left = 243
        Top = 133
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "Moved"'
        EditLabel.Width = 44
        EditLabel.Height = 23
        EditLabel.Caption = 'Moved*'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtCopied: TLabeledEdit
        Tag = 2
        Left = 61
        Top = 133
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "Copied"'
        EditLabel.Width = 38
        EditLabel.Height = 23
        EditLabel.Caption = 'Copied'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF1: TLabeledEdit
        Tag = 5
        Left = 61
        Top = 178
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F1'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF2: TLabeledEdit
        Tag = 6
        Left = 61
        Top = 206
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder2"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F2'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 5
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF3: TLabeledEdit
        Tag = 7
        Left = 61
        Top = 234
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder3"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F3'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 6
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF4: TLabeledEdit
        Tag = 8
        Left = 60
        Top = 263
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder4"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F4'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 7
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF5: TLabeledEdit
        Tag = 9
        Left = 243
        Top = 178
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder5"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F5'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 8
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF7: TLabeledEdit
        Tag = 11
        Left = 244
        Top = 234
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder7"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F7'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 10
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF8: TLabeledEdit
        Tag = 12
        Left = 244
        Top = 263
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder8"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F8'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 11
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF9: TLabeledEdit
        Tag = 13
        Left = 421
        Top = 178
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder9"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F9'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 12
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF6: TLabeledEdit
        Tag = 10
        Left = 243
        Top = 206
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder6"'
        EditLabel.Width = 13
        EditLabel.Height = 23
        EditLabel.Caption = 'F6'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 9
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF10: TLabeledEdit
        Tag = 14
        Left = 421
        Top = 206
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder10"'
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F10'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 13
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF11: TLabeledEdit
        Tag = 15
        Left = 421
        Top = 234
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder11"'
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F11'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 14
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
      object edtF12: TLabeledEdit
        Tag = 16
        Left = 421
        Top = 263
        Width = 96
        Height = 23
        Hint = 'Defaults to subfolder "folder12"'
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F12'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 15
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtCopiedChange
      end
    end
    object tsExternalApps: TTabSheet
      Caption = 'External Apps'
      ImageIndex = 4
      object Label9: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label29: TLabel
        Left = 28
        Top = 2
        Width = 517
        Height = 75
        Alignment = taCenter
        Caption = 
          'MMP'#39's main media window provides three function keys'#13#10'which you ' +
          'can use to launch the current media file in another application'#13 +
          #10'MMP defaults to three popular applications'#13#10#13#10'MMP will start th' +
          'e application passing the full path to the media file as a comma' +
          'nd line parameter'
      end
      object btnAppF10: TSpeedButton
        Left = 531
        Top = 112
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnAppF10Click
      end
      object btnAppF11: TSpeedButton
        Left = 531
        Top = 152
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnAppF10Click
      end
      object btnAppF12: TSpeedButton
        Left = 531
        Top = 198
        Width = 23
        Height = 22
        Glyph.Data = {
          0E060000424D0E06000000000000360000002800000016000000160000000100
          180000000000D8050000130B0000130B00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000D3EFF94AC0EE4EC1ED4DC1EE4BC1EE4AC0EE48C0EE47C0EE46BE
          ED45BDEC44BBEB44B9EA44B7E843B6E742B6E741B6E742B6E742B6E742B6E741
          B5E73BB3E7D6EDF70000B8EBFC6ED9FE6ED9FD67D7FD61D6FD5BD4FD56D2FE51
          D1FE4CD0FE48CFFE42CEFE3DCCFE3ACBFF35CAFF32CAFF30C9FF2EC9FF2EC9FF
          2EC9FF31C9FF28C7FF9BE2FC0000BFEEFE6FD7FC6FD8FD69D7FB63D4FC5ED4FB
          5AD2FC55D1FD51CFFD4DCEFD48CDFD44CCFC41CBFC3DCBFD3CCAFE39C9FE38C9
          FE38C9FE38CAFE39C9FE30C7FEA1E6FE0000C0EEFE71D9FC72D9FD6CD7FB66D6
          FB61D4FC5DD3FB58D2FD54D1FD50D0FD4CCFFD48CEFD45CDFC43CCFC40CBFD3F
          CBFD3DCBFE3ECBFD3ECBFE3FCBFD36C9FCA4E6FE0000C1EEFE74D9FC75DAFD6F
          D8FD69D7FB64D5FC60D3FC5BD2FB57D2FC53D1FD50D0FD4CCFFD49CEFD47CDFD
          45CDFC44CCFC43CCFC42CCFC43CCFC44CCFC3BCAFCA6E6FE0000C3EFFE77DAFC
          78DBFD73D9FD6ED8FC67D6FB63D4FC5ED3FC5BD2FB57D2FC55D1FD50D0FD4FCF
          FD4CCEFD4ACEFD49CEFD48CEFD48CEFD48CEFD49CEFD40CCFCA9E7FE0000C4F0
          FE7BDBFC7CDCFD77DAFD72D9FD6CD7FC67D5FB62D4FC5FD3FC5BD3FB59D2FC55
          D1FD53D0FD51D0FD4FCFFD4ECFFD4DCFFD4DCFFD4ECFFD4ECFFD46CDFCABE8FE
          0000C6F0FE7EDDFD7FDDFC7ADBFD75DAFD70D8FD6BD7FB66D5FB63D4FC5FD4FC
          5DD3FB5AD2FC58D1FD56D1FD54D1FD53D0FD53D1FD53D0FD53D1FD54D0FD4BCE
          FCADE9FE0000C8F0FE82DEFE84DEFC7FDCFC7ADCFD75DAFD71D8FD6CD7FB68D6
          FB65D5FC62D4FC5FD4FB5DD3FB5BD3FB5AD2FC59D2FC58D1FD58D2FD58D1FD59
          D2FC50D0FCB0E9FC0000CAF1FE87DFFE88DFFE83DEFC7EDDFC7ADBFD76DAFD71
          D9FD6ED7FC6AD6FB67D6FB63D5FC62D4FC60D4FC5FD4FB5ED3FB5ED3FB5DD3FB
          5ED3FB5ED3FB56D1FBB1EAFD0000CCF1FE8BE1FE8CE1FE88DFFE83DEFD7FDDFC
          7BDBFD77DBFD73DAFD70D9FC6CD7FC69D6FB67D5FB66D5FB64D5FC63D5FC62D4
          FC63D4FC62D4FC63D5FC5BD2FBB5EBFD0000D1F3FE8DE1FD8FE2FD8BE0FD86DF
          FE82DEFD7FDCFC7ADBFC78DAFC76DAFD73D9FD70D8FC6ED8FC6CD6FC6AD7FB69
          D7FB69D6FB68D6FB69D6FB6AD7FB62D4FBB7EBFD0000B5EBFCA4EAFF99E7FF97
          E7FF95E6FF93E5FF90E4FF8DE4FF85E1FF76DBFD78DAFC75D9FD73DAFD71D8FD
          70D8FC70D8FD6FD8FD6FD8FD70D8FD71D9FD6BD7FDBBEDFE000084DAF9E0F7F8
          FFFFF7FFFEF5FFFCF2FFF9EFFFF6EDFFF4EAF5F3EEBBEEFA8AE2FE89E1FE86E0
          FE82DFFE7EDEFE7ADDFD76DBFD72DAFD6ED9FD6AD7FD59D1FBABE1F600008DDE
          FA00B1F302B3F304B2F205B1F106B2F108B0F009B0EF00ABEC38BBEDA1DEF69C
          DBF49CDAF49DDAF39EDAF39FDAF2A0D9F2A0D9F1A1D8F1A1D8F0A8DAF0FFFFFE
          00008EDEFA03B5F413B8F414B8F314B7F216B6F117B5EF0FB1EE28B8EEFBFDFE
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF0000EEFAFD51CCF750CBF752CBF653CAF553CAF552C8F46DCFF4ECF9
          FDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000}
        OnClick = btnAppF10Click
      end
      object Label34: TLabel
        Left = 93
        Top = 286
        Width = 390
        Height = 30
        Alignment = taCenter
        Caption = 
          'Note: this functionality is not available in the Image && Thumbn' +
          'ail Browser'#13#10'as all the function keys are assigned to user folde' +
          'rs'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object edtAppF10: TLabeledEdit
        Left = 28
        Top = 111
        Width = 497
        Height = 23
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F10'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        TabOrder = 0
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtAppF10Change
      end
      object edtAppF11: TLabeledEdit
        Left = 28
        Top = 151
        Width = 497
        Height = 23
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F11'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        TabOrder = 1
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtAppF10Change
      end
      object edtAppF12: TLabeledEdit
        Left = 28
        Top = 197
        Width = 497
        Height = 23
        EditLabel.Width = 20
        EditLabel.Height = 23
        EditLabel.Caption = 'F12'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        TabOrder = 2
        Text = ''
        StyleElements = [seClient, seBorder]
        OnChange = edtAppF10Change
      end
    end
    object tsQuickRename: TTabSheet
      Caption = 'Quick Rename'
      ImageIndex = 5
      object Label10: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label35: TLabel
        Left = 87
        Top = 10
        Width = 414
        Height = 120
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'MMP'#39's main media window provides four function keys and [K]'#13#10'whi' +
          'ch you can use to quickly rename the current media file'#13#10#13#10'[K]ee' +
          'p renames the file with a leading ! (and a space)'#13#10'to bring all ' +
          'such files to top of the folder listing in your file explorer so' +
          'ftware'#13#10#13#10'You can then, if you wish, use Ctrl-[K]eep/delete to d' +
          'elete all files in the folder*'#13#10'except those you designated as [' +
          'K]eep'
      end
      object Label36: TLabel
        Left = 79
        Top = 284
        Width = 429
        Height = 30
        Alignment = taCenter
        Caption = 
          'Note: although [K] and Ctrl-[K] are available in the Image && Th' +
          'umbnail Browser'#13#10'the prefix/suffix keys aren'#39't as all the functi' +
          'on keys can be assigned to user folders'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label37: TLabel
        Left = 28
        Top = 132
        Width = 521
        Height = 30
        Alignment = taCenter
        AutoSize = False
        Caption = 
          '* deletion operations only ever affect the files in the current ' +
          'playlist folder'#13#10'MMP never touches the contents of any subfolder' +
          's'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        WordWrap = True
        StyleElements = [seClient, seBorder]
      end
      object Label38: TLabel
        Left = 122
        Top = 176
        Width = 332
        Height = 30
        Alignment = taCenter
        Caption = 
          'You can use F1 - F4 to set up to three prefixes and one suffix'#13#10 +
          'Any leading and/or trailing spaces you include will be recorded'
      end
      object GroupBox2: TGroupBox
        Left = 40
        Top = 0
        Width = 521
        Height = 168
        TabOrder = 4
      end
      object edtPrefixF1: TLabeledEdit
        Left = 129
        Top = 218
        Width = 126
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 50
        EditLabel.Height = 23
        EditLabel.Caption = 'F1 Prefix'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = ''
        OnChange = edtPrefixF1Change
      end
      object edtPrefixF2: TLabeledEdit
        Left = 127
        Top = 250
        Width = 126
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 50
        EditLabel.Height = 23
        EditLabel.Caption = 'F2 Prefix'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Text = ''
        OnChange = edtPrefixF1Change
      end
      object edtPrefixF3: TLabeledEdit
        Left = 323
        Top = 218
        Width = 126
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 50
        EditLabel.Height = 23
        EditLabel.Caption = 'F3 Prefix'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        Text = ''
        OnChange = edtPrefixF1Change
      end
      object edtSuffixF4: TLabeledEdit
        Left = 323
        Top = 250
        Width = 126
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 50
        EditLabel.Height = 23
        EditLabel.Caption = 'F4 Suffix'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Text = ''
        OnChange = edtPrefixF1Change
      end
    end
    object tsScaleFactor: TTabSheet
      Caption = 'Scale Factor'
      ImageIndex = 8
      object Label41: TLabel
        Left = 148
        Top = 293
        Width = 142
        Height = 15
        Caption = 'Scale Factor (percentage)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label42: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label43: TLabel
        Left = 20
        Top = 16
        Width = 543
        Height = 217
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'MMP is developed on a large, wide-screen monitor with a resoluti' +
          'on of 3440 x 1440'#13#10#13#10'While reasonable attempts are always made t' +
          'o ensure that popup dialog boxes (in particular) are a suitable ' +
          'size for typical monitors, it'#39's not always possible to suit ever' +
          'ybody'#39's monitors'#13#10#13#10'If you find that the Delete Confirmation dia' +
          'log looks too big on your monitor'#13#10'you can scale down its design' +
          '-time size'#13#10#13#10'The recommended setting is 90% of the design-time ' +
          'size'#13#10#13#10'Obviously, you can experiment to find a size you'#39're comf' +
          'ortable with'#13#10#13#10'The minimum setting is 50% and the maximum is 10' +
          '0%'
        WordWrap = True
      end
      object spinScaleFactor: TSpinEdit
        Left = 296
        Top = 289
        Width = 51
        Height = 24
        MaxValue = 999999
        MinValue = 0
        TabOrder = 0
        Value = 100
        OnChange = spinScaleFactorChange
      end
      object btnScaleFactorDefault: TButton
        Left = 352
        Top = 288
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 1
        OnClick = btnScaleFactorDefaultClick
      end
    end
    object tsSlideshowIntervalMs: TTabSheet
      Caption = 'Slideshows'
      ImageIndex = 9
      object Label44: TLabel
        Left = 128
        Top = 293
        Width = 181
        Height = 15
        Caption = 'Slideshow Interval (milliseconds)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label45: TLabel
        Left = 0
        Top = 316
        Width = 576
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
        ExplicitWidth = 380
      end
      object Label46: TLabel
        Left = 0
        Top = 16
        Width = 574
        Height = 266
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'MMP incorporates two image slideshows:'#13#10'one in the main media wi' +
          'ndow and the other in the Image && Thumbnail Browser'#13#10#13#10'Both ope' +
          'rate slightly differently. This is deliberate so that you can ch' +
          'oose'#13#10'which of the two slideshows suits your particular requirem' +
          'ents at any given time'#13#10#13#10'Depending on your playlist settings, t' +
          'he main window slideshow can'#13#10'cycle through all the images on an' +
          ' entire drive'#13#10#13#10'In the Image && Thumbnail Browser, the slidesho' +
          'w will continuously'#13#10'loop through all the images in one folder.'#13 +
          #10'If you want the slideshow to move to a different folder you can' +
          ' use the appropriate folder navigation keys'#13#10'You can also slow d' +
          'own and speed up the slideshow using the [\] and [/] keys '#13#10#13#10'Bo' +
          'th slideshows use the same setting for the interval between imag' +
          'es'#13#10'The minimum (fastest) is 100ms - There is no maximum'
        WordWrap = True
      end
      object spinSlideshowIntervalMs: TSpinEdit
        Left = 316
        Top = 289
        Width = 51
        Height = 24
        MaxValue = 999999
        MinValue = 0
        TabOrder = 0
        Value = 100
        OnChange = spinSlideshowIntervalMsChange
      end
      object btnSlideshowIntervalMsDefault: TButton
        Left = 372
        Top = 288
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 1
        OnClick = btnSlideshowIntervalMsDefaultClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Repeat Delay'
      ImageIndex = 6
      object Label12: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label39: TLabel
        Left = 11
        Top = 3
        Width = 555
        Height = 285
        Caption = 
          'This setting affects not only the Image && Thumbnail Browser, bu' +
          't also MMP'#39's main media window when it'#39's displaying many images ' +
          'in succession.'#13#10#13#10'When you hold down the left arrow key or the r' +
          'ight arrow key, MMP will cycle through the images at the rate of' +
          ' one every 100ms.'#13#10#13#10'This seems to be a reasonable speed for the' +
          ' average image collection. However, if you have a mixture of [e.' +
          'g.] 1MB images and 20MB images in the same folder, you may find ' +
          'that the 20MB image hasn'#39't finished rendering to the screen befo' +
          're the next image file gets loaded. Consequently it can appear t' +
          'hat MMP has skipped past it.'#13#10#13#10'The problem is, if you drop the ' +
          'speed down to [e.g.] 500ms, this might be appropriate for the 20' +
          'MB images but now the 1MB images progress too slowly.'#13#10#13#10'There a' +
          're two solutions to this:'#13#10'1) be aware that when you hold down a' +
          'n arrow key you are not guaranteed to see every image. If you mu' +
          'st see every image, tap the arrow keys rather than holding them ' +
          'down.'#13#10'2) you can experiment with the following setting to find ' +
          'a delay between images that suits your machine and the average s' +
          'ize of your images.'
        WordWrap = True
      end
      object Label40: TLabel
        Left = 144
        Top = 293
        Width = 152
        Height = 15
        Caption = 'Repeat Delay (milliseconds)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object spinImageDelayMs: TSpinEdit
        Left = 301
        Top = 289
        Width = 51
        Height = 24
        MaxValue = 999999
        MinValue = 0
        TabOrder = 0
        Value = 100
        OnChange = spinImageDelayMsChange
      end
      object btnRepeatDelayDefault: TButton
        Left = 357
        Top = 288
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 1
        OnClick = btnRepeatDelayDefaultClick
      end
    end
    object tsEditing: TTabSheet
      Caption = 'Editing'
      ImageIndex = 10
      object Label50: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label51: TLabel
        Left = 131
        Top = 30
        Width = 445
        Height = 32
        AutoSize = False
        Caption = 
          'Determines whether MMP will autoplay the [edited] audio or video' +
          ' file after successfully exporting and joining the segments from' +
          ' your Timeline*'
        WordWrap = True
      end
      object Label52: TLabel
        Left = 205
        Top = 4
        Width = 167
        Height = 15
        Caption = 'Audio && Video Timeline Editor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label53: TLabel
        Left = 20
        Top = 73
        Width = 543
        Height = 212
        AutoSize = False
        Caption = 
          '1. MMP will play the [edited] file in a new instance of MMP'#13#10#13#10'2' +
          '. In the new MMP, the [edited] file will be the only file in the' +
          ' playlist'#13#10#13#10'3. Playback will stop when MMP reaches the end of t' +
          'he file and will ignore '#13#10'    the nextFolderOnEnd [of playlist] ' +
          'setting so that you can further review your edits'#13#10#13#10'4. You can ' +
          '[Esc]ape to close the second MMP and return to your editing, or'#13 +
          #10'    You might Ctrl-[M]ove your successful edit to an e.g. "Fina' +
          'l Edits" folder, or'#13#10'    You can open the Audio && Video Timelin' +
          'e Editor in the second MMP and edit your [edited] file'#13#10'    to c' +
          'reate an "[edited] [edited]" file'#13#10#13#10'5. If you [Del]ete the [edi' +
          'ted] file with nextFolderOnEmpty=no, '#13#10'    the second MMP will c' +
          'lose and return you to your editing session'
        WordWrap = True
      end
      object Label54: TLabel
        Left = 41
        Top = 288
        Width = 494
        Height = 30
        Alignment = taCenter
        Caption = 
          '* Shift-[E] will also play the [edited] version of an audio or v' +
          'ideo file in a new instance of MMP'#13#10'Shift-[E] is available while' +
          ' the Audio && Video Timeline Editor is open'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object chbPlayEdited: TCheckBox
        Left = 20
        Top = 30
        Width = 101
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Play Edited'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chbPlayEditedClick
      end
    end
    object tsKeyframes: TTabSheet
      Caption = 'Keyframes'
      ImageIndex = 12
      object Label57: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label58: TLabel
        Left = 205
        Top = 4
        Width = 167
        Height = 15
        Caption = 'Audio && Video Timeline Editor'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label59: TLabel
        Left = 3
        Top = 24
        Width = 558
        Height = 90
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'Determines whether, for video files, the editor will always open' +
          ' with keyframes turned on. '#13#10'Keyframes help you to more accurate' +
          'ly determine where the start of each segment should be cut.'#13#10#13#10'K' +
          'eyframe processing is now so fast that keeping keyframes turned ' +
          'on permanently is a viable option.'#13#10#13#10'The vertical cursor will c' +
          'hange color to indicate how close it is the previous keyframe:'
        WordWrap = True
      end
      object lblWhite: TLabel
        Left = 58
        Top = 120
        Width = 32
        Height = 15
        Alignment = taCenter
        Caption = 'white'
        Color = clWhite
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lblYellow: TLabel
        Left = 58
        Top = 141
        Width = 36
        Height = 15
        Alignment = taCenter
        Caption = 'yellow'
        Color = clYellow
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lblPurple: TLabel
        Left = 58
        Top = 162
        Width = 36
        Height = 15
        Alignment = taCenter
        Caption = 'purple'
        Color = clFuchsia
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object Label60: TLabel
        Left = 175
        Top = 120
        Width = 377
        Height = 15
        Alignment = taCenter
        Caption = 
          'the previous keyframe is more than 1.0 seconds prior to this vid' +
          'eo point'
      end
      object Label61: TLabel
        Left = 175
        Top = 141
        Width = 351
        Height = 15
        Alignment = taCenter
        Caption = 
          'the previous keyframe is 0.5 to 1.0 seconds prior to this video ' +
          'point'
      end
      object Label62: TLabel
        Left = 175
        Top = 162
        Width = 368
        Height = 15
        Alignment = taCenter
        Caption = 
          'the previous keyframe is less than 0.5 seconds prior to this vid' +
          'eo point'
      end
      object Label63: TLabel
        Left = 0
        Top = 183
        Width = 573
        Height = 76
        Alignment = taCenter
        AutoSize = False
        Caption = 
          'FFmpeg will cut on the closet seek-point keyframe it can find'#13#10'p' +
          'rior to where you set your cut segment to start*'#13#10#13#10'Rough cuts w' +
          'ork really welll. If you prefer more accurate cuts for a particu' +
          'lar video'#13#10'work with FFmpeg by using keyframes in the MMP editor'
        WordWrap = True
      end
      object Label64: TLabel
        Left = 46
        Top = 263
        Width = 484
        Height = 30
        Alignment = taCenter
        Caption = 
          '* FFmpeg will always make a cut segment at least as long as you ' +
          'set it to be.'#13#10'A segment will always contain at least what you w' +
          'ant it to contain, give or take a few frames'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object Label65: TLabel
        Left = 107
        Top = 120
        Width = 60
        Height = 15
        AutoSize = False
        Caption = 'rough cut:'
        WordWrap = True
      end
      object Label66: TLabel
        Left = 107
        Top = 141
        Width = 60
        Height = 15
        AutoSize = False
        Caption = 'good cut:'
        WordWrap = True
      end
      object Label67: TLabel
        Left = 107
        Top = 162
        Width = 60
        Height = 15
        AutoSize = False
        Caption = 'best cut:'
        WordWrap = True
      end
      object chbKeyframes: TCheckBox
        Left = 230
        Top = 299
        Width = 116
        Height = 17
        Alignment = taLeftJustify
        Caption = 'Keyframes On'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = chbKeyframesClick
      end
    end
    object tsCleanFile: TTabSheet
      Caption = 'Clean File Name'
      ImageIndex = 11
      object Label55: TLabel
        Left = 0
        Top = 316
        Width = 380
        Height = 15
        Align = alBottom
        Alignment = taCenter
        Caption = 
          'changes are saved immediately - hit [Escape] to close this confi' +
          'g window'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object lblDirtyChars: TLabel
        Left = 36
        Top = 10
        Width = 500
        Height = 120
        Alignment = taCenter
        Caption = 
          'Ctrl-Shift-[R] will clean the current file name by replacing cer' +
          'tain characters with a space'#13#10#13#10'Some of these characters must be' +
          ' removed in order to open the Audio && Video Timeline Editor'#13#10'as' +
          ' they will break the FFMPEG command line when you try to Export ' +
          'your edits to a new file*'#13#10#13#10'Other characters are simply unneces' +
          'sary and add no value to the name of the file'#13#10#13#10'Ctrl-Shift-[R] ' +
          'regards the following characters as "dirty":'
      end
      object Label56: TLabel
        Left = 180
        Top = 292
        Width = 216
        Height = 30
        Alignment = taCenter
        Caption = 
          '* in particular, an apostrophe ( '#39' )'#13#10'Note: any .mmp file will a' +
          'lso be renamed'
        Color = clBtnFace
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clAqua
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        StyleElements = [seClient, seBorder]
      end
      object edtDirtyChars: TLabeledEdit
        Left = 154
        Top = 152
        Width = 185
        Height = 23
        Hint = 'Defaults to subfolder "folder1"'
        EditLabel.Width = 61
        EditLabel.Height = 23
        EditLabel.Caption = 'Dirty Chars'
        EditLabel.Font.Charset = DEFAULT_CHARSET
        EditLabel.Font.Color = clWindowText
        EditLabel.Font.Height = -12
        EditLabel.Font.Name = 'Segoe UI'
        EditLabel.Font.Style = [fsBold]
        EditLabel.ParentFont = False
        LabelPosition = lpLeft
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        Text = ''
        OnChange = edtDirtyCharsChange
      end
      object btnDirtyCharsDefault: TButton
        Left = 344
        Top = 152
        Width = 75
        Height = 25
        Caption = 'Default'
        TabOrder = 1
        OnClick = btnDirtyCharsDefaultClick
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 140
    Height = 361
    Align = alLeft
    TabOrder = 1
    object Label68: TLabel
      Left = 1
      Top = 1
      Width = 138
      Height = 15
      Align = alTop
      Alignment = taCenter
      Caption = 'Options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 43
    end
    object lbTabCaptions: TListBox
      Left = 1
      Top = 16
      Width = 137
      Height = 344
      Align = alLeft
      ItemHeight = 15
      Items.Strings = (
        'General'
        'File Deletion'
        'Playlist'
        'Playist Filter'
        'User-Defined Folders'
        'External Apps'
        'Quick Rename'
        'Scale Factor'
        'Slideshows'
        'Repeat Delay'
        'Editing'
        'Keyframes'
        'Clean File Name')
      TabOrder = 0
      OnClick = lbTabCaptionsClick
    end
  end
  object fileOpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 28
    Top = 315
  end
end
