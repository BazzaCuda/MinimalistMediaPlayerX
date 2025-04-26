object ConfigForm: TConfigForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'MMP Config'
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
    Left = 0
    Top = 0
    Width = 584
    Height = 361
    ActivePage = tsPlaylistFilter
    Align = alClient
    TabOrder = 0
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
        WordWrap = True
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
          'confirmation dialog'
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
          'playlist folder.'#13#10'MMP never touches the contents of any subfolde' +
          'rs'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
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
          'Note: Ctrl-Shift-[C]leanup goes straight to the delete confirmat' +
          'ion dialog'#13#10'because deleting your Timeline Editing files won'#39't a' +
          'ffect your original audio or video files'
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
          'Ctrl-[K]eep/delete can delete all the files in a folder*'#13#10'except' +
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
      object Label21: TLabel
        Left = 19
        Top = 2
        Width = 478
        Height = 30
        Alignment = taCenter
        Caption = 
          'When you launch MMP by [double-]clicking on a file in your file ' +
          'explorer software,'#13#10'MMP builds a playlist from all the supported' +
          ' file extensions in that folder and starts playing'
      end
      object Label22: TLabel
        Left = 19
        Top = 44
        Width = 362
        Height = 15
        Caption = 
          'MMP should move to the next folder and start playing a new playl' +
          'ist:'
      end
      object Label23: TLabel
        Left = 28
        Top = 171
        Width = 521
        Height = 30
        Alignment = taCenter
        AutoSize = False
        Caption = 
          '* deletion operations only ever affect the files in the current ' +
          'playlist folder.'#13#10'MMP never touches the contents of any subfolde' +
          'rs'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
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
        Font.Color = clTeal
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
      object chbNextFolderOnEnd: TCheckBox
        Left = 73
        Top = 63
        Width = 430
        Height = 33
        Alignment = taLeftJustify
        Caption = 'At the end of playing the current playlist'
        TabOrder = 0
        OnClick = chbNextFolderOnEndClick
      end
      object chbNextFolderOnEmpty: TCheckBox
        Left = 70
        Top = 102
        Width = 430
        Height = 63
        Alignment = taLeftJustify
        Caption = 
          'When you empty the playlist folder by:'#13#10'   a) Ctrl-[Del]ete all ' +
          'the remaining files in a folder* '#13#10'   b) Ctrl-[K]eep/delete all ' +
          'except those marked as [K]eep files (start with a "!")'#13#10'   c) [D' +
          ']elete the last remaining file in a folder'
        TabOrder = 1
        OnClick = chbNextFolderOnEmptyClick
      end
      object chbAllowIntoWindows: TCheckBox
        Left = 103
        Top = 235
        Width = 369
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
        Left = 83
        Top = 2
        Width = 427
        Height = 30
        Alignment = taCenter
        Caption = 
          'You can set MMP'#39's main media window to only play media files of ' +
          'a certain type.'#13#10'Ctrl-[P] turns Playlist Filtering on and off'
      end
      object Label27: TLabel
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
        Height = 121
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
        OnClick = rbFilterVideoClick
      end
      object rbFilterImage: TRadioButton
        Left = 31
        Top = 198
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
        OnClick = rbFilterImageClick
      end
    end
    object tsUserFolders: TTabSheet
      Caption = 'User Folders'
      ImageIndex = 3
      object Label8: TLabel
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
    end
    object tsExternalApps: TTabSheet
      Caption = 'External Apps'
      ImageIndex = 4
      object Label9: TLabel
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
    end
    object TabSheet1: TTabSheet
      Caption = 'Quick Rename'
      ImageIndex = 5
      object Label10: TLabel
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
    end
    object TabSheet2: TTabSheet
      Caption = 'Repeat Delay'
      ImageIndex = 6
      object Label12: TLabel
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
    end
  end
end
