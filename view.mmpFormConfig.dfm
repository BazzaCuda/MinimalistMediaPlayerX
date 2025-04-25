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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 584
    Height = 361
    ActivePage = tsGeneral
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
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
          'erate MMP as [in effect] two applications in one: playing audio ' +
          'and video files in the main media window and managing your image' +
          ' library in the Image && Thumbnail Browser'
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        ExplicitWidth = 380
      end
    end
    object tsPlaylist: TTabSheet
      Caption = 'Playlist'
      ImageIndex = 2
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        ExplicitWidth = 380
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
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
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
        ExplicitWidth = 380
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Repeat Delay'
      ImageIndex = 6
    end
  end
end
