{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit view.mmpFormConfig;

interface

uses
  {$ifopt D+}
    {$define designTime} // comment out when not designing this form
  {$endif}
  {$define designTime} // temporary until we sort out the uses clause
  {$ifdef designTime}
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Buttons, Vcl.Mask, Vcl.Samples.Spin,
  {$endif}
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazAction;

type
  TConfigForm = class(TForm)
    pageControl: TPageControl;
    tsGeneral: TTabSheet;
    chbAutoUpdate: TCheckBox;
    Label2: TLabel;
    Label1: TLabel;
    tsDeletions: TTabSheet;
    chbStartInEditor: TCheckBox;
    Label3: TLabel;
    tsPlaylist: TTabSheet;
    Label5: TLabel;
    tsUserFolders: TTabSheet;
    tsExternalApps: TTabSheet;
    tsQuickRename: TTabSheet;
    chbOpenImage: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    chbExitBrowser: TCheckBox;
    Label11: TLabel;
    TabSheet2: TTabSheet;
    chbAudio: TCheckBox;
    Label13: TLabel;
    chbVideo: TCheckBox;
    chbImage: TCheckBox;
    Label14: TLabel;
    chbFolderDelete: TCheckBox;
    Label15: TLabel;
    chbKeepDelete: TCheckBox;
    Label16: TLabel;
    RadioGroup1: TRadioGroup;
    rbRecycle: TRadioButton;
    rbStandard: TRadioButton;
    rbShred: TRadioButton;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label12: TLabel;
    Label21: TLabel;
    chbNextFolderOnEnd: TCheckBox;
    chbNextFolderOnEmpty: TCheckBox;
    Label23: TLabel;
    chbAllowIntoWindows: TCheckBox;
    Label24: TLabel;
    Bevel1: TBevel;
    tsPlaylistFilter: TTabSheet;
    RadioGroup2: TRadioGroup;
    rbFilterAll: TRadioButton;
    rbFilterAudio: TRadioButton;
    rbFilterVideo: TRadioButton;
    rbFilterImage: TRadioButton;
    Label25: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    edtBaseFolder: TLabeledEdit;
    btnBaseFolder: TSpeedButton;
    edtSaved: TLabeledEdit;
    btnSaved: TSpeedButton;
    edtMoved: TLabeledEdit;
    btnMoved: TSpeedButton;
    edtCopied: TLabeledEdit;
    btnCopied: TSpeedButton;
    fileOpenDialog: TFileOpenDialog;
    Label32: TLabel;
    Label33: TLabel;
    edtF1: TLabeledEdit;
    btnF1: TSpeedButton;
    edtF2: TLabeledEdit;
    btnF2: TSpeedButton;
    edtF3: TLabeledEdit;
    btnF3: TSpeedButton;
    edtF4: TLabeledEdit;
    btnF4: TSpeedButton;
    edtF5: TLabeledEdit;
    btnF5: TSpeedButton;
    edtF7: TLabeledEdit;
    btnF7: TSpeedButton;
    edtF8: TLabeledEdit;
    btnF8: TSpeedButton;
    edtF9: TLabeledEdit;
    btnF9: TSpeedButton;
    edtF6: TLabeledEdit;
    btnF6: TSpeedButton;
    edtF10: TLabeledEdit;
    btnF10: TSpeedButton;
    edtF11: TLabeledEdit;
    btnF11: TSpeedButton;
    edtF12: TLabeledEdit;
    btnF12: TSpeedButton;
    edtAppF10: TLabeledEdit;
    btnAppF10: TSpeedButton;
    edtAppF11: TLabeledEdit;
    btnAppF11: TSpeedButton;
    edtAppF12: TLabeledEdit;
    btnAppF12: TSpeedButton;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    edtPrefixF1: TLabeledEdit;
    edtPrefixF2: TLabeledEdit;
    edtPrefixF3: TLabeledEdit;
    edtSuffixF4: TLabeledEdit;
    GroupBox2: TGroupBox;
    Label39: TLabel;
    spinImageDelayMs: TSpinEdit;
    Label40: TLabel;
    tsScaleFactor: TTabSheet;
    Label41: TLabel;
    spinScaleFactor: TSpinEdit;
    Label42: TLabel;
    btnScaleFactorDefault: TButton;
    btnRepeatDelayDefault: TButton;
    Label43: TLabel;
    tsSlideshowIntervalMs: TTabSheet;
    Label44: TLabel;
    spinSlideshowIntervalMs: TSpinEdit;
    btnSlideshowIntervalMsDefault: TButton;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label22: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    tsEditing: TTabSheet;
    Label50: TLabel;
    chbPlayEdited: TCheckBox;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    tsCleanFile: TTabSheet;
    Label55: TLabel;
    edtDirtyChars: TLabeledEdit;
    lblDirtyChars: TLabel;
    btnDirtyCharsDefault: TButton;
    Label56: TLabel;
    rbFilterAudioVideo: TRadioButton;
    tsKeyframes: TTabSheet;
    Label57: TLabel;
    Label58: TLabel;
    chbKeyframes: TCheckBox;
    Label59: TLabel;
    lblWhite: TLabel;
    lblYellow: TLabel;
    lblPurple: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Panel1: TPanel;
    Label68: TLabel;
    lbTabCaptions: TListBox;
    Label26: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    tsChapters: TTabSheet;
    Label72: TLabel;
    chbChaptersShow: TCheckBox;
    Label73: TLabel;
    chbChaptersAudioWrite: TCheckBox;
    chbChaptersVideoWrite: TCheckBox;
    Label74: TLabel;
    procedure chbAutoUpdateClick(Sender: TObject);
    procedure chbStartInEditorClick(Sender: TObject);
    procedure chbOpenImageClick(Sender: TObject);
    procedure chbExitBrowserClick(Sender: TObject);
    procedure chbAudioClick(Sender: TObject);
    procedure chbVideoClick(Sender: TObject);
    procedure chbImageClick(Sender: TObject);
    procedure chbFolderDeleteClick(Sender: TObject);
    procedure chbKeepDeleteClick(Sender: TObject);
    procedure rbRecycleClick(Sender: TObject);
    procedure rbStandardClick(Sender: TObject);
    procedure rbShredClick(Sender: TObject);
    procedure chbNextFolderOnEndClick(Sender: TObject);
    procedure chbNextFolderOnEmptyClick(Sender: TObject);
    procedure chbAllowIntoWindowsClick(Sender: TObject);
    procedure rbFilterAllClick(Sender: TObject);
    procedure rbFilterAudioClick(Sender: TObject);
    procedure rbFilterVideoClick(Sender: TObject);
    procedure rbFilterImageClick(Sender: TObject);
    procedure btnBaseFolderClick(Sender: TObject);
    procedure edtBaseFolderChange(Sender: TObject);
    procedure edtCopiedChange(Sender: TObject);
    procedure btnAppF10Click(Sender: TObject);
    procedure edtAppF10Change(Sender: TObject);
    procedure edtPrefixF1Change(Sender: TObject);
    procedure spinImageDelayMsChange(Sender: TObject);
    procedure spinScaleFactorChange(Sender: TObject);
    procedure btnRepeatDelayDefaultClick(Sender: TObject);
    procedure btnScaleFactorDefaultClick(Sender: TObject);
    procedure btnSlideshowIntervalMsDefaultClick(Sender: TObject);
    procedure spinSlideshowIntervalMsChange(Sender: TObject);
    procedure chbPlayEditedClick(Sender: TObject);
    procedure edtDirtyCharsChange(Sender: TObject);
    procedure btnDirtyCharsDefaultClick(Sender: TObject);
    procedure rbFilterAudioVideoClick(Sender: TObject);
    procedure chbKeyframesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbTabCaptionsClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chbChaptersShowClick(Sender: TObject);
    procedure chbChaptersAudioWriteClick(Sender: TObject);
    procedure chbChaptersVideoWriteClick(Sender: TObject);
    procedure pageControlChanging(Sender: TObject; var AllowChange: Boolean);
    procedure pageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  strict private
  private
    procedure SetActiveTabByIx(const aTabIx: Integer);
  protected
    function loadConfig: TVoid;
    function populateListBox: TVoid;
  public
  end;

function mmpConfig(const aTabCaption: string = ''): boolean;

implementation

uses
  bazCmd,
  mmpConsts, mmpFolderUtils, mmpGlobalState, mmpKeyboardUtils, mmpShellUtils, mmpUserFolders, mmpUtils,
  model.mmpConfigFile,
  _debugWindow;

function mmpConfig(const aTabCaption: string = ''): boolean;
begin
  with TConfigForm.create(NIL) do begin
    mmp.cmd(evGSShowingConfig, TRUE);
    mmp.cmd(evGSUserInput, TRUE);
    populateListBox;
    loadConfig;

    case aTabCaption = '' of   TRUE:  begin
                                        lbTabCaptions.itemIndex := 0;
                                        pageControl.activePageIndex := 0; end;
                              FALSE:  begin
                                        lbTabCaptions.itemIndex     := lbTabCaptions.items.indexOf(aTabCaption);
                                        pageControl.activePageIndex := lbTabCaptions.itemIndex; end;end;

    setForegroundWindow(HANDLE); // the order of these two is important
    setWindowPos(HANDLE, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
    showModal;
    mmp.cmd(evGSUserInput, FALSE);
    mmp.cmd(evGSShowingConfig, FALSE);
    free;
  end;
end;

function selectFolder(const aFileOpenDialog: TFileOpenDialog; const aPrevFolder: string): string;
begin
  with aFileOpenDialog do begin
    title         := 'Select Folder';
    options       := [fdoPickFolders, fdoPathMustExist, fdoForceFileSystem, fdoNoValidate];
    OKButtonLabel := 'Select';
    defaultFolder := aPrevFolder;
    fileName      := aPrevFolder;
    fileTypes.clear;
    case execute of  TRUE: result := mmpITBS(fileName);
                    FALSE: result := EMPTY; end;
  end;
end;


function selectApplication(const aFileOpenDialog: TFileOpenDialog; const aPrevFolder: string): string;
begin
  with aFileOpenDialog do begin
    title         := 'Select an Application';
    options       := [fdoPathMustExist, fdoFileMustExist, fdoForceFileSystem, fdoNoValidate];
    OKButtonLabel := 'Select';
    defaultFolder := aPrevFolder;
    fileName      := aPrevFolder;

    fileTypes.clear;
    with fileTypes.Add do begin
      displayName := 'Exe Files';
      fileMask := '*.exe';
    end;

    with fileTypes.Add do begin
      displayName := 'Bat Files';
      fileMask := '*.bat';
    end;

    with fileTypes.Add do begin
      displayName := 'Shortcuts';
      fileMask := '*.lnk';
    end;

    with fileTypes.Add do begin
      displayName := 'All Files';
      fileMask := '*.*';
    end;

    fileTypeIndex := 0;

    case execute of  TRUE: result := fileName;
                    FALSE: result := EMPTY; end;
  end;
end;

{$R *.dfm}

procedure TConfigForm.btnAppF10Click(Sender: TObject);
{$J+} const vPrevFolder: string = EMPTY; {$J-}
begin
  var vNewFolder := selectApplication(fileOpenDialog, vPrevFolder);
  case vNewFolder = EMPTY of TRUE: EXIT; end;
  vPrevFolder := vNewFolder;

  case sender = btnAppF10 of TRUE: edtAppF10.text := vPrevFolder; end;
  case sender = btnAppF11 of TRUE: edtAppF11.text := vPrevFolder; end;
  case sender = btnAppF12 of TRUE: edtAppF12.text := vPrevFolder; end;
end;

procedure TConfigForm.btnBaseFolderClick(Sender: TObject);
{$J+} const vPrevFolder: string = EMPTY; {$J-}
begin
  var vNewFolder := selectFolder(fileOpenDialog, vPrevFolder);
  case vNewFolder = EMPTY of TRUE: EXIT; end;
  vPrevFolder := vNewFolder;

  // each TLabeledEdit and each TSpeedButton have tags 1 - 16
  var vTag := TSpeedButton(sender).tag;
  for var i := 0 to tsUserFolders.controlCount - 1 do
    case (tsUserFolders.controls[i] is TLabeledEdit) and (TLabeledEdit(tsUserFolders.controls[i]).tag = vTag) of TRUE: TLabeledEdit(tsUserFolders.controls[i]).text := vPrevFolder; end;
end;

procedure TConfigForm.btnDirtyCharsDefaultClick(Sender: TObject);
begin
  edtDirtyChars.text := ' '; // don't specify an empty string otherwise CF will delete the entry in the .conf file
end;

procedure TConfigForm.btnRepeatDelayDefaultClick(Sender: TObject);
begin
  spinImageDelayMs.value := DEFAULT_REPEAT_DELAY_MS;
end;

procedure TConfigForm.btnScaleFactorDefaultClick(Sender: TObject);
begin
  spinScaleFactor.value := DEFAULT_SCALE_FACTOR;
end;

procedure TConfigForm.btnSlideshowIntervalMsDefaultClick(Sender: TObject);
begin
  spinSlideshowIntervalMs.value := IMAGE_DISPLAY_DURATION * MILLISECONDS;
end;

procedure TConfigForm.chbAllowIntoWindowsClick(Sender: TObject);
begin
  CF.asBoolean[CONF_ALLOW_INTO_WINDOWS] := chbAllowIntoWindows.checked;
end;

procedure TConfigForm.chbAudioClick(Sender: TObject);
begin
  CF.asBoolean[CONF_AUDIO_DELETE] := chbAudio.checked;
end;

procedure TConfigForm.chbAutoUpdateClick(Sender: TObject);
begin
  CF.asBoolean[CONF_AUTO_UPDATE] := chbAutoUpdate.checked;
end;

procedure TConfigForm.chbChaptersAudioWriteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE] := chbChaptersAudioWrite.checked;
end;

procedure TConfigForm.chbChaptersShowClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_SHOW] := chbChaptersShow.checked;
end;

procedure TConfigForm.chbChaptersVideoWriteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE] := chbChaptersVideoWrite.checked;
end;

procedure TConfigForm.chbExitBrowserClick(Sender: TObject);
begin
  case chbExitBrowser.checked of   TRUE: CF[CONF_EXIT_BROWSER] := CONF_EXIT_APP;
                                  FALSE: CF[CONF_EXIT_BROWSER] := CONF_MAIN end;
end;

procedure TConfigForm.chbFolderDeleteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_FOLDER_DELETE] := chbFolderDelete.checked;
end;

procedure TConfigForm.chbImageClick(Sender: TObject);
begin
  CF.asBoolean[CONF_IMAGE_DELETE] := chbImage.checked;
end;

procedure TConfigForm.chbKeepDeleteClick(Sender: TObject);
begin
  CF.asBoolean[CONF_KEEP_DELETE] := chbKeepDelete.checked;
end;

procedure TConfigForm.chbKeyframesClick(Sender: TObject);
begin
  CF.asBoolean[CONF_KEYFRAMES] := chbKeyframes.checked;
end;

procedure TConfigForm.chbNextFolderOnEmptyClick(Sender: TObject);
begin
  CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY] := chbNextFolderOnEmpty.checked;
end;

procedure TConfigForm.chbNextFolderOnEndClick(Sender: TObject);
begin
  CF.asBoolean[CONF_NEXT_FOLDER_ON_END] := chbNextFolderOnEnd.checked;
end;

procedure TConfigForm.chbOpenImageClick(Sender: TObject);
begin
  case chbOpenImage.checked of   TRUE: CF[CONF_OPEN_IMAGE] := CONF_BROWSER;
                                FALSE: CF[CONF_OPEN_IMAGE] := CONF_MAIN; end;
end;

procedure TConfigForm.chbPlayEditedClick(Sender: TObject);
begin
  CF.asBoolean[CONF_PLAY_EDITED] := chbPlayEdited.checked;
end;

procedure TConfigForm.chbStartInEditorClick(Sender: TObject);
begin
  CF.asBoolean[CONF_START_IN_EDITOR] := chbStartInEditor.checked;
end;

procedure TConfigForm.chbVideoClick(Sender: TObject);
begin
  CF.asBoolean[CONF_VIDEO_DELETE] := chbVideo.checked;
end;

procedure TConfigForm.edtAppF10Change(Sender: TObject);
begin
  var vEdit := sender as TLabeledEdit;
  case fileExists(vEdit.text) of   TRUE: vEdit.font.color := DARK_MODE_SILVER;
                                  FALSE: vEdit.font.color := clRed; end; // warn the user but let them setup MMP how they want

  case trim(vEdit.text) = EMPTY of TRUE: vEdit.text := ' '; end; // allow the user to blank the folder name without CF deleting the entry

  // if the .conf file entry hasn't changed, or the edit box contains the default app for the Fnn key, don't write to the .conf file
  case (vEdit = edtAppF10) and NOT (edtAppF10.text = mmpGetExternalApp(F10_APP)) of TRUE: CF[mmpFnnKeyAppToString(F10_APP)] := vEdit.text; end;
  case (vEdit = edtAppF11) and NOT (edtAppF11.text = mmpGetExternalApp(F11_APP)) of TRUE: CF[mmpFnnKeyAppToString(F11_APP)] := vEdit.text; end;
  case (vEdit = edtAppF12) and NOT (edtAppF12.text = mmpGetExternalApp(F12_APP)) of TRUE: CF[mmpFnnKeyAppToString(F12_APP)] := vEdit.text; end;
end;

procedure TConfigForm.edtBaseFolderChange(Sender: TObject);
begin
  var vText := edtBaseFolder.text;
  case directoryExists(vText) of   TRUE: edtBaseFolder.font.color := DARK_MODE_SILVER;
                                  FALSE: edtBaseFolder.font.color := clRed; end; // warn the user but them let setup MMP how they want
  case trim(vText) = EMPTY of TRUE: vText := ' '; end; // allow the user to blank the folder name without CF deleting the entry
  CF[CONF_BASE_FOLDER] := vText;
end;

procedure TConfigForm.edtCopiedChange(Sender: TObject);
begin
  var vEdit := sender as TLabeledEdit;
  var vText := vEdit.text;

  case (length(vText) > 1) and (vText[2] = ':') and NOT directoryExists(vText) of  TRUE: vEdit.font.color := clRed;
                                                                                  FALSE: vEdit.font.color := DARK_MODE_SILVER; end; // warn the user but let them setup MMP how they want

  case trim(vText) = EMPTY of TRUE: vText := SINGLE_SPACE; end; // allow the user to blank the folder name without CF deleting the entry

  CF[CONF_FOLDERS[vEdit.tag]] := vText;
end;

procedure TConfigForm.edtDirtyCharsChange(Sender: TObject);
begin
  CF[CONF_DIRTY_CHARS] := trim(edtDirtyChars.text) + SINGLE_SPACE;
end;

procedure TConfigForm.edtPrefixF1Change(Sender: TObject);
begin
  var vText := (sender as TLabeledEdit).text;
  case vText = EMPTY of TRUE: vText := SINGLE_SPACE; end; // don't trim the user's leading or trailing spaces but also don't let CF delete the entry if the user blanks the text
  case sender = edtPrefixF1 of TRUE: CF[CONF_CAT_F1] := vText; end;
  case sender = edtPrefixF2 of TRUE: CF[CONF_CAT_F2] := vText; end;
  case sender = edtPrefixF3 of TRUE: CF[CONF_CAT_F3] := vText; end;
  case sender = edtSuffixF4 of TRUE: CF[CONF_CAT_F4] := vText; end;
end;

procedure TConfigForm.FormCreate(Sender: TObject);
begin
  lblWhite.styleElements  := [];
  lblYellow.styleElements := [];
  lblPurple.styleElements := [];
  lblWhite.font.color  := clWhite;
  lblYellow.font.color := clYellow;
  lblPurple.font.color := clFuchsia;

  panel1.bevelInner  := bvLowered;
  panel1.bevelOuter  := bvLowered;

  lbTabCaptions.bevelInner  := bvNone;
  lbTabCaptions.bevelOuter  := bvNone;
  lbTabCaptions.borderStyle := bsNone;
end;

function TConfigForm.loadConfig: TVoid;
begin
  chbAutoUpdate.checked         := CF.asBoolean[CONF_AUTO_UPDATE];
  chbStartInEditor.checked      := CF.asBoolean[CONF_START_IN_EDITOR];
  chbOpenImage.checked          := lowerCase(CF[CONF_OPEN_IMAGE])   = CONF_BROWSER;
  chbExitBrowser.checked        := lowerCase(CF[CONF_EXIT_BROWSER]) = lowerCase(CONF_EXIT_APP);

  chbAudio.checked              := CF.asBoolean[CONF_AUDIO_DELETE];
  chbVideo.checked              := CF.asBoolean[CONF_VIDEO_DELETE];
  chbImage.checked              := CF.asBoolean[CONF_IMAGE_DELETE];
  chbFolderDelete.checked       := CF.asBoolean[CONF_FOLDER_DELETE];
  chbKeepDelete.checked         := CF.asBoolean[CONF_KEEP_DELETE];
  rbRecycle.checked             := CF.asDeleteMethod[CONF_DELETE_METHOD] = dmRecycle;
  rbStandard.checked            := CF.asDeleteMethod[CONF_DELETE_METHOD] = dmStandard;
  rbShred.checked               := CF.asDeleteMethod[CONF_DELETE_METHOD] = dmShred;

  chbNextFolderOnEnd.checked    := CF.asBoolean[CONF_NEXT_FOLDER_ON_END];
  chbNextFolderOnEmpty.checked  := CF.asBoolean[CONF_NEXT_FOLDER_ON_EMPTY];
  chbAllowIntoWindows.checked   := CF.asBoolean[CONF_ALLOW_INTO_WINDOWS];

  rbFilterAll.checked           := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtUnk;
  rbFilterAudio.checked         := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtAudio;
  rbFilterVideo.checked         := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtVideo;
  rbFilterAudioVideo.checked    := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtAudioVideo;
  rbFilterImage.checked         := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtImage;

  // each TLabeledEdit and each TSpeedButton have tags 1 - 16
  for var i := 0 to tsUserFolders.controlCount - 1 do
    case tsUserFolders.controls[i] is TLabeledEdit of TRUE: TLabeledEdit(tsUserFolders.controls[i]).text := trim(CF[CONF_FOLDERS[TLabeledEdit(tsUserFolders.controls[i]).tag]]); end;

  edtAppF10.text                := trim(mmpGetExternalApp(F10_APP));
  edtAppF11.text                := trim(mmpGetExternalApp(F11_APP));
  edtAppF12.text                := trim(mmpGetExternalApp(F12_APP));

  edtPrefixF1.text              := CF[CONF_CAT_F1];
  edtPrefixF2.text              := CF[CONF_CAT_F2];
  edtPrefixF3.text              := CF[CONF_CAT_F3];
  edtSuffixF4.text              := CF[CONF_CAT_F4];

  var vMs                       := CF[CONF_REPEAT_DELAY_MS]; // get it as a string, so that we can control the default value not the spinEdit
  try spinImageDelayMs.value    := strToIntDef(vMs, DEFAULT_REPEAT_DELAY_MS); except spinImageDelayMs.value := DEFAULT_REPEAT_DELAY_MS; end; // if the entry is blank, default to

  spinScaleFactor.minValue      := MIN_SCALE_FACTOR;
  spinScaleFactor.maxValue      := MAX_SCALE_FACTOR;
  spinScaleFactor.value         := CF.asInteger[CONF_SCALE_FACTOR];

  spinSlideshowIntervalMs.minValue := SLIDESHOW_DELTA_MS;
  case CF.asInteger[CONF_SLIDESHOW_INTERVAL_MS] = 0 of  TRUE: spinSlideshowIntervalMs.value := IMAGE_DISPLAY_DURATION;
                                                       FALSE: spinSlideshowIntervalMs.value := CF.asInteger[CONF_SLIDESHOW_INTERVAL_MS]; end;

  chbPlayEdited.checked         := CF.asBoolean[CONF_PLAY_EDITED];
  chbKeyframes.checked          := CF.asBoolean[CONF_KEYFRAMES];

  edtDirtyChars.text            := trim(CF[CONF_DIRTY_CHARS]);

  chbChaptersShow.checked       := CF.asBoolean[CONF_CHAPTERS_SHOW];
  chbChaptersAudioWrite.checked := CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE];
  chbChaptersVideoWrite.checked := CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE];
end;

function TConfigForm.populateListBox: TVoid;
begin
  lbTabCaptions.items.beginUpdate;
  try
    lbTabCaptions.items.clear;

    for var i := 0 to pageControl.pageCount - 1 do  begin
                                                      var tab := pageControl.pages[i];
                                                      lbTabCaptions.items.addObject(tab.caption, tab); end;
    lbTabCaptions.sorted := TRUE;
  finally
    lbTabCaptions.items.endUpdate;
  end;
end;

//==================== TAB CONTROL ====================
procedure TConfigForm.pageControlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  allowChange   := FALSE; // only allow programmatic changes
end;

procedure TConfigForm.pageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // find which tab was clicked
  for var i := 0 to pageControl.pageCount - 1 do
    case pageControl.tabRect(i).contains(point(x, y)) of TRUE: setActiveTabByIx(i); end;
end;

procedure TConfigForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // we action Ctrl-Tab, not pageControl
  case (Key = VK_TAB) and (ssCtrl in Shift) of TRUE:  begin
                                                        var vIx := pageControl.activePageIndex;

                                                        case ssShift in Shift of   TRUE: case vIx > 0                         of TRUE: vIx := vIx - 1; end;
                                                                                  FALSE: case vIx < pageControl.pageCount - 1 of TRUE: vIx := vIx + 1; end;end;

                                                        SetActiveTabByIx(vIx); end;end;

  // absolutely vital!
  case key = VK_ESCAPE of TRUE: begin
                                  //mmp.cmd(evGSIgnoreEscape, TRUE); // test without this at some point soon
                                  modalResult := mrOK; end;end;
end;

procedure TConfigForm.setActiveTabByIx(const aTabIx: integer);
begin
  case (aTabIx >= 0) and (aTabIx < pageControl.pageCount) of TRUE:  begin
                                                                      pageControl.activePageIndex := aTabIx;
                                                                      lbTabCaptions.itemIndex     := lbTabCaptions.items.indexOf(pageControl.activePage.caption);
                                                                      lbTabCaptionsClick(NIL); end;end;
end;

procedure TConfigForm.lbTabCaptionsClick(Sender: TObject);
begin
  case lbTabCaptions.itemIndex >= 0 of TRUE: pageControl.activePage := TTabSheet(lbTabCaptions.items.objects[lbTabCaptions.itemIndex]); end;
end;
//==================== TAB CONTROL ====================


procedure TConfigForm.rbShredClick(Sender: TObject);
begin
  CF.asDeleteMethod[CONF_DELETE_METHOD] := dmShred;
end;

procedure TConfigForm.rbStandardClick(Sender: TObject);
begin
  CF.asDeleteMethod[CONF_DELETE_METHOD] := dmStandard;
end;

procedure TConfigForm.spinImageDelayMsChange(Sender: TObject);
begin
  case spinImageDelayMs.value = 0 of   TRUE: CF[CONF_REPEAT_DELAY_MS] := ' '; // reset to default blank entry
                                      FALSE: CF.asInteger[CONF_REPEAT_DELAY_MS] := spinImageDelayMs.value; end;
end;

procedure TConfigForm.spinScaleFactorChange(Sender: TObject);
begin
  CF.asInteger[CONF_SCALE_FACTOR] := spinScaleFactor.value;
end;

procedure TConfigForm.spinSlideshowIntervalMsChange(Sender: TObject);
begin
  CF.asInteger[CONF_SLIDESHOW_INTERVAL_MS] := spinSlideshowIntervalMs.value;
end;

procedure TConfigForm.rbFilterAllClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtUnk;
end;

procedure TConfigForm.rbFilterAudioClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtAudio;
end;

procedure TConfigForm.rbFilterAudioVideoClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtAudioVideo;
end;

procedure TConfigForm.rbFilterImageClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtImage;
end;

procedure TConfigForm.rbFilterVideoClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtVideo;
end;

procedure TConfigForm.rbRecycleClick(Sender: TObject);
begin
  CF.asDeleteMethod[CONF_DELETE_METHOD] := dmRecycle;
end;

end.
