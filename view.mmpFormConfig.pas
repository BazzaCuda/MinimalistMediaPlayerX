unit view.mmpFormConfig;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber, Vcl.StdCtrls, Vcl.ExtCtrls;

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
    TabSheet1: TTabSheet;
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
    Label22: TLabel;
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
    Label26: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  strict private
  protected
    function loadConfig: boolean;
  public
  end;

function mmpConfig: boolean;

implementation

uses
  mmpConsts, mmpFuncProg, mmpGlobalState, mmpUtils,
  model.mmpConfigFile;

function mmpConfig: boolean;
begin
  with TConfigForm.create(NIL) do begin
    mmp.cmd(evGSShowingConfig, TRUE);
    mmp.cmd(evGSUserInput, TRUE);
    loadConfig;
    pageControl.activePageIndex := 0;
    setForegroundWindow(HANDLE); // the order of these two is important
    setWindowPos(HANDLE, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
    showModal;
    mmp.cmd(evGSUserInput, FALSE);
    mmp.cmd(evGSShowingConfig, FALSE);
    free;
  end;
end;

{$R *.dfm}

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

procedure TConfigForm.chbExitBrowserClick(Sender: TObject);
begin
  case chbExitBrowser.checked of   TRUE: CF[CONF_EXIT_BROWSER] := 'exitApp';
                                  FALSE: CF[CONF_EXIT_BROWSER] := 'main'; end;
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
  case chbOpenImage.checked of   TRUE: CF[CONF_OPEN_IMAGE] := 'browser';
                                FALSE: CF[CONF_OPEN_IMAGE] := 'main'; end;
end;

procedure TConfigForm.chbStartInEditorClick(Sender: TObject);
begin
  CF.asBoolean[CONF_START_IN_EDITOR] := chbStartInEditor.checked;
end;

procedure TConfigForm.chbVideoClick(Sender: TObject);
begin
  CF.asBoolean[CONF_VIDEO_DELETE] := chbVideo.checked;
end;

procedure TConfigForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: modalResult := mrOK; end;
end;

function TConfigForm.loadConfig: boolean;
begin
  chbAutoUpdate.checked         := CF.asBoolean[CONF_AUTO_UPDATE];
  chbStartInEditor.checked      := CF.asBoolean[CONF_START_IN_EDITOR];
  chbOpenImage.checked          := lowerCase(CF[CONF_OPEN_IMAGE]) = 'browser';
  chbExitBrowser.checked        := lowerCase(CF[CONF_EXIT_BROWSER]) = 'exitapp';

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
  rbFilterImage.checked         := CF.asMediaType[CONF_PLAYLIST_FORMAT] = mtImage;
end;

procedure TConfigForm.rbShredClick(Sender: TObject);
begin
  CF.asDeleteMethod[CONF_DELETE_METHOD] := dmShred;
end;

procedure TConfigForm.rbStandardClick(Sender: TObject);
begin
  CF.asDeleteMethod[CONF_DELETE_METHOD] := dmStandard;
end;

procedure TConfigForm.rbFilterAllClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtUnk;
end;

procedure TConfigForm.rbFilterAudioClick(Sender: TObject);
begin
  CF.asMediaType[CONF_PLAYLIST_FORMAT] := mtAudio;
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
