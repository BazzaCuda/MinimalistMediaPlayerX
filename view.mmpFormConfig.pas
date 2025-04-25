unit view.mmpFormConfig;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.comCtrls, vcl.controls, vcl.dialogs, vcl.forms, vcl.graphics,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber, Vcl.StdCtrls;

type
  TConfigForm = class(TForm)
    PageControl1: TPageControl;
    tsGeneral: TTabSheet;
    chbAutoUpdate: TCheckBox;
    Label2: TLabel;
    Label1: TLabel;
    tsDeletions: TTabSheet;
    chbStartInEditor: TCheckBox;
    Label3: TLabel;
    tsPlaylist: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    tsUserFolders: TTabSheet;
    tsExternalApps: TTabSheet;
    TabSheet1: TTabSheet;
    chbOpenImage: TCheckBox;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    chbExitBrowser: TCheckBox;
    Label11: TLabel;
    TabSheet2: TTabSheet;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chbAutoUpdateClick(Sender: TObject);
    procedure chbStartInEditorClick(Sender: TObject);
    procedure chbOpenImageClick(Sender: TObject);
    procedure chbExitBrowserClick(Sender: TObject);
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
    setForegroundWindow(HANDLE); // the order of these two is important
    setWindowPos(HANDLE, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
    showModal;
    mmp.cmd(evGSUserInput, FALSE);
    mmp.cmd(evGSShowingConfig, FALSE);
    free;
  end;
end;

{$R *.dfm}

procedure TConfigForm.chbAutoUpdateClick(Sender: TObject);
begin
  CF.asBoolean[CONF_AUTO_UPDATE] := chbAutoUpdate.checked;
end;

procedure TConfigForm.chbExitBrowserClick(Sender: TObject);
begin
  case chbExitBrowser.checked of   TRUE: CF[CONF_EXIT_BROWSER] := 'exitApp';
                                  FALSE: CF[CONF_EXIT_BROWSER] := 'main'; end;
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

procedure TConfigForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: modalResult := mrOK; end;
end;

function TConfigForm.loadConfig: boolean;
begin
  chbAutoUpdate.checked       := CF.asBoolean[CONF_AUTO_UPDATE];
  chbStartInEditor.checked    := CF.asBoolean[CONF_START_IN_EDITOR];
  chbOpenImage.checked        := lowerCase(CF[CONF_OPEN_IMAGE]) = 'browser';
  chbExitBrowser.checked      := lowerCase(CF[CONF_EXIT_BROWSER]) = 'exitapp';
end;

end.
