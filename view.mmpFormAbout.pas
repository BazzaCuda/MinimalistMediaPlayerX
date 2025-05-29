{   MMP: Minimalist Media Player
    Copyright (C) 2021-2099 Baz Cuda
    https://github.com/BazzaCuda/MinimalistMediaPlayerX

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307, USA
}
unit view.mmpFormAbout;

interface

uses
  winApi.messages, winApi.windows,
  system.sysUtils, system.variants, system.classes,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.graphics, vcl.imaging.pngImage, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber, Vcl.Menus;


type
  IAboutForm = interface
    ['{958B8D00-5F85-401C-80F8-390721F9AE39}']
    function notify(const aNotice: INotice): INotice;
  end;

  {$REGION}
  // this should be in the implementation section but that would cause problems with the IDE
  TAboutForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblReleaseVersion: TLabel;
    lblCopyright: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblWebsiteURL: TLabel;
    Bevel1: TBevel;
    btnClose: TButton;
    Bevel2: TBevel;
    Label6: TLabel;
    lblBuildVersion: TLabel;
    lblLatestReleaseVersion: TLabel;
    Label7: TLabel;
    btnWhatsNew: TButton;
    Label3: TLabel;
    lblWikiURL: TLabel;
    btnLicence: TButton;
    mnuPopup: TPopupMenu;
    procedure lblWebsiteURLClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure lblWebsiteURLMouseEnter(Sender: TObject);
    procedure lblWebsiteURLMouseLeave(Sender: TObject);
    procedure btnWhatsNewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lblWikiURLClick(Sender: TObject);
    procedure lblWikiURLMouseEnter(Sender: TObject);
    procedure lblWikiURLMouseLeave(Sender: TObject);
    procedure btnLicenceClick(Sender: TObject);
  private
    procedure menuClick(sender: TObject);
  protected
    function setBuildVersion(const aBuild: string): boolean;
    function setCopyrightYear(const aYear: WORD): boolean;
    function setLatestReleaseVersion(const aRelease: string): boolean;
    function setReleaseVersion(const aRelease: string): boolean;
    function setNoStyle: boolean;
    function setWhatsNew(const aHasReleaseNotes: boolean): boolean;
    function checkPreviousReleaseNotes: boolean;
    function compareVersions(const thisVersion: string; const latestVersion: string): boolean;
  public
  end;
  {$ENDREGION}

implementation

uses
  winApi.shellAPI,
  system.types,
  mmpConsts, mmpFileUtils, mmpFuncProg, mmpProgramUpdates, mmpShellUtils, mmpUtils,
  view.mmpFormProgress, view.mmpFormReleaseNotes,
  _debugWindow;

type
  TAboutFormProxy = class(TInterfacedObject, IAboutForm)
  strict private
    FProgramUpdates:  IProgramUpdates;
    FProgressForm:    TProgressForm;
    FSubscriber:      ISubscriber;
  private
    procedure   timerTimer(Sender: TObject);
    function    onNotify(const aNotice: INotice): INotice;
    function    showForm(const thisVersion: string; const buildVersion: string): boolean; overload;
    function    showForm:         boolean; overload;
    function    showProgressForm: boolean;
  protected
  public
    constructor create;
    destructor  Destroy; override;
    function    notify(const aNotice: INotice): INotice;
  end;

function AB: IAboutForm;
{$J+} const gAboutFormProxy: IAboutForm = NIL; {$J-}
begin
  case gAboutFormProxy = NIL of TRUE: gAboutFormProxy := TAboutFormProxy.create; end;
  result := gAboutFormProxy;
end;

{$R *.dfm}

procedure TAboutForm.btnCloseClick(Sender: TObject);
begin
  modalResult := mrOK;
end;

procedure TAboutForm.btnLicenceClick(Sender: TObject);
begin
  AB.notify(newNotice(evAboutGNULicenceShow));
end;

procedure TAboutForm.btnWhatsNewClick(Sender: TObject);
begin
  case TComponent(sender).tag of
      0: AB.notify(newNotice(evAboutReleaseNotesFormShow));
    999: AB.notify(newNotice(evAboutPreviousReleaseNotes, TButton(sender).caption)); end;
end;

procedure TAboutForm.menuClick(sender: TObject);
begin
  btnWhatsNew.caption := TMenuItem(sender).caption;
  btnWhatsNewClick(btnWhatsNew);
end;

function TAboutForm.checkPreviousReleaseNotes: boolean;
  function releaseTags(const aReleaseNotesPath: string): TArray<string>;
  var
    RC: integer;
    SR: TSearchRec;
    FN: string;
  begin
    setLength(result, 0);
    RC := findFirst(aReleaseNotesPath + '*.md', faAnyFile, SR);
    while RC = 0 do begin
      setLength(result, length(result) + 1);
      FN := mmpFileNameWithoutExtension(SR.name);
      delete(FN, 1, pos('v', FN) - 1);
      result[high(result)] := FN;
      RC := findNext(SR);
    end;
  end;
begin
  result := FALSE;
//  case btnWhatsNew.tag = 0 of TRUE: EXIT; end;

  mnuPopup.autoHotKeys      := maManual; // prevent spurious & accelerator-key characters from being added to the menuItem captions

  btnWhatsNew.style         := bsSplitButton;
  btnWhatsNew.dropDownMenu  := mnuPopup;
  btnWhatsNew.visible       := TRUE;

  var vTags := releaseTags(mmpReleaseNotesFolder);

  for var i := 0 to high(vTags) do  begin
                                      var vMenuItem             := TMenuItem.create(mnuPopup);
                                      vMenuItem.caption         := vTags[i];
                                      vMenuItem.onClick         := menuClick;
                                      mnuPopup.items.add(vMenuItem); end;

  setLength(vTags, 0);
  result := TRUE;
end;

function versionLessThan(const aVersion1: string; const aVersion2: string): boolean;
var
  vParts1: TStringDynArray;
  vParts2: TStringDynArray;
  vMajor1: integer;
  vMinor1: integer;
  vPatch1: integer;
  vMajor2: integer;
  vMinor2: integer;
  vPatch2: integer;
begin
  vParts1 := (stringReplace(aVersion1, 'v', '', [rfReplaceAll])).split(['.']);
  vParts2 := (stringReplace(aVersion2, 'v', '', [rfReplaceAll])).split(['.']);

  vMajor1 := strToInt(vParts1[0]);
  vMinor1 := strToInt(vParts1[1]);
  vPatch1 := strToInt(vParts1[2]);

  vMajor2 := strToInt(vParts2[0]);
  vMinor2 := strToInt(vParts2[1]);
  vPatch2 := strToInt(vParts2[2]);

  if vMajor1 < vMajor2 then
  begin
    result := TRUE;
    EXIT;
  end;
  if vMajor1 > vMajor2 then
  begin
    result := FALSE;
    EXIT;
  end;

  if vMinor1 < vMinor2 then
  begin
    result := TRUE;
    EXIT;
  end;
  if vMinor1 > vMinor2 then
  begin
    result := FALSE;
    EXIT;
  end;

  if vPatch1 < vPatch2 then
  begin
    result := TRUE;
    EXIT;
  end;
  if vPatch1 > vPatch2 then
  begin
    result := FALSE;
    EXIT;
  end;

  result := FALSE;
end;

function TAboutForm.compareVersions(const thisVersion: string; const latestVersion: string): boolean;
begin
  case latestVersion[1] = 'v' of FALSE: EXIT; end;
  case versionLessThan(thisVersion, latestVersion) of TRUE: lblLatestReleaseVersion.font.style := [fsBold, fsUnderline]; end;
//  case thisVersion = latestVersion of FALSE: lblLatestReleaseVersion.font.style := [fsBold, fsUnderline]; end;
end;

procedure TAboutForm.FormShow(Sender: TObject);
begin
  case btnWhatsNew.visible of TRUE: btnWhatsNew.setFocus; end;
  btnClose.cancel := TRUE;
end;

procedure TAboutForm.lblWebsiteURLClick(Sender: TObject);
begin
  mmpShellExec('https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases/latest');
end;

procedure TAboutForm.lblWebsiteURLMouseEnter(Sender: TObject);
begin
  lblWebsiteURL.font.style := [fsUnderline];
end;

procedure TAboutForm.lblWebsiteURLMouseLeave(Sender: TObject);
begin
  lblWebsiteURL.font.style := [];
end;

procedure TAboutForm.lblWikiURLClick(Sender: TObject);
begin
  mmpShellExec('https://minimalistmediaplayer.com');
end;

procedure TAboutForm.lblWikiURLMouseEnter(Sender: TObject);
begin
  lblWikiURL.font.style := [fsUnderline];
end;

procedure TAboutForm.lblWikiURLMouseLeave(Sender: TObject);
begin
  lblWikiURL.font.style := [];
end;

function TAboutForm.setBuildVersion(const aBuild: string): boolean;
begin
  lblBuildVersion.Caption := aBuild;
end;

function TAboutForm.setCopyrightYear(const aYear: WORD): boolean;
begin
  lblCopyright.caption := lblCopyright.caption + intToStr(aYear);
end;

function TAboutForm.setLatestReleaseVersion(const aRelease: string): boolean;
begin
  lblLatestReleaseVersion.caption := aRelease;
end;

function TAboutForm.setNoStyle: boolean;
begin
  lblWebsiteURL.styleElements := [seClient, seBorder];
  lblWikiURL.styleElements    := [seClient, seBorder];
end;

function TAboutForm.setReleaseVersion(const aRelease: string): boolean;
begin
  lblReleaseVersion.caption := aRelease;
end;

function TAboutForm.setWhatsNew(const aHasReleaseNotes: boolean): boolean;
begin
  btnWhatsNew.visible := TRUE;
  case aHasReleaseNotes of  TRUE: btnWhatsNew.caption := 'What''s New?';
                           FALSE: begin
                                    btnWhatsNew.tag := 999;
                                    btnWhatsNew.caption := 'Release Notes'; end;end;
  btnWhatsNew.default := aHasReleaseNotes;
end;

{ TAboutBoxFormProxy }

constructor TAboutFormProxy.create;
begin
  inherited;
  FSubscriber     := appEvents.subscribe(newSubscriber(onNotify));
  FProgramUpdates := newProgramUpdates;
end;

destructor TAboutFormProxy.Destroy;
begin
  appEvents.unsubscribe(FSubscriber);
//  FSubscriber := NIL;
  inherited;
end;

function TAboutFormProxy.notify(const aNotice: INotice): INotice;
begin
  result := onNotify(aNotice);
end;

function TAboutFormProxy.onNotify(const aNotice: INotice): INotice;
begin
  case aNotice = NIL of TRUE: EXIT; end;
  case aNotice.event of
    evAboutFormShow:                showForm;
    evAboutReleaseNotesFormShow:    showReleaseNotes('Release Notes ' + FProgramUpdates.releaseTag, mmpReleaseNotesFilePath(FProgramUpdates.releaseTag));
    evAboutPreviousReleaseNotes:    showReleaseNotes('Release Notes ' + aNotice.text, mmpReleaseNotesFilePath(aNotice.text));
    evAboutGNULicenceShow:          showReleaseNotes('GNU General Public License', mmpExePath + 'license');
  end;
end;

function TAboutFormProxy.showForm(const thisVersion: string; const buildVersion: string): boolean;
begin
  mmp.cmd(evGSShowingAbout, TRUE);
  showProgressForm;
  with TAboutForm.create(NIL) do
  try
    setNoStyle;
    setCopyrightYear(currentYear);
    setReleaseVersion(thisVersion);
    setBuildVersion(buildVersion);
    setLatestReleaseVersion(FProgramUpdates.releaseTag);        // if the releaseTag is got, PU also downloads the release zip file and the release notes
    compareVersions(thisVersion, FProgramUpdates.releaseTag);
    setWhatsNew(FProgramUpdates.hasReleaseNotes(FProgramUpdates.releaseTag));
    checkPreviousReleaseNotes;
    showModal;
  finally
    free;
    mmp.cmd(evGSShowingAbout, FALSE);
  end;
end;

function TAboutFormProxy.showForm: boolean;
begin
  showForm(mmpFileVersionFmt('', 'v%d.%d.%d'), mmpFileVersionFmt);
end;

function TAboutFormProxy.showProgressForm: boolean;
begin
  FProgressForm := TProgressForm.create(NIL);
  try
    FProgressForm.modal               := FALSE;
    FProgressForm.buttons             := FALSE;
    FProgressForm.heading.caption     := 'MMP: Minimalist Media Player';
    FProgressForm.subHeading.caption  := 'Loading About Box...';
    FProgressForm.show;
    mmpProcessMessages;
    setWindowPos(FProgressForm.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE + SWP_NOMOVE);
    FProgressForm.timer.interval  := 1 * MILLISECONDS;
    FProgressForm.timer.OnTimer   := timerTimer;
    FProgressForm.timer.enabled   := TRUE;
  finally
  end;
end;

procedure TAboutFormProxy.timerTimer(Sender: TObject);
begin
  FProgressForm.timer.enabled := FALSE;
  freeAndNIL(FProgressForm);
end;

initialization
  AB; // to create the appEvents.listener and IProgramUpdates

end.
