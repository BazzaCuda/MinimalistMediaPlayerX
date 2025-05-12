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
unit mmpFormConfirmDelete;

interface

uses
  winApi.windows, winApi.Messages,
  system.imageList, system.sysUtils, system.variants, system.classes, vcl.graphics,
  vcl.controls, vcl.dialogs, vcl.extCtrls, vcl.forms, vcl.imaging.pngImage, vcl.imgList, vcl.stdCtrls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  mmpConsts;

type
  TDeletionObject = (doFile, doFolder, doKeepDelete, doCleanup);

  IConfirmDeleteForm = interface
    ['{96B437DD-E174-4008-9DCA-C55BE40D5BF2}']
  end;

  {$REGION}
  TConfirmDeleteForm = class(TForm)
    btnYes: TButton;
    btnNo: TButton;
    imgDeleteFolder: TImage;
    lblConfirm: TLabel;
    imgDeleteFile: TImage;
    lblItemToDelete: TLabel;
    lblTitle: TLabel;
    lblSubFolders: TLabel;
    imgDeleteMethod: TImage;
    lblRecycle: TLabel;
    ImageList: TImageList;
    lblStandard: TLabel;
    lblShred: TLabel;
    lblDeleteMethod: TLabel;
    lblGoneMeansGone: TLabel;
    Label1: TLabel;
    lblKeepFiles: TLabel;
    chbDeleteFolder: TCheckBox;
    procedure FormActivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure chbDeleteFolderClick(Sender: TObject);
  private
  public
    constructor create(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod);
  end;
  {$ENDREGION}

function mmpShowConfirmDelete(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod): TModalResult;

implementation

{$R *.dfm}

uses
  mmpFuncProg, mmpGlobalState,
  model.mmpConfigFile,
  _debugWindow;

function mmpShowConfirmDelete(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod): TModalResult;
begin
  result := mrNo;
  mmp.cmd(evGSUserInput, TRUE);
  with TConfirmDeleteForm.create(aPath, aDeletionObject, aDeleteMethod) do begin
    result := showModal;
    free;
  end;
  mmp.cmd(evGSUserInput, FALSE);
end;

function wrapText(const aText: string; const aTextWidth: integer; const aMaxWidth: integer): string;
var TF, FF: TSFunc;
begin
  TF := function:string begin
                          var vChop := length(aText) div 2;
                          repeat
                            inc(vChop);
                          until (vChop > length(aText)) or (aText[vChop] = '\');
                          var vLine1 := copy(aText, 1, vChop) + '...';
                          var vLine2 := '...\' + copy(aText, vChop + 1);
                          result := vLine1 + #13#10 + vLine2; end;

  FF := function:string begin result := aText; end;

  result := mmp.cmd(aTextWidth > aMaxWidth, TF, FF);
end;

{ TConfirmDeleteForm }

procedure TConfirmDeleteForm.chbDeleteFolderClick(Sender: TObject);
begin
  btnYes.enabled := chbDeleteFolder.checked;
end;

constructor TConfirmDeleteForm.create(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod);
const
  TITLE_CAPTIONS:   array[TDeletionObject]  of string = ('Delete File', 'Delete Folder Contents', 'Delete all but the "[K]eep" files', 'Cleanup Timeline Editing Files');
  CONFIRM_CAPTIONS: array[TDeleteMethod]    of string = ('Confirm Recycle', 'Confirm Delete', 'Confirm Shred');
  TYPE_CAPTIONS:    array[TDeletionObject]  of string = (' File?', ' Folder?', ' non-"[K]eep" files?', ' Timeline Editing Files?');
begin
  inherited create(NIL);

  borderIcons := [];

  label1.caption            := extractFilePath(aPath); // lblItemToDelete.canvas.textWidth(...) wasn't even close for some reason
  var vCaption              := wrapText(extractFilePath(aPath), label1.width, lblItemToDelete.width - 100); // -100 to create a minimum 50-pixel margin on each end

  case aDeletionObject of doFile: vCaption := vCaption + #13#10 + extractFileName(aPath); end;
  lblItemToDelete.caption   := vCaption;

  imgDeleteFolder.visible   := aDeletionObject in [doFolder, doKeepDelete, doCleanup];
  imgDeleteFile.visible     := aDeletionObject = doFile;

  chbDeleteFolder.visible   := aDeletionObject in [doFolder, doKeepDelete, doCleanup];
  btnYes.enabled            := chbDeleteFolder.checked or NOT (aDeletionObject in [doFolder, doKeepDelete, doCleanup]);

  lblTitle.caption          := TITLE_CAPTIONS[aDeletionObject];

  lblSubFolders.visible     := aDeletionObject in [doFolder, doKeepDelete, doCleanup];
  lblKeepFiles.visible      := aDeletionObject = doKeepDelete;

  lblDeleteMethod.caption   := 'deleteMethod=' + CF[CONF_DELETE_METHOD];

  lblConfirm.caption        := CONFIRM_CAPTIONS[aDeleteMethod];

  lblConfirm.caption        := lblConfirm.caption + TYPE_CAPTIONS[aDeletionObject];

  lblRecycle.visible        := aDeleteMethod = dmRecycle;
  lblStandard.visible       := aDeleteMethod = dmStandard;
  lblShred.visible          := aDeleteMethod = dmShred;

  lblGoneMeansGone.visible  := aDeleteMethod = dmShred;

  imageList.getBitmap(ord(aDeleteMethod), imgDeleteMethod.picture.bitmap);

  var vScaleFactor          := CF.asInteger[CONF_SCALE_FACTOR];
  vScaleFactor              := mmp.use(vScaleFactor <  MIN_SCALE_FACTOR, MIN_SCALE_FACTOR, vScaleFactor);
  vScaleFactor              := mmp.use(vScaleFactor >  MAX_SCALE_FACTOR, MAX_SCALE_FACTOR, vScaleFactor);
  SELF.scaleBy(vScaleFactor, MAX_SCALE_FACTOR);
  imgDeleteMethod.top       := lblShred.top;
  imgDeleteMethod.left      := lblShred.left - imgDeleteMethod.width;

  setWindowPos(SELF.handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE OR SWP_NOSIZE); // otherwise it can end up behind the Image & Thumbnail Browser window
end;

procedure TConfirmDeleteForm.FormActivate(Sender: TObject);
begin
  btnNo.setFocus;
end;

procedure TConfirmDeleteForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case key = VK_ESCAPE of TRUE: modalResult := mrNo; end;
end;

end.
