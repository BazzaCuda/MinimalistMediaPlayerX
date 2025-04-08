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
  TDeletionObject = (doFile, doFolder, doKeepDelete);

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
    procedure FormActivate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
    constructor create(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod);
  end;
  {$ENDREGION}

function mmpShowConfirmDelete(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod): TModalResult;

implementation

{$R *.dfm}

uses
  mmpDoProcs, mmpGlobalState,
  model.mmpConfigFile,
  _debugWindow;

function mmpShowConfirmDelete(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod): TModalResult;
begin
  result := mrNo;
  mmpDo(evGSUserInput, TRUE);
  with TConfirmDeleteForm.create(aPath, aDeletionObject, aDeleteMethod) do begin
    result := showModal;
    free;
  end;
  mmpDo(evGSUserInput, FALSE);
end;

function wrapText(const aText: string; const aTextWidth: integer; const aMaxWidth: integer): string;
begin
  case aTextWidth > aMaxWidth of   TRUE:  begin
                                            var vChop := length(aText) div 2;
                                            repeat
                                              inc(vChop);
                                            until (vChop > length(aText)) or (aText[vChop] = '\');
                                            var vLine1 := copy(aText, 1, vChop) + '...';
                                            var vLine2 := '...\' + copy(aText, vChop + 1);
                                            result := vLine1 + #13#10 + vLine2; end;
                                  FALSE:  result := aText; end;
end;

{ TConfirmDeleteForm }

constructor TConfirmDeleteForm.create(const aPath: string; const aDeletionObject: TDeletionObject; const aDeleteMethod: TDeleteMethod);
begin
  inherited create(NIL);

  label1.caption := extractFilePath(aPath); // lblItemToDelete.canvas.textWidth(...) wasn't even close for some reason
  var vCaption := wrapText(extractFilePath(aPath), label1.width, lblItemToDelete.width);
  case aDeletionObject of doFile: vCaption := vCaption + #13#10 + extractFileName(aPath); end;
  lblItemToDelete.caption := vCaption;

  imgDeleteFolder.visible := aDeletionObject in [doFolder, doKeepDelete];
  imgDeleteFile.visible   := aDeletionObject = doFile;

  case aDeletionObject of
    doFile:       lblTitle.caption := 'Delete File';
    doFolder:     lblTitle.caption := 'Delete Folder Contents';
    doKeepDelete: lblTitle.caption := 'Delete all but the "[K]eep" files';
  end;

  lblSubFolders.visible := aDeletionObject in [doFolder, doKeepDelete];
  lblKeepFiles.visible  := aDeletionObject = doKeepDelete;

  lblDeleteMethod.caption := 'deleteMethod=' + CF[CONF_DELETE_METHOD];

  case aDeleteMethod of
    dmRecycle:  lblConfirm.caption := 'Confirm Recycle';
    dmStandard: lblConfirm.caption := 'Confirm Delete';
    dmShred:    lblConfirm.caption := 'Confirm Shred'; end;

  case aDeletionObject of
    doFile:       lblConfirm.caption := lblConfirm.caption + ' File?';
    doFolder:     lblConfirm.caption := lblConfirm.caption + ' Folder?';
    doKeepDelete: lblConfirm.caption := lblConfirm.caption + ' non-"[K]eep" files?'; end;

  lblRecycle.visible  := aDeleteMethod = dmRecycle;
  lblStandard.visible := aDeleteMethod = dmStandard;
  lblShred.visible    := aDeleteMethod = dmShred;

  lblGoneMeansGone.visible := aDeleteMethod = dmShred;

  imageList.getBitmap(ord(aDeleteMethod), imgDeleteMethod.picture.bitmap);

  var vScaleFactor := CF.asInteger[CONF_SCALE_FACTOR];
  case vScaleFactor <  50 of TRUE: vScaleFactor :=  50; end;
  case vScaleFactor > 100 of TRUE: vScaleFactor := 100; end;
  SELF.scaleBy(vScaleFactor, 100);
  imgDeleteMethod.top     := lblShred.top;
  imgDeleteMethod.left    := lblShred.left - imgDeleteMethod.width;

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
