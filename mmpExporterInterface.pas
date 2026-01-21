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
unit mmpExporterInterface;

interface

uses
  mmpConsts;

type
  IExporter = interface
    function copySourceFile:  boolean;
    function exportEdits:     boolean;
  end;

function mmpExporter(const aMediaFilePath: string; const aMediaType: TMediaType): IExporter;

implementation

uses
  system.classes, system.sysUtils,
  winApi.windows,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazCmd,
  mmpExportExec, mmpFileUtils, mmpUtils,
  TSegmentClass,
  model.mmpConfigFile,
  view.mmpFormProgress;

type

  TExporter = class(TInterfacedObject, IExporter)
  strict private
    FMediaFilePath: string;
    FMediaType:     TMediaType;

    FCancelled:     boolean;
    FProcessHandle: THandle;
  private
    function  fileChapterData: string;
    function  filePathLOG: string;
    function  filePathMMP: string;
    function  filePathOUT(aSuffix: string = ' [edited]'): string;
    function  filePathSEG: string;
    function  filePathTempChapters(aSuffix: string = ' [chapters]'): string;
    function  log(const aLogEntry: string): boolean;

    procedure onCancelCopy(sender: TObject);
  public
    constructor Create(const aMediaFilePath: string; const aMediaType: TMediaType);

    function  copySourceFile:  boolean;
    function  exportEdits:     boolean;
  end;

function mmpExporter(const aMediaFilePath: string; const aMediaType: TMediaType): IExporter;
begin
  result := TExporter.create(aMediaFilePath, aMediaType);
end;

{ TExporter }

function TExporter.copySourceFile: boolean;
const
  COPY_PARAMS = ' -map 0 -c copy -ignore_unknown';
var
  cmdLine: string;
begin
  result      := FALSE;
  FCancelled  := FALSE;

  var vProgressForm := TProgressForm.create(NIL);
  vProgressForm.heading.caption     := 'Copying source file';
  vProgressForm.subHeading.caption  := 'Please wait';
  vProgressForm.onCancel            := onCancelCopy;
  vProgressForm.show;

  try
    cmdLine := '-hide_banner';
    cmdLine := cmdLine + ' -i "'  + FMediaFilePath + '"';
    cmdLine := cmdLine + COPY_PARAMS;
    var vFilePathOUT := filePathOUT(' [c]');

    case lowerCase(extractFileExt(vFilePathOUT)) = '.m4v' of TRUE: vFilePathOUT := changeFileExt(vFilePathOUT, '.mp4'); end; // FFmpeg fix ?

    cmdLine := cmdLine + ' -y "' + vFilePathOUT + '"';
    log(cmdLine); log(EMPTY);

    result := mmpExportExecAndWait(cmdLine, rtFFmpegShow, FProcessHandle, FCancelled);

    case result of TRUE:  begin
                            case fileExists(vFilePathOUT) of FALSE: EXIT; end;

                            var vWasPlaying := mmp.cmd(evMPReqPlaying).tf;
                            var vPosition   := mmp.cmd(evMPReqPosition).integer;
                            var vWasMuted   := CF.asBoolean[CONF_MUTED];

                            vProgressForm.heading.caption := 'Loading Copy';

                            mmp.cmd(evVMReloadPlaylist);
                            mmpCopyMMPFile(FMediaFilePath, vFilePathOUT);
                            mmp.cmd(evPLFind, vFilePathOUT);
                            mmp.cmd(evPLFormLoadBox);

                            case vWasMuted of FALSE: mmp.cmd(evMPMuteUnmute); end; // mute while we load and reposition the copy
                            FMediaFilePath := vFilePathOUT;
                            mmp.cmd(evVMMPPlayCurrent);
                            while mmp.cmd(evMPReqPosition).integer < 2 do mmpProcessMessages;
                            mmp.cmd(evMPSeek, vPosition);
                            case vWasMuted of FALSE: mmp.cmd(evMPMuteUnmute); end; // unmute now we're at the correct timestamp

                            case vWasPlaying of FALSE: mmp.cmd(evMPPause); end;
                          end;end;

  finally
    vProgressForm.free;
  end;


end;

constructor TExporter.Create(const aMediaFilePath: string; const aMediaType: TMediaType);
begin
  inherited Create;

  FMediaFilePath  := aMediaFilePath;
  FMediaType      := aMediaType;
end;

function TExporter.exportEdits: boolean;
begin


end;

function TExporter.fileChapterData: string;
begin
  result := changeFileExt(filePathOUT, '.chp');
end;

function TExporter.filePathTempChapters(aSuffix: string = ' [chapters]'): string;
begin
  result := mmpChapterContainer(filePathOUT(aSuffix), FMediaType);
end;

function TExporter.filePathOUT(aSuffix: string = ' [edited]'): string;
begin
  result := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + aSuffix + extractFileExt(FMediaFilePath);
end;

function TExporter.filePathLOG: string;
begin
  result := changeFileExt(FMediaFilePath, '.log');
end;

function TExporter.filePathMMP: string;
begin
  result := changeFileExt(FMediaFilePath, '.mmp');
end;

function TExporter.filePathSEG: string;
begin
  result := changeFileExt(FMediaFilePath, '.seg');
end;

function TExporter.log(const aLogEntry: string): boolean;
begin
  var vLogFile          := filePathLOG;
  var vLog              := TStringList.create;
  vLog.defaultEncoding  := TEncoding.UTF8;
  try
    case fileExists(vLogFile) of TRUE: vLog.loadFromFile(vLogFile); end;
    vLog.add(aLogEntry);
    vLog.saveToFile(vLogFile);
  finally
    vLog.free;
  end;
end;

procedure TExporter.onCancelCopy(sender: TObject);
begin
  FCancelled := TRUE;
  terminateProcess(FProcessHandle, 1);
end;


end.
