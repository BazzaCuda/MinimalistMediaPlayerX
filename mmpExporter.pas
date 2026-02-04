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
unit mmpExporter;

interface

uses
  mmpAction, mmpConsts;

type
  IExporter = interface
    function copySourceFile:  boolean;
    function exportEdits:     boolean;
  end;

function mmpNewExporter(const aMediaFilePath: string; const aMediaType: TMediaType): IExporter;

implementation

uses
  winApi.windows,
  system.ioUtils, system.classes, system.generics.collections, system.sysUtils,
  vcl.controls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazCmd,
  mmpExportExec, mmpFileUtils, mmpGlobalState, mmpKeyboardUtils, mmpUtils,
  TSegmentClass,
  model.mmpConfigFile, model.mmpMediaInfo,
  view.mmpFormProgress,
  _debugWindow;

type
  TExporter = class(TInterfacedObject, IExporter)
  strict private
    FMediaFilePath: string;
    FMediaType:     TMediaType;

    FCancelled:     boolean;
    FProcessHandle: THandle;

    FProgressForm:      IProgressForm;
    FCreatedCoverArt:   boolean;
    FReAttachCoverArt:  boolean;
    FSegOneFN:          string;
  private
    function  concatSegments: boolean;
    function  createChaptersAndOrCoverArt(const bWriteChapters: boolean): boolean;
    function  createChaptersFromOutput: TVoid;
    function  createFinalFile(const bWriteChapters: boolean): boolean;
    function  createSegments(const bExportedCoverArt: boolean): boolean;
    function  createCoverArt: boolean;
    function  deletePreviousExport: TVoid;
    function  exportFailRerun(const aProgressForm: IProgressForm; const aSegID: string = EMPTY): TModalResult;
    function  fileChapterData: string;
    function  filePathCover: string;
    function  filePathLOG: string;
    function  filePathOUT(const bWriteChapters: boolean = FALSE; const aSuffix: string = ' [edited]'): string;
    function  filePathSEG: string;
    function  filePathTempChapters(const bWriteChapters: boolean; const aSuffix: string = ' [chapters]'): string;
    function  initProgressForm: IProgressForm;
    function  log(const aLogEntry: string): TVoid;
    function  playExportedMediaFile(const bMultiSegs: boolean): TVoid;
    function  segFileEntry(const aSegFile: string): string;
    function  showProgressForm(const aHeading: string; const aSubHeading: string; const aOnCancel: TNotifyEvent): IProgressForm;

    procedure onCancel(sender: TObject);
  public
    constructor Create(const aMediaFilePath: string; const aMediaType: TMediaType);

    function  copySourceFile:  boolean;
    function  exportEdits:     boolean;
  end;

function mmpNewExporter(const aMediaFilePath: string; const aMediaType: TMediaType): IExporter;
begin
  result := TExporter.create(aMediaFilePath, aMediaType);
end;

function segments: TObjectList<TSegment>; // alias
begin
  result := TSegment.segments;
end;

const
  STD_SEG_PARAMS      = ' -avoid_negative_ts make_zero -movflags +faststart -default_mode infer_no_subs -ignore_unknown';
  STD_MAP_METADATA_0  = ' -map_metadata 0';


{ TExporter }

function TExporter.showProgressForm(const aHeading: string; const aSubHeading: string; const aOnCancel: TNotifyEvent): IProgressForm;
begin
  result            := mmpNewProgressForm;
  result.heading    := aHeading;
  result.subHeading := aSubHeading;
  result.onCancel   := aOnCancel;
  result.formShow;
end;

function TExporter.copySourceFile: boolean;
//====== DO FFMPEG COPY ONLY ======
const
  COPY_PARAMS = ' -map 0 -c copy -ignore_unknown';
var
  cmdLine: string;
begin
  var vProgressForm := showProgressForm('Copying source file', 'Please wait', onCancel);

  cmdLine := '-hide_banner';
  cmdLine := cmdLine + ' -i "'  + FMediaFilePath + '"';
  cmdLine := cmdLine + COPY_PARAMS;
  var vFilePathOUT := filePathOUT(FALSE, ' [c]');

  vFilePathOUT := TAction<string>.pick(lowerCase(extractFileExt(vFilePathOUT)) = '.m4v', changeFileExt).default(vFilePathOUT).perform(vFilePathOUT, '.mp4');

  cmdLine := cmdLine + ' -y "' + vFilePathOUT + '"';
  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpegShow, FProcessHandle, FCancelled);

  mmp.cmd(result and fileExists(vFilePathOUT), procedure begin
//  case result of TRUE:  begin
                          //case fileExists(vFilePathOUT) of FALSE: EXIT; end;

                          var vWasPlaying := mmp.cmd(evMPReqPlaying).tf;
                          var vPosition   := mmp.cmd(evMPReqPosition).integer;
                          var vWasMuted   := CF.asBoolean[CONF_MUTED];

                          vProgressForm.heading := 'Loading Copy';

                          mmp.cmd(evVMReloadPlaylist);
                          mmpCopyMMPFile(FMediaFilePath, vFilePathOUT);
                          mmp.cmd(evPLFind, vFilePathOUT);
                          mmp.cmd(evPLFormLoadBox);

                          mmp.cmd(NOT vWasMuted, evMPMuteUnmute); // mute while we load and reposition the copy
                          FMediaFilePath := vFilePathOUT;
                          mmp.cmd(evVMMPPlayCurrent);
                          while mmp.cmd(evMPReqPosition).integer < 2 do mmpProcessMessages;
                          mmp.cmd(evMPSeek, vPosition);
                          mmp.cmd(NOT vWasMuted, evMPMuteUnmute); // unmute now we're at the correct timestamp

                          mmp.cmd(NOT vWasPlaying, evMPPause);
                        end); // end;
end;

constructor TExporter.Create(const aMediaFilePath: string; const aMediaType: TMediaType);
begin
  inherited Create;

  FMediaFilePath  := aMediaFilePath;
  FMediaType      := aMediaType;
  // TDebug.debugEnum<TMediaType>('aMediaType', aMediaType);
end;

function TExporter.concatSegments: boolean;
//====== CONCAT MULTIPLE SEGMENTS ======
begin
  // result := TRUE; // default to TRUE unless an FFmpeg process fails - pointless

//  var vWriteChapters := mmp.use<boolean>(FMediaType = mtAudio, CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE], CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE]);

  var cmdLine := EMPTY;

  // concatenate exported segments
  FProgressForm.subHeading := 'Joining segments';
  cmdLine := '-f concat -safe 0 -i "' + changeFileExt(FMediaFilePath, '.seg') + '"';

  cmdLine := cmdLine + ' -i "' + FMediaFilePath + '" -map_metadata 1'; // artist/title/album metadata donor

  // This is concat, not the segment export, so we want everything from "stream" 0, aka the list of concat files
  case FMediaType = mtAudio of   TRUE: cmdLine := cmdLine + ' -map 0:0 -c copy';
                                FALSE: cmdLine := cmdLine + ' -map 0   -c copy'; end;

  cmdLine := cmdLine + STD_SEG_PARAMS;

  cmdLine := cmdLine + ' -y "' + filePathOUT + '"';

  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled);
  case result of FALSE: case exportFailRerun(FProgressForm) = mrYes of TRUE: result := mmpExportExecAndWait(cmdLine, rtCMD, FProcessHandle, FCancelled); end;end;
end;

function TExporter.createChaptersAndOrCoverArt(const bWriteChapters: boolean): boolean;
//====== CREATE CHAPTERS FROM MULTIPLE SEG FILES ======
//======      ATTACH COVER.JPG IF IT EXISTS      ======

// audio file cover art is also re-attached at this stage
// if the caller doesn't want chapter metadata included, we still try to include the cover art

// bWriteChapters = TRUE forces us to write an MKV and to -attach the cover art
// bWriteChapters = FALSE and writing to an M4A, we have to use -disposition:v:0 attached_pic otherwise FFmpeg will turn the image into a video stream

{
Scenario                                      Input File (-i)             Temp File (-y)              Final File (Post-Rename)
Audio:  without Chapters, + Cover Art        File [edited].m4a           File [chapters].m4a             File [edited].m4a
Audio:  with Chapters, + Cover Art           File [edited].m4a,          File [chapters].mkv             File [edited].mkv
Video:  without Chapters,                    File [edited].mp4           File [chapters].mp4             File [edited].mp4
Video:  with Chapters                        File [edited].mp4           File [chapters].mkv             File [edited].mkv
}
begin
// if concat was ok, we can create chapters from the multiple .segnn files
// for Audio and Video, an MKV container format and a .mkv file extension will be enforced if bWriteChapters = TRUE
  result := FALSE;

  var cmdLine := EMPTY;

  mmp.cmd(FCreatedCoverArt and NOT FReAttachCoverArt, procedure begin mmpDeleteThisFile(filePathCover, [], TRUE, TRUE, FALSE); end);

  case bWriteChapters of   TRUE: FProgressForm.subHeading := 'Creating Chapters';
                          FALSE: FProgressForm.subHeading := 'Attaching Cover Art'; end;

  case bWriteChapters of TRUE:  begin
                                  createChaptersFromOutput;
                                  case fileExists(fileChapterData) of FALSE: EXIT; end;end;end;

  cmdLine := ' -i "' + filePathOUT + '" ';  // filePathOUT from the previous step

  case bWriteChapters of TRUE: cmdLine := cmdLine + ' -i "' +  fileChapterData + '" -map_metadata 1'; end;

  // M4A audio WITHOUT chapters (and other Video Stream / AtomicParsley Method types)
  case (FMediaType = mtAudio) and NOT bWriteChapters and fileExists(filePathCover) and ('.m4a.mp4.m4b.mov.flac.'.contains(extractFileExt(filePathOUT).toLower + '.')) of TRUE:
        cmdLine := cmdLine + ' -i "' + filePathCover + '" -map 0:a -map 1 -c copy -disposition:v:0 attached_pic'; end;

  // MP3 audio WITHOUT chapters (and other ID3v2 / Tagging Method types)
  case (FMediaType = mtAudio) and NOT bWriteChapters and fileExists(filePathCover) and ('.mp3.wav.aif.aiff.'.contains(extractFileExt(filePathOUT).toLower + '.'))  of TRUE:
        cmdLine := cmdLine + ' -i "' + filePathCover + '" -map 0:a -map 1 -c copy -id3v2_version 3 -metadata:s:t mimetype=image/jpeg'; end;

  // audio WITH chapters or MKV audio WITH OR WITHOUT chapters (and other Attachment / Metadata Block Method)
  case (FMediaType = mtAudio) and (bWriteChapters or (NOT bWriteChapters and ('.mkv.mka.'.contains(extractFileExt(filePathOUT).toLower + '.')))) and fileExists(filePathCover) of TRUE:
        cmdLine := cmdLine + ' -c copy -attach "' + filePathCover + '" -metadata:s:t mimetype=image/jpg -metadata:s:t filename="cover.jpg"'; end;

  cmdLine := cmdLine + STD_SEG_PARAMS + STD_MAP_METADATA_0;
  case FMediaType = mtVideo of TRUE: cmdLine := cmdLine + ' -c copy'; end;

  // FFmpeg always outputs to a [chapters] file to avoid potentially reading from and writing to the same file
  // The extension remains original if bWriteChapters is FALSE, otherwise .mkv is forced
  case fileExists(filePathTempChapters(bWriteChapters)) of TRUE: mmpDeleteThisFile(filePathTempChapters(bWriteChapters), [], TRUE, TRUE, FALSE); end;
  cmdLine := cmdLine + ' "' + filePathTempChapters(bWriteChapters) + '"';

  log(cmdLine); log(EMPTY);

  result  := mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled);
end;

function TExporter.createChaptersFromOutput: TVoid;
// get the actual segment lengths that FFmpeg created
type
  TSegment = record
    duration: integer;
    title:    string;
  end;
var
  segments: TList<TSegment>;
  vSegment: TSegment;
begin
  segments := TList<TSegment>.create;
  var vMI := mmpNewMediaInfo;  // Dependency Injection takes a back seat to Pragmatism, yet again :D

  for var vSegLine in TFile.readAllLines(filePathSEG) do begin
    var vParts  := vSegLine.split(['''']);
    var vFile   := vParts[1];

    case vMI.getMediaInfo(newNotice(vFile, mtVideo), TRUE).tf of   TRUE:  begin
                                                                            vSegment.duration := vMI.duration;
                                                                            vSegment.title    := vMI.title;
                                                                            segments.add(vSegment); end;end;
  end;

  var vSL := TStringList.create;
  try
    vSL.add(';FFMETADATA1');

    var vTimeStamp := 0;
//    var vSegCount  := 0;

    for vSegment in segments do begin

      vSL.add('[CHAPTER]');
      vSL.add('TIMEBASE=1/1');
      vSL.add(format('START=%d', [vTimeStamp]));

      case vTimeStamp = 0 of   TRUE: vSL.add(format('END=%d', [vTimeStamp + vSegment.duration]));
                              FALSE: vSL.add(format('END=%d', [vTimeStamp + vSegment.duration - 1])); end;

      // inc(vSegCount);
      vSL.add(format('title=%s', [vSegment.title]));

      case vTimeStamp = 0 of   TRUE: vTimeStamp := vTimeStamp + vSegment.duration + 1;
                              FALSE: vTimeStamp := vTimeStamp + vSegment.duration; end;
    end;

    vSL.saveToFile(fileChapterData);
  finally
    vSL.free;
    segments.free;
  end;
end;

function TExporter.createCoverArt: boolean;
//====== CHECK COVER ART ======
// when chapters are required, export any cover art to a separate file
// then re-attach it when adding the chapter metadata
// otherwise ffmpeg will create a video stream from it at the concat stage
// and an exported audio file will become a video file
begin
  result            := TRUE; // default to TRUE unless an FFmpeg process fails
  FCreatedCoverArt  := FALSE;

  FProgressForm.subHeading := 'Extracting Cover Art';

  case fileExists(filePathCover) of TRUE: EXIT; end; // we don't overwrite a user's beloved album art, they have to delete it themselves!

  var cmdLine := ' -i "' + FMediaFilePath + '" -map 0:V? -c copy "' + filePathCover + '"';
  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled);

  FCreatedCoverArt := result;
end;

function TExporter.createFinalFile(const bWriteChapters: boolean): boolean;
//====== NAME THE OUTPUT FILE ======
// and remove any intermediary files
begin
  result := TRUE; // unless one of the renames fails
  case bWriteChapters of   TRUE:  begin // replace the incoming [edited] file with the [chapters] file
                                    var vChapterContainer := filePathOut(bWriteChapters);
                                    case fileExists(vChapterContainer) of TRUE: mmpDeleteThisFile(vChapterContainer, [], TRUE, TRUE, FALSE); end;
                                    result := renameFile(filePathTempChapters(bWriteChapters), vChapterContainer);
                                    FProgressForm.subHeading := 'Deleting Intermediary File';
                                    case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end;
                                  end;
                           FALSE: begin // replace the incoming [edited] file with the [chapters] file, which audio used to re-attach the cover art
                                    case FMediaType of mtAudio: begin
                                                                  FProgressForm.subHeading := 'Deleting Intermediary File';
                                                                  case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end;
                                                                  result := renameFile(filePathTempChapters(bWriteChapters), filePathOUT); end;end;
                                  end;end;
end;

function TExporter.createSegments(const bExportedCoverArt: boolean): boolean;
//====== EXPORT SEGMENTS ======
begin
  result := TRUE; // default to TRUE unless an FFmpeg process fails

  var cmdLine       := EMPTY;
  FReAttachCoverArt := TRUE;

  var vSL := TStringList.create;
  try
    vSL.saveToFile(filePathSEG); // clear previous contents
    for var vSegment in segments do begin
      case vSegment.deleted of TRUE: CONTINUE; end;

      cmdLine := '-hide_banner'; // -v debug';

      cmdLine := cmdLine + ' -ss "' + intToStr(vSegment.startSS) + '"';
      cmdLine := cmdLine + ' -i "'  + FMediaFilePath + '"';
      cmdLine := cmdLine + ' -t "'  + intToStr(vSegment.duration) + '"';

      var vMaps       := EMPTY;

      for var vMediaStream in MI.mediaStreams do begin
        // exclude any cover art streams from the exported segments if we're going to be adding chapter metadata or concatenating multiple segments
        // otherwise FFmpeg will convert the cover art to a video stream during the concat stage
        case NOT vMediaStream.selected and (vMediaStream.streamType = 'Image') of TRUE: FReAttachCoverArt := FALSE; end;
        case bExportedCoverArt and (vMediaStream.streamType = 'Image') of TRUE: CONTINUE; end; // ignore the cover art - it will be re-attached later
        case vMediaStream.selected of TRUE: vMaps := vMaps + format(' -map 0:%d', [vMediaStream.Ix]); end;end;

      vMaps   := vMaps + ' -c copy -metadata title="' +vSegment.title + '"';
      cmdLine := cmdLine + vMaps;
      cmdLine := cmdLine + STD_SEG_PARAMS + STD_MAP_METADATA_0;

      var segFile := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + '.seg' + vSegment.segID + extractFileExt(FMediaFilePath);
      case TSegment.includedCount = 1 of TRUE: FSegOneFN := segFile; end;

      cmdLine := cmdLine + ' -y "' + segFile + '"';
      log(cmdLine); log(EMPTY);

      FProgressForm.dummyLabel.caption := extractFileName(segFile);
      FProgressForm.subHeading := mmpWrapText(extractFileName(segFile), FProgressForm.dummyLabel.width, FProgressForm.subHeadingWidth - 50, TRUE); // -50 to create a minimum 25-pixel margin on each end

      case  mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled) of
              TRUE:   vSL.add(segFileEntry(segFile));
              FALSE:  begin
                        result := FALSE; // stays false during all other segment exports
                        case exportFailRerun(FProgressForm, vSegment.segID) = mrYes of TRUE:  begin
                                                                                                vSL.add(segFileEntry(segFile)); // the user will correct the export
                                                                                                mmpExportExecAndWait(cmdLine, rtCmd, FProcessHandle, FCancelled); end;end;end;end; // result will still be FALSE
    end;
    vSL.saveToFile(filePathSEG);
  finally
    vSL.free;
  end;
end;

function TExporter.deletePreviousExport: TVoid;
//====== DELETE PREVIOUS EXPORT ======
begin
  // Previously, FFmpeg would have overwritten the output file, except now we don't run FFmpeg if we only have one segment
  // and renaming that segment would fail if we don't delete the output file or at least rename it. So we may as well delete it.
  case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end; // use the user's specified deleteMethod; don't mpvStop the video being edited

  while fileExists(filePathOUT) do mmpDelay(1 * MILLISECONDS); // give the thread time to run.
end;

function TExporter.exportEdits: boolean;
begin
  result := TRUE; // default to TRUE unless an FFmpeg process fails

  //====== SETUP PROGRESS FORM ======
  FProgressForm     := initProgressForm;

  // delete any previous .chp file up front to ensure that TVM.playEdited plays the correct [edited] if both exist after this export
  mmp.cmd(fileExists(fileChapterData), procedure begin mmpDeleteThisFile(fileChapterData, [], TRUE, TRUE, FALSE); end);

  var FSegOneFN       := EMPTY;
  var vWriteChapters  := mmp.use<boolean>(FMediaType = mtAudio, CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE], CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE]);
  var vExportCoverArt := (FMediaType = mtAudio);

  //====== CHECK COVER ART ======
  result := TAction<boolean>.pick(result and NOT mmpCtrlKeyDown and vExportCoverArt and mmp.cmd(evMIReqHasCoverArt).tf, createCoverArt).default(result).perform;

  //====== EXPORT SEGMENTS ======
  result := TAction<boolean>.pick(result and NOT mmpCtrlKeyDown, createSegments).default(result).perform(vExportCoverArt); // abort if at least one of the segment exports failed

  mmp.cmd(result, deletePreviousExport); // now we have newly-exported segment(s)

  //====== CHECK FOR SINGLE OR MULTIPLE SEGMENTS ======
  // single segment so just rename without the concat stage
  var vDoConcat := (FSegOneFN = EMPTY) or vWriteChapters; // vDoCancat is also a synonym for bHasMultiSegs
  TAction<boolean>.pick(result and NOT vDoConcat, renameFile).perform(filePathOUT);

  //====== CONCAT MULTIPLE SEGMENTS ======
  result := TAction<boolean>.pick(result and vDoConcat, concatSegments).default(result).perform;

  //====== CREATE CHAPTERS FROM MULTIPLE SEG FILES ======
  //======         AND/OR ATTACH COVER ART         ======
  result := TAction<boolean>.pick(result and (vWriteChapters or vExportCoverArt), createChaptersAndOrCoverArt).default(result).perform(vWriteChapters);

  //====== NAME THE OUTPUT FILE ======
  // and remove any intermediary files
  result := TAction<boolean>.pick(result, createFinalFile).default(result).perform(vWriteChapters);

  //====== PLAY THE EXPORTED MEDIA FILE ======
  TAction<TVoid>.pick(result and CF.asBoolean[CONF_PLAY_EDITED], playExportedMediaFile).perform(vDoConcat);

  TAction<TVoid>.pick(result, mmpDelay).perform(500); // so we can see the final message
  FProgressForm := NIL;
end;

function TExporter.exportFailRerun(const aProgressForm: IProgressForm; const aSegID: string = EMPTY): TModalResult;
begin
  case aSegID = EMPTY of   TRUE: aProgressForm.subHeading := 'Concatenation failed';
                          FALSE: aProgressForm.subHeading := format('Export of seg%s failed', [aSegID]); end;

  aProgressForm.modal := TRUE;

  aProgressForm.formHide;
  result := aProgressForm.formShowModal;

  aProgressForm.modal := FALSE;
  aProgressForm.formShow; // reshow non-modally after the showModal
end;

function TExporter.fileChapterData: string;
begin
  result := changeFileExt(filePathOUT, '.chp');
end;

function TExporter.filePathCover: string;
begin
  result := extractFilePath(FMediaFilePath) + 'cover.jpg'; // mandatory name!
end;

function TExporter.filePathLOG: string;
begin
  result := changeFileExt(FMediaFilePath, '.log');
end;

function TExporter.filePathTempChapters(const bWriteChapters: boolean; const aSuffix: string = ' [chapters]'): string;
begin
  result := filePathOUT(bWriteChapters, aSuffix);
end;

function TExporter.filePathOUT(const bWriteChapters: boolean = FALSE; const aSuffix: string = ' [edited]'): string;
begin
  result := extractFilePath(FMediaFilePath) + mmpFileNameWithoutExtension(FMediaFilePath) + aSuffix + extractFileExt(FMediaFilePath);
  case bWriteChapters of TRUE: result := mmpChapterContainer(result, FMediaType); end;
end;

function TExporter.filePathSEG: string;
begin
  result := changeFileExt(FMediaFilePath, '.seg');
end;

function TExporter.initProgressForm: IProgressForm;
//====== SETUP PROGRESS FORM ======
begin
  var vS1 := EMPTY; case segments.count   > 1 of  TRUE: vS1 := 's'; end; // it bugs me that so many programmers don't bother to do this! :D
  var vS2 := EMPTY; case MI.selectedCount > 1 of  TRUE: vS2 := 's'; end;

  result := showProgressForm(format('Exporting %d segment%s (%d stream%s)', [TSegment.includedCount, vS1, MI.selectedCount, vS2]), '', onCancel);
end;

function TExporter.log(const aLogEntry: string): TVoid;
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

procedure TExporter.onCancel(sender: TObject);
begin
  FCancelled := TRUE;
  terminateProcess(FProcessHandle, 1);
end;

function TExporter.segFileEntry(const aSegFile: string): string;
begin
  result := 'file ''' + stringReplace(aSegFile, '\', '\\', [rfReplaceAll]) + '''';
end;

function TExporter.playExportedMediaFile(const bMultiSegs: boolean): TVoid;
begin
  FProgressForm.subHeading := 'Playing Exported File';
  var vWriteChapters := mmp.use<boolean>(FMediaType = mtAudio, CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE], CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE]);
  case {bMultiSegs and} vWriteChapters of  TRUE: mmp.cmd(evVMMPPlayEdited, mmpChapterContainer(filePathOUT, FMediaType)); // EXPERIMENTAL why do we care about bMultiSegs?
                                          FALSE: mmp.cmd(evVMMPPlayEdited, filePathOUT()); end;
end;

end.
