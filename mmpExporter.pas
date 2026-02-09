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

  TExportContext = record
    ecMediaFilePath:    string;
    ecMediaType:        TMediaType;
    ecWriteChapters:    boolean;
    ecHasCoverArt:      boolean;
    ecPlayEdited:       boolean;
    ecCtrlKeyDown:      boolean;

    ecWasPlaying:       boolean;
    ecWasMuted:         boolean;

    ecExportCoverArt:   boolean;
    ecExportedCoverArt: boolean;
    ecReAttachCoverArt: boolean;
    ecSegOneFN:         string;
    ecDoConcat:         boolean;
  end;

function mmpNewExporter(const aExportContext: TExportContext): IExporter;

implementation

uses
  winApi.windows,
  system.ioUtils, system.classes, system.generics.collections, system.sysUtils,
  vcl.controls,
  mmpNotify.notices, mmpNotify.notifier, mmpNotify.subscriber,
  bazCmd, bazFuncDefs,
  mmpExportExec, mmpFileUtils, mmpKeyboardUtils, mmpUtils,
  TSegmentClass,
  model.mmpMediaInfo,
  view.mmpFormProgress,
  _debugWindow;

type
  TExporter = class(TInterfacedObject, IExporter)
  strict private
    FCancelled:     boolean;
    FProcessHandle: THandle;

    FProgressForm:      IProgressForm;
    FEC:                TExportContext;
  private
    function  concatSegments: boolean;
    function  createChaptersAndOrCoverArt: boolean;
    function  createChaptersFromOutput: TVoid;
    function  createFinalFile: boolean;
    function  createSegments: boolean;
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
    function  playExportedMediaFile: TVoid;
    function  segFileEntry(const aSegFile: string): string;
    function  showProgressForm(const aHeading: string; const aSubHeading: string; const aOnCancel: TNotifyEvent): IProgressForm;

    procedure onCancel(sender: TObject);
  public
    constructor Create(const aExportContext: TExportContext); overload;

    function  copySourceFile:  boolean;
    function  exportEdits:     boolean;
  end;

function mmpNewExporter(const aExportContext: TExportContext): IExporter;
begin
  result := TExporter.create(aExportContext);
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
  cmdLine := cmdLine + ' -i "'  + FEC.ecMediaFilePath + '"';
  cmdLine := cmdLine + COPY_PARAMS;
  var vFilePathOUT := filePathOUT(FALSE, ' [c]');

  vFilePathOUT := TAction<string>.pick(lowerCase(extractFileExt(vFilePathOUT)) = '.m4v', changeFileExt).default(vFilePathOUT).perform(vFilePathOUT, '.mp4');

  cmdLine := cmdLine + ' -y "' + vFilePathOUT + '"';
  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpegShow, FProcessHandle, FCancelled);

  mmp.cmd(result and fileExists(vFilePathOUT), procedure begin
                          var vPosition := mmp.cmd(evMPReqPosition).integer; // wait for the copy to finish before getting the current position

                          vProgressForm.heading := 'Loading Copy';

                          mmp.cmd(evVMReloadPlaylist);
                          mmpCopyMMPFile(FEC.ecMediaFilePath, vFilePathOUT);
                          mmp.cmd(evPLFind, vFilePathOUT);
                          mmp.cmd(evPLFormLoadBox);

                          mmp.cmd(NOT FEC.ecWasMuted, evMPMuteUnmute); // mute while we load and reposition the copy
                          // FEC.ecMediaFilePath := vFilePathOUT; // ???
                          mmp.cmd(evVMMPPlayCurrent);
                          while mmp.cmd(evMPReqPosition).integer < 2 do mmpProcessMessages;
                          mmp.cmd(evMPSeek, vPosition);
                          mmp.cmd(NOT FEC.ecWasMuted, evMPMuteUnmute); // unmute now we're at the correct timestamp

                          mmp.cmd(NOT FEC.ecWasPlaying, evMPPause);
                        end); // end;
end;

constructor TExporter.Create(const aExportContext: TExportContext);
begin
  FEC                     := aExportContext;
  FEC.ecExportCoverArt    := (FEC.ecMediaType = mtAudio);
  FEC.ecExportedCoverArt  := FALSE;
  FEC.ecSegOneFN          := EMPTY;
  FEC.ecReAttachCoverArt  := FALSE;
  FEC.ecDoConcat          := FALSE;
end;

function TExporter.concatSegments: boolean;
//====== CONCAT MULTIPLE SEGMENTS ======
begin
  // result := TRUE; // default to TRUE unless an FFmpeg process fails - pointless

//  var vWriteChapters := mmp.use<boolean>(FMediaType = mtAudio, CF.asBoolean[CONF_CHAPTERS_AUDIO_WRITE], CF.asBoolean[CONF_CHAPTERS_VIDEO_WRITE]);

  var cmdLine := EMPTY;

  // concatenate exported segments
  FProgressForm.subHeading := 'Joining segments';
  cmdLine := '-f concat -safe 0 -i "' + changeFileExt(FEC.ecMediaFilePath, '.seg') + '"';

  cmdLine := cmdLine + ' -i "' + FEC.ecMediaFilePath + '" -map_metadata 1'; // artist/title/album metadata donor

  // This is concat, not the segment export, so we want everything from "stream" 0, aka the list of concat files
  case FEC.ecMediaType = mtAudio of  TRUE: cmdLine := cmdLine + ' -map 0:0 -c copy';
                                    FALSE: cmdLine := cmdLine + ' -map 0   -c copy'; end;

  cmdLine := cmdLine + STD_SEG_PARAMS;

  cmdLine := cmdLine + ' -y "' + filePathOUT + '"';

  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled);
  case result of FALSE: case exportFailRerun(FProgressForm) = mrYes of TRUE: result := mmpExportExecAndWait(cmdLine, rtCMD, FProcessHandle, FCancelled); end;end;
end;

function TExporter.createChaptersAndOrCoverArt: boolean;
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

  mmp.cmd(FEC.ecExportedCoverArt and NOT FEC.ecReAttachCoverArt, procedure begin mmpDeleteThisFile(filePathCover, [], TRUE, TRUE, FALSE); end);

  case FEC.ecWriteChapters of  TRUE: FProgressForm.subHeading := 'Creating Chapters';
                              FALSE: FProgressForm.subHeading := 'Attaching Cover Art'; end;

  case FEC.ecWriteChapters of TRUE: begin
                                      createChaptersFromOutput;
                                      case fileExists(fileChapterData) of FALSE: EXIT; end;end;end;

  cmdLine := ' -i "' + filePathOUT + '" ';  // filePathOUT from the previous step

  case FEC.ecWriteChapters of TRUE: cmdLine := cmdLine + ' -i "' +  fileChapterData + '" -map_metadata 1'; end;

  // M4A audio WITHOUT chapters (and other Video Stream / AtomicParsley Method types)
  case (FEC.ecMediaType = mtAudio) and NOT FEC.ecWriteChapters and fileExists(filePathCover) and ('.m4a.mp4.m4b.mov.flac.'.contains(extractFileExt(filePathOUT).toLower + '.')) of TRUE:
        cmdLine := cmdLine + ' -i "' + filePathCover + '" -map 0:a -map 1 -c copy -disposition:v:0 attached_pic'; end;

  // MP3 audio WITHOUT chapters (and other ID3v2 / Tagging Method types)
  case (FEC.ecMediaType = mtAudio) and NOT FEC.ecWriteChapters and fileExists(filePathCover) and ('.mp3.wav.aif.aiff.'.contains(extractFileExt(filePathOUT).toLower + '.'))  of TRUE:
        cmdLine := cmdLine + ' -i "' + filePathCover + '" -map 0:a -map 1 -c copy -id3v2_version 3 -metadata:s:t mimetype=image/jpeg'; end;

  // audio WITH chapters or MKV audio WITH OR WITHOUT chapters (and other Attachment / Metadata Block Method)
  case (FEC.ecMediaType = mtAudio) and (FEC.ecWriteChapters or (NOT FEC.ecWriteChapters and ('.mkv.mka.'.contains(extractFileExt(filePathOUT).toLower + '.')))) and fileExists(filePathCover) of TRUE:
        cmdLine := cmdLine + ' -c copy -attach "' + filePathCover + '" -metadata:s:t mimetype=image/jpg -metadata:s:t filename="cover.jpg"'; end;

  cmdLine := cmdLine + STD_SEG_PARAMS + STD_MAP_METADATA_0;
  case FEC.ecMediaType = mtVideo of TRUE: cmdLine := cmdLine + ' -c copy'; end;

  // FFmpeg always outputs to a [chapters] file to avoid potentially reading from and writing to the same file
  // The extension remains original if bWriteChapters is FALSE, otherwise .mkv is forced
  case fileExists(filePathTempChapters(FEC.ecWriteChapters)) of TRUE: mmpDeleteThisFile(filePathTempChapters(FEC.ecWriteChapters), [], TRUE, TRUE, FALSE); end;
  cmdLine := cmdLine + ' "' + filePathTempChapters(FEC.ecWriteChapters) + '"';

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
  result                  := TRUE; // default to TRUE unless an FFmpeg process fails
  FEC.ecExportedCoverArt  := FALSE;

  FProgressForm.subHeading := 'Extracting Cover Art';

  case fileExists(filePathCover) of TRUE: EXIT; end; // we don't overwrite a user's beloved album art, they have to delete it themselves!

  var cmdLine := ' -i "' + FEC.ecMediaFilePath + '" -map 0:V? -c copy "' + filePathCover + '"';
  log(cmdLine); log(EMPTY);

  result := mmpExportExecAndWait(cmdLine, rtFFmpeg, FProcessHandle, FCancelled);

  FEC.ecExportedCoverArt := result;
end;

function TExporter.createFinalFile: boolean;
//====== NAME THE OUTPUT FILE ======
// and remove any intermediary files
begin
  result := TRUE; // unless one of the renames fails
  case FEC.ecWriteChapters of  TRUE:  begin // replace the incoming [edited] file with the [chapters] file
                                        var vChapterContainer := filePathOut(FEC.ecWriteChapters);
                                        case fileExists(vChapterContainer) of TRUE: mmpDeleteThisFile(vChapterContainer, [], TRUE, TRUE, FALSE); end;
                                        result := renameFile(filePathTempChapters(FEC.ecWriteChapters), vChapterContainer);
                                        FProgressForm.subHeading := 'Deleting Intermediary File';
                                        case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end;
                                      end;
                              FALSE:  begin // replace the incoming [edited] file with the [chapters] file, which audio used to re-attach the cover art
                                        case FEC.ecMediaType of mtAudio:  begin
                                                                            FProgressForm.subHeading := 'Deleting Intermediary File';
                                                                            case fileExists(filePathOUT) of TRUE: mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end;
                                                                            result := renameFile(filePathTempChapters(FEC.ecWriteChapters), filePathOUT); end;end;
                                      end;end;
end;

function TExporter.createSegments: boolean;
//====== EXPORT SEGMENTS ======
begin
  result := TRUE; // default to TRUE unless an FFmpeg process fails

  var cmdLine             := EMPTY;
  FEC.ecReAttachCoverArt  := TRUE;

  var vSL := TStringList.create;
  try
    vSL.saveToFile(filePathSEG); // clear previous contents
    for var vSegment in segments do begin
      case vSegment.deleted of TRUE: CONTINUE; end;

      cmdLine := '-hide_banner'; // -v debug';

      cmdLine := cmdLine + ' -ss "' + intToStr(vSegment.startSS) + '"';
      cmdLine := cmdLine + ' -i "'  + FEC.ecMediaFilePath + '"';
      cmdLine := cmdLine + ' -t "'  + intToStr(vSegment.duration) + '"';

      var vMaps       := EMPTY;

      for var vMediaStream in MI.mediaStreams do begin
        // exclude any cover art streams from the exported segments if we're going to be adding chapter metadata or concatenating multiple segments
        // otherwise FFmpeg will convert the cover art to a video stream during the concat stage
        case NOT vMediaStream.selected and (vMediaStream.streamType = 'Image') of TRUE: FEC.ecReAttachCoverArt := FALSE; end;
        case FEC.ecExportedCoverArt and (vMediaStream.streamType = 'Image') of TRUE: CONTINUE; end; // ignore the cover art - it will be re-attached later
        case vMediaStream.selected of TRUE: vMaps := vMaps + format(' -map 0:%d', [vMediaStream.Ix]); end;end;

      vMaps   := vMaps + ' -c copy -metadata title="' +vSegment.title + '"';
      cmdLine := cmdLine + vMaps;
      cmdLine := cmdLine + STD_SEG_PARAMS + STD_MAP_METADATA_0;

      var segFile := extractFilePath(FEC.ecMediaFilePath) + mmpFileNameWithoutExtension(FEC.ecMediaFilePath) + '.seg' + vSegment.segID + extractFileExt(FEC.ecMediaFilePath);
      case TSegment.includedCount = 1 of TRUE: FEC.ecSegOneFN := segFile; end;

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
    FEC.ecDoConcat := (FEC.ecSegOneFN = EMPTY) or FEC.ecWriteChapters
  finally
    vSL.free;
  end;
end;

function TExporter.deletePreviousExport: TVoid;
//====== DELETE PREVIOUS EXPORT ======
begin
  // Previously, FFmpeg would have overwritten the output file, except now we don't run FFmpeg if we only have one segment
  // and renaming that segment would fail if we don't delete the output file or at least rename it. So we may as well delete it.
  mmp.cmd(fileExists(filePathOUT), procedure begin mmpDeleteThisFile(filePathOUT, [], TRUE, TRUE, FALSE); end);

  while fileExists(filePathOUT) do mmpDelay(1 * MILLISECONDS); // give the thread time to run.
end;

function TExporter.exportEdits: boolean; // v3
begin

  result := TAction<boolean>.startWith(TRUE) // default to TRUE unless an FFmpeg process fails

  //====== SETUP PROGRESS FORM ======
                            .aside<TVoid>(TRUE, function:TVoid begin FProgressForm := initProgressForm; end)

  //====== DELETE PREVIOUS CHAPTER DATA ======

                            // delete any previous .chp file up front to ensure that TVM.playEdited plays the correct [edited] if both exist after this export
                            // Standalone: Logic runs if file exists; result remains TRUE regardless
                            .aside<TVoid>(fileExists(fileChapterData), function:TVoid begin mmpDeleteThisFile(fileChapterData, [], TRUE, TRUE, FALSE); end) // non-standard parameter list

  //====== CHECK COVER ART ======

                            // Standalone: result remains TRUE even if NOT FEC.ecHasCoverArt
                            .aside(NOT FEC.ecCtrlKeyDown and FEC.ecExportCoverArt and FEC.ecHasCoverArt, createCoverArt)

  //====== EXPORT SEGMENTS ======

                            // CRITICAL PATH: Failure here sets result to FALSE and stops the chain
                            // abort if at least one of the segment exports fails
                            .andThen(NOT FEC.ecCtrlKeyDown, createSegments)
                            .aside(TRUE, function:boolean begin deletePreviousExport; result := TRUE; end) // now we have newly-exported segment(s)

  //====== CHECK FOR SINGLE OR MULTIPLE SEGMENTS ======

                            // single segment so just rename without the concat stage
                            // Aside rename doesn't overwrite the success of segments
                            .aside(NOT FEC.ecDoConcat, renameFile, FEC.ecSegOneFN, filePathOUT)

  //====== CONCAT MULTIPLE SEGMENTS ======

                            // CRITICAL PATH: Failure here sets result to FALSE
                            .andThen(FEC.ecDoConcat, concatSegments)

  //====== CREATE CHAPTERS FROM MULTIPLE SEG FILES ======
  //======         AND/OR ATTACH COVER ART         ======

                            // CRITICAL PATH: Combined chapter and cover art attachment stage
                            .andThen(FEC.ecWriteChapters or FEC.ecExportCoverArt, createChaptersAndOrCoverArt)

  //====== NAME THE OUTPUT FILE ======
  // and remove any intermediary files

                            .andThen(TRUE, createFinalFile)

  //====== PLAY THE EXPORTED MEDIA FILE ======

                            // Post-process actions that do not impact the final result
                            .aside(FEC.ecPlayEdited,  function:boolean begin playExportedMediaFile; result := TRUE; end) // these methods don't return a boolean so we force one which we then ignore
                            .aside(TRUE,              function:boolean begin mmpDelay(500);         result := TRUE; end) // delay briefly so we can see the final message

  //====== TEARDONW PROGRESS FORM ======

                            .aside(TRUE, function:boolean begin FProgressForm := NIL; end)

                            // return the final boolean to result
                            .thenStop;

end;

function TExporter.exportFailRerun(const aProgressForm: IProgressForm; const aSegID: string = EMPTY): TModalResult;
begin
  aProgressForm.subHeading := mmp.use<string>(aSegID = EMPTY, 'Concatenation failed', format('Export of seg%s failed', [aSegID]));

//  case aSegID = EMPTY of   TRUE: aProgressForm.subHeading := 'Concatenation failed';
//                          FALSE: aProgressForm.subHeading := format('Export of seg%s failed', [aSegID]); end;

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
  result := extractFilePath(FEC.ecMediaFilePath) + 'cover.jpg'; // mandatory name!
end;

function TExporter.filePathLOG: string;
begin
  result := changeFileExt(FEC.ecMediaFilePath, '.log');
end;

function TExporter.filePathTempChapters(const bWriteChapters: boolean; const aSuffix: string = ' [chapters]'): string;
begin
  result := filePathOUT(bWriteChapters, aSuffix);
end;

function TExporter.filePathOUT(const bWriteChapters: boolean = FALSE; const aSuffix: string = ' [edited]'): string;
begin
  result := extractFilePath(FEC.ecMediaFilePath) + mmpFileNameWithoutExtension(FEC.ecMediaFilePath) + aSuffix + extractFileExt(FEC.ecMediaFilePath);
  case bWriteChapters of TRUE: result := mmpChapterContainer(result, FEC.ecMediaType); end;
end;

function TExporter.filePathSEG: string;
begin
  result := changeFileExt(FEC.ecMediaFilePath, '.seg');
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

function TExporter.playExportedMediaFile{(const bMultiSegs: boolean)}: TVoid;
begin
  FProgressForm.subHeading := 'Playing Exported File';
  case {bMultiSegs and} FEC.ecWriteChapters of   TRUE: mmp.cmd(evVMMPPlayEdited, mmpChapterContainer(filePathOUT, FEC.ecMediaType)); // EXPERIMENTAL why do we care about bMultiSegs?
                                                FALSE: mmp.cmd(evVMMPPlayEdited, filePathOUT()); end;
end;

end.
