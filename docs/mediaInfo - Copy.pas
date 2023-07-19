unit mediaInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Winapi.mediaFoundationApi.MFApi, {Winapi.mediaFoundationApi.MFMediaEngine,} Winapi.mediaFoundationApi.MFPlay, Winapi.mediaFoundationApi.MFIdl, Winapi.mediaFoundationApi.MFObjects,
  Winapi.mediaFoundationApi.MFReadWrite, system.win.comObj, winApi.ActiveX.objBase, winApi.comBaseApi;

type
  TMediaInfo = class(TObject)
  private
    procedure GetVideoBitrate(pSourceReader: IMFSourceReader);
  public
    function initMediaInfo(aURL: string): string;
  end;

function MI: TMediaInfo;

implementation

uses
  _debugWindow;

var
  gMI: TMediaInfo;

function MI: TMediaInfo;
begin
  case gMI = NIL of TRUE: gMI := TMediaInfo.create; end;
  result := gMI;
end;

{ TMediaInfo }

procedure TMediaInfo.GetVideoBitrate(pSourceReader: IMFSourceReader);
//var
//  dwMediaTypeIndex: DWORD;
//  pMediaType: IMFMediaType;
//  majorType: TGUID;
//  videoBitrate: UINT32;
//begin
//  dwMediaTypeIndex := 0;
//  pMediaType := nil;

//  // Get the current media type
//  if Succeeded(pSourceReader.GetNativeMediaType(dwMediaTypeIndex, 0, pMediaType)) then
//  begin
////    if Succeeded(pMediaType.GetGUID(MF_MT_MAJOR_TYPE, majorType)) and (majorType = MFMediaType_Video) then
////    begin
//      if Succeeded(pMediaType.GetUINT32(MF_MT_AVG_BITRATE, videoBitrate)) then
//      begin
//        debugInteger('Video Bitrate (bps): ', videoBitrate);
//      end;
////    end;
//  end;
var
  dwMediaTypeIndex: DWORD;
  MediaType: IMFMediaType;
  hr: HResult;
  VideoBitrate: UINT32;
  framesize: UINT64;
  width, height: cardinal;
begin
//  if succeeded(pSourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM, MediaType));
//  if Succeeded(hr) then
//  if Succeeded(pSourceReader.GetNativeMediaType(dwMediaTypeIndex, 0, MediaType)) then // this worked
  if succeeded(pSourceReader.GetCurrentMediaType(MF_SOURCE_READER_FIRST_VIDEO_STREAM, MediaType)) then
   begin
//    MediaType.GetUINT32(MF_MT_FRAME_RATE_RANGE_MAX, VideoBitrate);

// Retrieve the average time per frame
  var AvgTimePerFrame: Int64;
  MediaType.GetUINT64(MF_MT_FRAME_SIZE, frameSize);
  width := int64Rec(frameSize).hi;
  height := int64Rec(frameSize).lo;
  debugInteger('width', width);
  debugInteger('height', height);
//  debugString('lo', intToStr(lo(frameSize)));

  // Retrieve the time scale
//  var TimeScale: UINT32;
//  MediaType.GetUINT32(MF_MT_FRAME_RATE_RANGE_MAX, TimeScale);
//
//  // Calculate the frame rate
//  var FrameRate: Double := 10000000.0 / AvgTimePerFrame;
//  FrameRate := Round(FrameRate * TimeScale) / TimeScale;



//    debugInteger('Video Bitrate: ', VideoBitrate);
  end;
end;


function TMediaInfo.initMediaInfo(aURL: string): string;
var
  pSourceReader: IMFSourceReader;
  hr: HRESULT;
begin
  // Initialize COM
  if Failed(CoInitializeEx(nil, COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE)) then
    Exit;

  // Initialize Media Foundation
  if Failed(MFStartup(MF_VERSION)) then
  begin
    CoUninitialize;
    Exit;
  end;

  pSourceReader := nil;

  try
    // Create the media source reader
    hr := MFCreateSourceReaderFromURL('B:\Movies\Blazing Saddles (1974).mp4', nil, pSourceReader);
    if Succeeded(hr) then
    begin
      // Configure the source reader
      hr := pSourceReader.SetStreamSelection(MF_SOURCE_READER_ALL_STREAMS, FALSE);
      if Succeeded(hr) then
      begin
        hr := pSourceReader.SetStreamSelection(MF_SOURCE_READER_FIRST_VIDEO_STREAM, TRUE);
        if Succeeded(hr) then
        begin
          // Get video bitrate
          GetVideoBitrate(pSourceReader);
        end;
      end;
    end;
  finally
    // Clean up
//    if Assigned(pSourceReader) then
//      pSourceReader._Release;

    // Shut down Media Foundation
//    MFLockPlatform;
//    MFShutdown;
//    MFUnlockPlatform;

    // Uninitialize COM
//    CoUninitialize;
  end;

end;

initialization
  gMI := NIL;

finalization
  case gMI <> NIL of TRUE: gMI.free; end;

end.
