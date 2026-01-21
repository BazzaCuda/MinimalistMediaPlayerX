unit mmpExportExec;

interface

type
  TRunType = (rtFFmpeg, rtCMD, rtFFmpegShow, rtFFprobe, rtFFProbeShow);

function mmpExportExecAndWait(const aCmdLine: string; const aRunType: TRunType; var vProcessHandle: THandle; var vCancelled: boolean): boolean;

implementation

uses
  winApi.shellApi, winApi.windows,
  mmpFileUtils, mmpUtils;


function mmpExportExecAndWait(const aCmdLine: string; const aRunType: TRunType; var vProcessHandle: THandle; var vCancelled: boolean): boolean;
// rtFFMPeg: normal running - the user just gets to see progress dialogs
// rtCMD: a segment export failed, so we rerun showing the cmd prompt box so the user can view the FFMPeg error messages
var
  vExecInfo: TShellExecuteInfo;
  vExitCode: cardinal;
begin
  vCancelled := FALSE;
  zeroMemory(@vExecInfo, SizeOf(vExecInfo));
  with vExecInfo do
  begin
    cbSize  := sizeOf(vExecInfo);
    fMask   := SEE_MASK_NOCLOSEPROCESS;
    Wnd     := 0;
    lpVerb  := 'open';

    case aRunType of
      rtFFmPeg,
      rtFFmpegShow:   lpFile := 'ffmpeg';
      rtFFprobe,
      rtFFProbeShow:  lpFile := 'ffprobe';
      rtCMD:          lpFile := 'cmd';
    end;

    case aRunType of
      rtFFmpeg, rtFFmpegShow, rtFFprobe, rtFFProbeShow: lpParameters := pChar(aCmdLine);
      rtCMD:                                            lpParameters := pChar(' /K ffmpeg ' + aCmdLine); // + ' > "B:\Downloads\New Folder\out.txt" 2>&1');
    end;

    lpDirectory := pWideChar(mmpExePath);

    case aRunType of
      rtFFmpeg:       nShow := SW_HIDE;
      rtCMD:          nShow := SW_SHOW;
      rtFFmpegShow:   nShow := SW_SHOW;
      rtFFprobe:      nShow := SW_HIDE;
      rtFFProbeShow:  nShow := SW_SHOW;
    end;

  end;

  result := ShellExecuteEx(@vExecInfo);
  case aRunType in [rtFFprobe, rtFFProbeShow] of FALSE: vProcessHandle := vExecInfo.hProcess; end;

  case result AND (vExecInfo.hProcess <> 0) of TRUE: begin // no handle if the process was activated by DDE
                                                      case aRunType of
                                                        rtFFmpeg, rtFFmpegShow, rtFFprobe, rtFFProbeShow, rtCmd:
                                                                  begin
                                                                    repeat
                                                                      case msgWaitForMultipleObjects(1, vExecInfo.hProcess, FALSE, INFINITE, QS_ALLINPUT) = (WAIT_OBJECT_0 + 1) of   TRUE: mmpProcessMessages;
                                                                                                                                                                                    FALSE: BREAK; end;
                                                                    until vCancelled;
                                                                    getExitCodeProcess(vExecInfo.hProcess, vExitCode);
                                                                    result := vExitCode = 0;
                                                                  end;
                                                        // rtCMD: result := TRUE;
                                                      end;
                                                      closeHandle(vExecInfo.hProcess);
                                                    end;
  end;
end;

end.
