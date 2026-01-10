@echo on

set logFile="B:\Win64_Dev\Programs\MediaPlayerX\_buildEvents.log"
echo *** Release Build Event Called: %date% %time% *** >> %logFile%
echo. >> %logFile%

echo EXE: %1 >> %logFile%
echo MAP: %2 >> %logFile%
echo DPR: %3 >> %logFile%
echo. >> %logFile%

rem Copy EXE to MinimalistMediaPlayer_v5_4_ if folder exists
echo copy %1 "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer_v5_4_\" >> %logFile% 2>&1
if exist "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer_v5_4_\" (
    copy /Y %~1 "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer_v5_4_\" >> %logFile% 2>&1
)
@echo --- >> %logFile%
echo. >> %logFile%

rem backup DPR file if backup folder exists
@echo copy %3 "B:\Win64_Dev\Programs\MediaPlayerX\_dprBackups\" >> %logFile% 2>&1
if exist ".\_dprBackups\" (
    copy /Y %~3 ".\_dprBackups\" >> %logFile% 2>&1
)
echo *** >> %logFile%
echo. >> %logFile%
