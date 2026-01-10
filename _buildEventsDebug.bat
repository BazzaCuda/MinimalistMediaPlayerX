@echo on

set logFile="B:\Win64_Dev\Programs\MediaPlayerX\_buildEvents.log"
echo *** Debug Build Event Called: %date% %time% *** >> %logFile%
echo. >> %logFile%

echo EXE: %1 >> %logFile%
echo MAP: %2 >> %logFile%
echo DPR: %3 >> %logFile%
echo. >> %logFile%

rem copy EXE to local Release folder
@echo copy %1 "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" >> %logFile% 2>&1
if exist "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" (
    copy /Y "%~1" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" >> %logFile% 2>&1
)
@echo --- >> %logFile%

rem copy MAP to local Release folder
@echo copy %2 "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" >> %logFile% 2>&1
if exist "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" (
    copy /Y %~2 "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\" >> %logFile% 2>&1
)
@echo --- >> %logFile%

rem copy EXE to X:\MMP if X: exists
@echo copy %1 "X:\MMP\" >> %logFile% 2>&1
if exist "X:\" (
    copy /Y %~1 "X:\MMP\" >> %logFile% 2>&1
)
@echo --- >> %logFile%

rem copy MAP to X:\MMP if X: exists
@echo copy %2 "X:\MMP\" >> %logFile% 2>&1
if exist "X:\" (
    copy /Y %~2 "X:\MMP\" >> %logFile% 2>&1
)
@echo --- >> %logFile%

rem backup DPR file if backup folder exists
@echo copy %3 ".\_dprBackups\" >> %logFile% 2>&1
if exist ".\_dprBackups\" (
    copy /Y %~3 ".\_dprBackups\" >> %logFile% 2>&1
)
echo *** >> %logFile%
echo. >> %logFile%


