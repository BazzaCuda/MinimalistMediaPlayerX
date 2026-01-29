@echo ON

tasklist | find /i "MinimalistMediaPlayer.exe"
if %errorlevel%==0 (
    taskkill /im MinimalistMediaPlayer.exe /f
    timeout /t 2 /nobreak
)

copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.exe" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\"
copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.map" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\"
copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.rsm" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\"

B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\madExceptPatch "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\MinimalistMediaPlayer.exe"

if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.exe" "X:\MMP\")
if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.map" "X:\MMP\")
if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.rsm" "X:\MMP\")
copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer.dpr"  ".\_dprBackups\"


@echo.
@echo.
@echo.

: @pause