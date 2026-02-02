@echo ON

tasklist | find /i "MinimalistMediaPlayer.exe"
if %errorlevel%==0 (
    taskkill /im MinimalistMediaPlayer.exe /f
    timeout /t 2 /nobreak
)

copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\MinimalistMediaPlayer.exe" "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer_v6_0_\"
if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\MinimalistMediaPlayer.exe" "X:\MMP\")
if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\MinimalistMediaPlayer.map" "X:\MMP\")
if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\MinimalistMediaPlayer.rsm" "X:\MMP\")
copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer.dpr"  ".\_dprBackups\"



@echo.
@echo.
@echo.



: @pause