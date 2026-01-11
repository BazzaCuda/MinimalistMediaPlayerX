@echo ON

    copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.exe" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\"
    copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.map" "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Release\"
    if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.exe" "X:\MMP\")
    if exist "X:\MMP\" (copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\Win64\Debug\MinimalistMediaPlayer.map" "X:\MMP\")
    copy /Y "B:\Win64_Dev\Programs\MediaPlayerX\MinimalistMediaPlayer.dpr"  ".\_dprBackups\"


@echo.
@echo.
@echo.

@pause