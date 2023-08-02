Minimalist Media Player
=====================

A simple but ``very`` effective media player built on MVP's API, libMPV-2.dll

``Minimalist Media Player`` provides a minimalist keyboard- and mouse-operated user interface. Minimal on-screen clutter allows videos to be viewed without distractions. Keyboard options allow you to have no borders, no window title bar/caption, no progress bar, no video timestamp, etc.

Also, the window resizes to fit the video perfectly, eliminating ugly black borders/bands around the video, particularly above and below. This provides an immersive viewing experience even when played in window mode rather than full-screen. Despite all this, ``Minimalist Media Player``provides a wealth of functions via the keyboard and mouse for manipulating the user-interface and the media files themselves.

This is a complete rewrite of (and supercedes) https://github.com/BazzaCuda/MinimalistMediaPlayer (which is based on Windows Media Player). As such, ``Minimalist Media Player`` provides far superior media file handling and playback functionality such as zoom, pan, step frame (forwards and backwards!), brightness control, etc.

![Clipboard Image](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/6d824225-dd62-42f1-9b41-15527ed35edc)

### Key Features
----------------
Automatically matches the aspect ratio of the window to the aspect ratio of the video so it fits the window perfectly, but still allows you to zoom and pan. This is something the MPV player itself doesn't do - you can only have one or the other. So no ugly borders. Ever. Unless they're hard-baked into the video, of course.

Clicking on a media file in Windows Explorer (etc.) will automatically fill the playlist with all media files in the same folder. The playlist is sorted in "natural order" to match the order your files are listed in your file manager.

You can "fast forward" through an entire media file by holding down the TAB key. Or backwards by holding down CTRL-TAB. Thanks to MPV, the audio is maintained at the correct pitch.

All the keyboard and mouse functions are listed in a handy help panel which can be shown or hidden by pressing F1. The full functionality of ``Minimalist Media Player`` can be used while this panel is showing so you can try things out.

``Minimalist Media Player`` provides handy keyboard access to brightness, contrast, gamma and saturation. Handy tip: quite often, if you want to brighten the picture, increasing the gamma setting (and sometimes the contrast) gives better results than going straight to the brightness control.

You can quickly bookmark the current position of a media file and restart from that point at a later time.

You can quickly resize the video by holding down the G key to enlarge it. Or quickly reduce it by holding down Ctrl-G.

``Minimalist Media Player`` provides full zoom and pan functions as well as video rotation.

The app allows MPV to be configured via the normal mpv.conf file. As always with this file, some experimentation will be required to get the effect you require.

N.B. Many keys can be held down for rapid repitition.

### Keyboard Controls
---------------------
Although there are a lot of keyboard controls, each alphabetic letter has been applied intuitively. The action description with therefore help to associate the action with the letter.

Control | Action
------- | ------
`F1`                    | show or hide the Help panel listing all the keyboard and mouse functions
`ALT-SPACEBAR`          | activate the system menu to get to the About Box
`ESCape`                | exit Fullscreen mode, or exit the app if not in fullscreen mode
`SPACEBAR`              | pause/resume playback
`Numlock and mouse`     | with NumLock on, run your mouse along the progress bar to quickly "scrub" backwards and forwards through the video
`Ctrl and mouse`        | alternatively, hold a Ctrl key down to temporarily "scrub" along the progress bar with the mouse
`A` or `HOME`           | play the first media file in the playlist (Z/END plays last)
`B`                     | [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar [B]righter/more prominent.
`C`                     | show/Hide on-screen [C]ontrols (media info) and media file timestamp
`Ctrl-C`                | show/Hide all on-screen controls, media file timestamp and media metadata
`D` and `DEL`           | [D]elete current media file (after user confirmation) - deletion functions can be disabled by deleting rot.exe
`Ctrl-D and Ctrl-DEL`   | [D]elete all files in the current media file's folder (after user confirmation) - subfolders are not affected
`E`                     | [E]ars - Mute/Unmute sound
`F`                     | show/cancel [F]ullScreen mode
`G`                     | [G]reater window size. Can be held down for rapid increase.
`Ctrl-G`                | reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease.
`H`                     | position the window [H]orizontally (and Vertically) in the center of the screen
`I`                     | zoom [I]n. Can be held down for rapid zooming.
`J`                     | ad[J]ust the window's aspect ratio to match the video's aspect ratio. Gets rid of borders. Usually automatic.
`K`                     | mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`L`                     | re[L]oad the list of supported media files from the current folder
`M`                     | Display all media info for the current media file (in development)
`N`                     | mi[N]imize window to the Windows taskbar
`O`                     | zoom [O]ut. Can be held down for rapid zooming.
`P`                     | show/hide the [P]laylist (in development)
`Q`                     | play the previous media file in the [Q]ueue/playlist
`R`                     | [R]ename the current media file
`S`                     | re[S]tart the current media file from the beginning, aka [S]tartover
`T`                     | Tabbing. See below.
`U`                     | [U]nzoom, i.e. re-fit the video to the window
`V`                     | maximize / restore [V]iew
`W` or `Numpad ENTER`   | [W]atch the next video in the list (or play the next audio)
`X`                     | e[X]it the application
`Y`                     | Display all MPV settings (in development)
`Z` or `END`            | play the last media file in the playlist (A/HOME plays the first)
`BACKSPACE`             | reset zoom, pan, rotation, speed, brightness, contrast, gamma and saturation to normal
`1`                     | reset the playback speed to normal, i.e. [1]00%
`2`                     | reset panning to normal
`3`                     | reset brightness to normal
`4`                     | reset rotation to normal/not rotated
`5`                     | bookmark the current media file timestamp to the configuration file
`6`                     | retrieve a bookmarked media file timestamp from the configuration file and continue playback from that point
`7`                     | delete any previously saved/bookmarked media file timestamp for the current media file
`8`                     | decrease brightness. Can be held down for rapid decrease.
`9`                     | increase brightness. Can be held down for rapid increase.
`0`                     | decrease contrast. Can be held down for rapid decrease.
`-` (Hyphen)            | increase contrast. Can be held down for rapid increase.
`=`                     | reset contrast to normal 
`#`                     | briefly reshow the media caption (the filename and its position/number in the playlist)
`[`                     | decrease gamma setting. Can be held down for rapid decrease.
`]`                     | increase gamma setting. Can be held down for rapid increase.
`{`                     | decrease saturation setting. Can be held down for rapid decrease.
`}`                     | increase saturation setting. Can be held down for rapid increase.
`'` (single quote)      | reset gamma to normal
`;`                     | reset saturation to normal
INSERT                  | copy media file name (without the extension) to the clipboard. e.g. for saving the file after F12
### By Category
---------------
Control | Action
------- | ------
Brightness |
`8`                     | Decrease brightness. Can be held down for rapid decrease.
`9`                     | Increase brightness. Can be held down for rapid increase.
`3`                     | reset brightness to normal
Contrast |
`=`                     | increase contrast. Can be held down for rapid increase.
`-` (Hyphen)            | decrease contrast. Can be held down for rapid decrease.
`0`                     | reset contrast to normal 
File Control |
`INSERT`                | copy media file name (without the extension) to the clipboard. e.g. for saving the file after F12
`D` and `DEL`           | [D]elete current media file (after confirmation)
`K`                     | mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`R`                     | [R]ename the current media file
Freeze Frame |
`RIGHT ARROW`           | pauses the video and steps forwards one frame. Can be held down for rapid continuous stepping.
`LEFT ARROW`            | pauses the video and steps backwards one frame. Can be held down for rapid continuous stepping.
Gamma |
`[`                     | decrease gamma. Can be held down for rapid decrease;
`]`                     | increase gamma. Can be held down for rapid increase;
`'`                     | reset gamma
Mouse |
`Left Click on the window and hold` | the window can be dragged/repositioned
`Left double-click video`           | toggle fullscreen mode
`Right double-click video`          | toggle fullscreen mode
`Right single-click video`          | Pause/resume playback
`with NUMLOCK on`                   | move mouse along the progress bar to "scrub" backwards and forwards through the media file
`CTRL`                              | hold ctrl key down and move mouse along the progress bar. Same as above.
On-Screen Display |
`#`                     | briefly show media caption (the filename and its position in the playlist)
`B`                     | [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar [B]righter/more prominent. Hold for rapid increase. Your preference gets saved to the configuration file.
`Ctrl-Shift-B`          | Make the progress [Bar] darker. Hold for rapid decrease. Your preference gets saved to the configuration file.
`C`                     | show/Hide on-screen [C]ontrols (media info) and media file timestamp
`Ctrl-C`                | show/Hide all on-screen controls, media file timestamp and media metadata
Panning |
`Ctrl-Up Arrow`         | pan up. Can be held down for continuous panning.
`Ctrl-Down Arrow`       | pan down. Can be held down for continuous panning.
`Ctrl-Left Arrow`       | pan left. Can be held down for continuous panning.
`Ctrl-Right Arrow`      | pan right. Can be held down for continuous panning.
`2`                     | reset panning to normal.
Playlist |
`A`                     | play the first media file in the playlist (Z/END plays last)
`HOME`                  | play the first media file in the playlist (Z/END plays last)
`Z`                     | play the last media file in the playlist (A/HOME plays the first)
`END`                   | play the last media file in the playlist (A/HOME plays the first)
`W`                     | [W]atch the next video in the list (or play the next audio)
`Numpad ENTER`          | [W]atch the next video in the list (or play the next audio)
`Q`                     | play the previous media file in the [Q]ueue/playlist
`L`                     | re[L]oad the list of supported media files from the current folder
Rotating |
`Pg Up`                 | rotate video left/anti-clockwise (you must have first rotated right at least once)
`Pg Dn`                 | rotate video right/clockwise (you must do this at least once before you can rotate left)
`4`                     | reset rotation to normal/not rotated
Saturation |
`{`                     | decrease saturation. Can be held down for rapid decrease.
`}`                     | increase saturation. Can be held down for rapid increase.
`;`                     | reset saturation to normal
Screenshots |
`F5`                    | save a screenshot of the current frame (currently to the same folder as the media file)
Speed |
`Numpad +`              | increase playback speed. Can be held down for rapid increase.
`/`                     | increase playback speed. Can be held down for rapid increase.
`Numpad -`              | decrease playback speed. Can be held down for rapid decrease.
`\`                     | decrease playback speed. Can be held down for rapid decrease.
`1`                     | reset the playback speed to normal, i.e. [1]00%
Tabbing |
`T`                     | [T]ab through the media file a 100th (default), 200th or 10th of its duration (use ALT (10th) and CAPS LOCK (200th) to modify). Can be held down for rapid tabbing.
`Ctrl-T`                | [T]ab back through the media file a 100th (default), 200th or 10th of its duration (use ALT (10th) and CAPS LOCK (200th) to modify). Can be held down for rapid tabbing.
`TAB`                   | tab forwards 1 second. Can be tapped repeatedly or held down to do a "fast forward" through the media file
`Ctrl-TAB`              | tab backwards 1 second. Can be tapped repeatedly or held down to do a "fast reserve" through the media file
`CAPS LOCK T`           | tab forwards 200th of media duration. Can be tapped repeatedly or held down for "fast forward"
`CAPS LOCK Ctrl-T`      | tab backwards 200th of media duration. Can be tapped repeatedly or held down for "fast reverse" 				
Volume |
`Up Arrow`              | increase the volume. Can be held down for rapid increase.
`Down Arrow`            | decrease the volume. Can be held down for rapid decrease.
`Vol+ media key`        | increase the volume. Can be held down for rapid increase.
`Vol- media key`        | decrease the volume. Can be held down for rapid decrease.
`Vol-mute media key`    | Mute/Unmute sound
`E`                     | [E]ars - Mute/Unmute sound
Window Control |
`ESCape`                | exit Fullscreen mode, or exit the app if not in fullscreen mode
`F`                     | show/cancel [F]ullScreen mode
`G`                     | [G]reater window size. Can be held down for rapid increase.
`Ctrl-G`                | reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease.
`H`                     | position the window [H]orizontally (and Vertically) in the center of the screen
`J`                     | ad[J]ust the window's aspect ratio to match the video's aspect ratio. Gets rid of borders (usually done automatically)
`M`                     | [M]aximize / restore window
`N`                     | mi[N]imize window to the Windows taskbar
`V`                     | maximize / restore [V]iew, same as [M]
Zoom |
`I`                     | zoom [I]n. Can be held down for rapid zooming in.
`O`                     | zoom [O]ut. Can be held down for rapid zooming out.
`U`                     | [U]nzoom, i.e. re-fit the video to the window
Additional |
`F10`                   | open media file in PotPlayer (assumes default installation folder)
`F11`                   | open media file in LossLess Cut (assumes default installation folder)            
`F12`                   | open media file in ShotCut editor (assumes default installation folder)
INSERT                  | copy media file name (without the extension) to the clipboard. e.g. for saving the file after F12

Media File Formats
------------------
``Minimalist Media Player`` uses MVP for its media playback and manipulation functions and will play the overwhelming majority of media files.

Dependencies
------------
1. ``Minimalist Media Player`` requires libmvp-2.dll to be in the same folder as the executable. If you have an mvp.conf file in the same folder, mvp will take it into account when initializing, for example to specify your audio output preferences.

2. Each release comes with a program called "rot.exe". When deleting a file or the contents of a folder from within ``Minimalist Media Player``, it is actually rot.exe that carries out the deletion. This allows the deletion to be done in a separate process which results (in my experience) in less chances of Windows complaining that the file is in use (by MPV). If you wish, you can checkout rot.exe's many features by running it from the command line. Many of you will recognize it, I'm sure ;) If you want to completely disable the ability to delete files, simply delete rot.exe. Although ``Minimalist Media Player`` will still ask for confirmation, nothing will happen.
