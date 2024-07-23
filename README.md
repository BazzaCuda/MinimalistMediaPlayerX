MMP: Minimalist Media Player
=====================

Good or bad, what do you like or dislike about `MMP`? Let me know [here](https://github.com/BazzaCuda/MinimalistMediaPlayerX/discussions/47). I'm _**very**_ keen to hear from you.

`MMP` (`v4.0.0`) released! Now with multiple delete methods (recycle, delete or shred) and playlist filtering to automatically play through only the images, audio or video on an entire drive.

`MMP` (`v3.0.3`) can now play through all the media files on an entire drive, starting wherever you choose. Folders that don't contain supported media files will be skipped. For those folders that do, a new playlist will be created and played through automatically. You can, of course, have the [P]laylist panel open while you do this and watch `MMP` create each new playlist as it navigates through all your folders. You can use any of the playlist controls or double-click with the mouse to skip items in the list and jump to others. Requires the new `nextFolderOnEnd=yes` setting. See the release notes.

_N.B. Some v2 users reported issues with the main window resizing itself several times on normal-sized monitors, e.g. 1920 x 1080, until it eventually fitted.
These issues were fixed in v3._

MMP: Minimalist Media Player now has its own [domain](https://minimalistmediaplayer.com) and its own [wiki](https://minimalistmediaplayer.com) (currently being developed)

VERSION 3 is HERE!! Now with image and thumbnail browsing and image manipulation. (https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases)
![Clipboard Image (23)](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/4a5d4849-0dba-4c44-9ef0-13673afea03e)





VERSION 2 RELEASED: Fast, lossless audio/video editing 
- https://github.com/BazzaCuda/MinimalistMediaPlayerX/wiki/Lossless-Audio-Video-editing

Now also displays all IMAGES in the current playlist folder (.jpg, .jpeg, .bmp, .png, .webp, animated .gif, .avif, .jfif). If you set an image display duration, a slideshow of consecutive image files will be shown.
- https://github.com/BazzaCuda/MinimalistMediaPlayerX/releases

**NEW**: Now supports .AVIF and .JFIF image files which will be included automatically in the playlist and the slideshow.

**TIP**: You can crop an image by using zoom ([I]n/[O]ut) and pan (Ctrl-left, ctrl-right, ctrl-up, ctrl-down) until the video window shows only the part of the image you want, then [F5] screenshot it. The screenshot will include any changes you've made to the Brightness, Contrast, Gamma, Saturation or Rotation of the original image.

**TIP**: You can edit (Ctrl-[E]) an entire audio or video file using just [I] and [O] to set [I]n and [O]ut points. E.g. Use the TAB key to fast forward through a video: when you get to the next section you want to include, press [I]; when you get to the next section that you want to exclude, press [O]. When you're done, click **Export**. It really doesn't get much easier than that!!

**MMP: Minimalist Media Player**

A powerful and ``very`` effective media player built on MPV's API, libMPV-2.dll, which is, in turn, based on the famous FFMPEG.

``MMP: Minimalist Media Player`` provides a minimalist keyboard- and mouse-operated user interface. Minimal on-screen clutter allows videos to be viewed without distractions. There are no window borders and no window title bar/caption, and keyboard options allow you to also have no progress bar and no audio/video timestamp, if that's what you want.

Also, the window resizes to fit the video perfectly, eliminating ugly black borders/bands around the video, particularly above and below. All of this provides an immersive viewing experience even when played in window mode rather than full-screen. Despite all this, ``MMP: Minimalist Media Player``provides a wealth of functions via the keyboard and mouse for manipulating the user-interface and the media files themselves.

This is a complete rewrite of (and supercedes) https://github.com/BazzaCuda/MinimalistMediaPlayer (which is based on Windows Media Player). As such, this [here] incarnation of ``MMP: Minimalist Media Player`` provides far superior media file handling and playback functionality such as zoom, pan, step frame (forwards and backwards!), rotation, brightness control, etc.
![Clipboard Image (1)](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/dcf21a34-657a-4221-b926-58f06078d19e)



### Key Features
----------------
- Automatically matches the aspect ratio of the window to the aspect ratio of the video so it fits the window perfectly, but still allows you to zoom and pan. This is something the MPV player itself doesn't do - you can only have one or the other. So no ugly borders. Ever. Unless they're hard-baked into the video, of course.

- Opening a media file from Windows Explorer (etc.) will automatically fill the playlist with all media files in the same folder. The playlist is sorted in "natural order" to match the order your files are [usually] listed in your file manager, taking numeric digits in filenames into account.

- You can "fast forward" through an entire media file by holding down the TAB key. Or backwards by holding down CTRL-TAB. Thanks to MPV, the audio is maintained at the correct pitch.

- If a corresponding subtitle file exists for the media file (.srt file, for example), or if the media file contains subtitles, the subtitles will be shown automatically. [F7] will cycle through all the subtitle tracks in the media file.

- Full-resolution screenshots can be saved for individual frames and for a displayed image.

- All the keyboard and mouse functions are listed in a handy help panel which can be shown or hidden by pressing Ctrl-[H]. The full functionality of ``MMP: Minimalist Media Player`` can be used while this panel is showing so you can try things out.

- ``MMP: Minimalist Media Player`` provides handy keyboard access to brightness, contrast, gamma and saturation. Handy tip: quite often, if you want to brighten the picture, increasing the gamma setting (and sometimes the contrast) gives better results than going straight to the brightness control.

- You can quickly bookmark the current position of a media file and restart from that point at a later time.

- You can quickly resize the video by holding down the [G] key to enlarge it. Or quickly reduce it by holding down Ctrl-[G].

- ``MMP: Minimalist Media Player`` provides full zoom and pan functions as well as video rotation.

- MPV playback can be configured via the normal mpv.conf file. As always with this file, some experimentation will be required to get the effect you require.

- N.B. Many keys can be held down for rapid repitition.

The following list will be missing some of the very latest - see the [Wiki](https://minimalistmediaplayer.com) for an up to date and detailed description of all of `MMP`'s many controls and features.

### Keyboard Controls (Main Window*)
---------------------
\* For the additional controls available in the `Image & Thumbnail Browser` see the `Ctrl-[H]` help panel in `MMP`'s image browser.

Although there are a lot of keyboard controls, the alphabetic letters have been applied intuitively. The action description will therefore help to associate the action with the letter.

Control | Action
------- | ------
`Ctrl-H`                | toggle the [H]elp panel listing all the keyboard and mouse functions
`ESCape`                | exit Fullscreen mode, or exit the app if not in fullscreen mode
`SPACEBAR`              | pause/resume playback of audio/video. pause/resume slideshow of images
`BACKSPACE`             | reset brightness, contrast, gamma, pan, rotation, saturation, speed, zoom to their defaults
`Ctrl and mouse`        | with a Ctrl key down, run your mouse along the progress bar to quickly "scrub" backwards and forwards through the video
`Right Arrow`           | pauses the video and steps forwards one frame (or next image). Can be held down for rapid continuous stepping
`Left Arrow`            | pauses the video and steps backwards one frame (or prev image). Can be held down for rapid continuous stepping
`Up Arrow`              | increase the volume. Can be held down for rapid increase (or mouse wheel)
`Down Arrow`            | decrease the volume. Can be held down for rapid decrease (or mouse wheel)
`Ctrl-Up Arrow`         | pan up. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Down Arrow`       | pan down. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Left Arrow`       | pan left. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Right Arrow`      | pan right. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Left button down`      | drag the media window around the screen
`Pg Up`                 | rotate video left/anti-clockwise (you must have first rotated right at least once)
`Pg Dn`                 | rotate video right/clockwise (you must do this at least once before you can rotate left)
`INSERT`                | copy media file name (without the extension) to the clipboard. e.g. for saving the file after F12
`Vol+ media key`        | increase the volume. Can be held down for rapid increase
`Vol- media key`        | decrease the volume. Can be held down for rapid decrease
`Vol-mute media key`    | mute/unmute sound
`Numpad +`              | increase playback speed. Can be held down for rapid increase
`/`                     | increase playback speed. Can be held down for rapid increase
`Numpad -`              | decrease playback speed. Can be held down for rapid decrease
`\`                     | decrease playback speed. Can be held down for rapid decrease
`#`                     | briefly reshow the media caption (the filename and its position/number in the playlist)
`-` (Hyphen)            | decrease brightness. Can be held down for rapid decrease
`=`                     | increase brightness. Can be held down for rapid increase  
`{`                     | decrease saturation setting. Can be held down for rapid decrease
`}`                     | increase saturation setting. Can be held down for rapid increase
`[`                     | decrease gamma setting. Can be held down for rapid decrease
`]`                     | increase gamma setting. Can be held down for rapid increase
`;`                     | reset saturation to normal
`'` (single quote)      | reset gamma to normal

### Alphabetic and Numeric Controls (Main Window*)
---------------
\* For the additional controls available in the `Image & Thumbnail Browser` see the `Ctrl-[H]` help panel in `MMP`'s image browser

Control | Action
------- | ------
`A` or `HOME`           | play the first media file in the playlist (`Z`/`END` plays last)
`Ctrl-A`                | show [A]bout Box. Checks for updates if _autoUpdates=yes_ is in .conf
`B`                     | [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar and captions [B]righter/more prominent
`Shift-B`               | Make the progress [B]ar and captions darker/less prominent
`Ctrl-Shift-B`          | Reset the progress [B]ar and captions to default color
`C`                     | show/hide on-screen [C]aptions (toggle media file timestamp, hide media metadata)
`Ctrl-C`                | show/hide all on-screen [C]aptions (media file timestamp and media metadata)
`DEL`                   | [D]elete current media file (after user confirmation) - deletion functions can be disabled by deleting rot.exe
`Ctrl-DEL`              | [D]elete all files in the current media file's folder (after user confirmation) - subfolders are not affected - deletion functions can be disabled by deleting rot.exe
`D`                     | navigate to the next [D]irectory aka Folder
`Ctrl-D`                | navigate to the previous [D]irectory aka Folder 
`E`                     | [E]ars - mute/unmute sound
`Ctrl-E`                | toggle the media timeline [E]diting tools for lossless editing
`F`                     | show/cancel [F]ullScreen mode
`Ctrl-F`                | open your File Explorer at the current folder
`G`                     | [G]reater window size. Can be held down for rapid increase
`Ctrl-G`                | reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease
`H`                     | position the window [H]orizontally (and Vertically) in the center of the screen
`Ctrl-H`                | toggle the [H]elp panel listing all the keyboard and mouse functions
`I`                     | zoom [I]n. Can be held down for rapid zooming
`Ctrl-I`                | open an image in the `Image & Thumbnail Browser`
`J`                     | ad[J]ust the aspect ratio of the window to fit the aspect ratio of an image
`K`                     | mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`Ctrl-K`                | keep/delete: keep the `Kept` files in a folder, delete the others 
`L`                     | re[L]oad the playlist with all supported media files from the current folder. Useful when you have knowingly changed the folder contents while the app is running. Saves having to restart the app to access the new folder contents
`M`                     | Maximize the video to the largest allowable by the screen whilst maintaining the correct aspect ratio
`N`                     | mi[N]imize window to the Windows taskbar
`Ctrl-N`                | toggle NUMLOCK on and off. For controlling multiple `MMP` windows
`O`                     | zoom [O]ut. Can be held down for rapid zooming
`P`                     | show/hide the [P]laylist
`Ctrl-P`                | toggle [P]laylist filtering on and off
`Q`                     | play the previous media file in the [Q]ueue/playlist
`R`                     | [R]ename the current media file
`Ctrl-R`                | toggle infinite [R]epeat of the current media file
`S`                     | re[S]tart the current media file from the beginning, aka [S]tartover
`Ctrl-S`                | toggle [S]ubtitles on/off
`T`                     | Tab through the audio or video file. See below in Category section
`T`                     | if viewing an image, open the `Image & [T]humbnail Browser` 
`U`                     | [U]nzoom, i.e. re-fit the video to the window
`V`                     | synchronize all [V]ideo windows
`Ctrl-V`                | set System Vol to Max
`W` or `Numpad ENTER`   | [W]atch the next video in the list (or play the next audio or image)
`X`                     | e[X]it the application
`Y`                     | open the `Image & Thumbnail Browser`
`Z` or `END`            | play the last media file in the playlist (`A`/`HOME` plays the first)
`1`                     | reset the playback speed to normal, i.e. [1]00%
`2`                     | reset Contrast to normal
`3`                     | reset Panning to normal
`4`                     | reset Rotation to normal/not rotated
`5`                     | bookmark the current media file timestamp to the configuration file
`6`                     | retrieve a bookmarked media file timestamp from the configuration file and continue playback from that point
`7`                     | delete any previously saved/bookmarked media file timestamp for the current media file
`8`                     | decrease contrast. Can be held down for rapid decrease
`9`                     | increase contrast. Can be held down for rapid increase
`0`                     | reset brightness to normal
`-` (Hyphen)            | decrease brightness. Can be held down for rapid decrease
`=`                     | increase brightness. Can be held down for rapid increase  
`Ctrl-9`                | Auto-arrange up to 4 (or more) simultaneous videos
`Ctrl-0`                | close all running instances of MMP

### Full List of Controls By Category (Main Window*)
---------------
\* For the additional controls available in the `Image & Thumbnail Browser` see the `Ctrl-[H]` help panel in `MMP`'s image browser.

Control | Action
------- | ------
**Bookmarks** ||
`5`                     | bookmark the current media file timestamp to the configuration file
`6`                     | retrieve a bookmarked media file timestamp from the configuration file and continue playback from that point
`7`                     | delete any previously saved/bookmarked media file timestamp for the current media file
**Brightness** || 
`-`                     | decrease brightness. Can be held down for rapid decrease
`=`                     | increase brightness. Can be held down for rapid increase
`0`                     | reset brightness to normal
**Contrast** | 
`8` (Hyphen)            | decrease contrast. Can be held down for rapid decrease
`9`                     | increase contrast. Can be held down for rapid increase
`2`                     | reset contrast to normal 
**Editing** (Ctrl-E) |
`C`                     | [C]ut the segment under the cursor into two segments
`Ctrl-C`                | same as above but mark the left-hand segment as e[X]cluded (same as [I])
`I`                     | [I]n point: split segment at cursor and mark left-hand segment as e[X]cluded, i.e. deleted
`O`                     | [O]ut point: split segment at cursor and mark right-hand segment as e[X]cluded, i.e. deleted
`L`                     | [L]engthen the selected segment by one second
`S`                     | [S]horten the selected segment by one second
`M`                     | [M]erge the selected segment with the segment to its right
`N`                     | [N]erge the selected segment with the segment to its left
`R`                     | [R]estore the selected e[X]cluded segment
`X`                     | e[X]clude the selected segment from the final edit
**File Control** | 
`INSERT`                | copy media file name (without the extension) to the clipboard. e.g. for saving the project file after F12
`DEL`                   | [D]elete current media file (after confirmation)
`Ctrl-DEL`              | [D]elete all files in the current media file's folder (after user confirmation) - subfolders are not affected - deletion functions can be disabled by deleting rot.exe
`K`                     | mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`Ctrl-K`                | Keep/delete: delete all but the Kept files in a folder 
`R`                     | [R]ename the current media file
**Freeze Frame** | 
`Right Arrow`           | pauses the video and steps forwards one frame. Can be held down for rapid continuous stepping.
`Left Arrow`            | pauses the video and steps backwards one frame. Can be held down for rapid continuous stepping.
**Gamma** | 
`[`                     | decrease gamma. Can be held down for rapid decrease
`]`                     | increase gamma. Can be held down for rapid increase
`'` (single quote)      | reset gamma
**Mouse** | 
`Left click the window and hold` | the window can be dragged/repositioned
`Left double-click the window`   | toggle fullscreen mode
`Right single-click the window`  | reserved for future context menu
`CTRL`                           | hold ctrl key down and move mouse along the progress bar to "scrub" backwards and forwards through the media file
`Wheel up`                       | volume up
`Wheel down`                     | volume down
**On-Screen Display** | 
`#`                     | briefly reshow the media caption (the filename and its position/number in the playlist)
`B`                     | [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar [B]righter/more prominent. Hold for rapid increase. Your preference gets saved to the configuration file
`Shift-B`               | Make the progress [Bar] darker. Hold for rapid decrease. Your preference gets saved to the configuration file
`Ctrl-Shift-B`          | reset progress [B] to its default. Your preference gets saved to the configuration file
`C`                     | show/Hide on-screen [C]aptions (media info) and media file timestamp
`Ctrl-C`                | show/Hide all on-screen captions, media file timestamp and media metadata
`Ctrl-S`                | toggle subtitles on/off
`F7`                    | cycle through all subtitle tracks in the media file
**Panning** | 
`Ctrl-Up Arrow`         | pan up. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Down Arrow`       | pan down. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Left Arrow`       | pan left. Can be held down for continuous panning (Ctrl-Shift for double speed)
`Ctrl-Right Arrow`      | pan right. Can be held down for continuous panning (Ctrl-Shift for double speed)
`3`                     | reset panning to normal
**Playback** |
`SPACEBAR`              | pause/resume playback of audio/video. pause/resume slideshow of images
`Right single-click the window`  | pause/resume playback of audio/video. pause/resume slideshow of images
`Ctrl-R`                | toggle infinite [R]epeat of the current media file
`S`                     | re[S]tart the current media file from the beginning, aka [S]tartover
`F8`                    | skip to the previous chapter, if there is one
`F9`                    | skip to the next chapter, if there is one
`Right Arrow`           | pauses the video and steps forwards one frame. Can be held down for rapid continuous stepping.
`Left Arrow`            | pauses the video and steps backwards one frame. Can be held down for rapid continuous stepping.
**Playlist** | 
`A`                     | play the first media file in the playlist (`Z`/`END` plays last)
`HOME`                  | play the first media file in the playlist (`Z`/`END` plays last)
`Z`                     | play the last media file in the playlist (`A`/`HOME` plays the first)
`END`                   | play the last media file in the playlist (`A`/`HOME` plays the first)
`W`                     | [W]atch the next video in the list (or play the next audio)
`Numpad ENTER`          | [W]atch the next video in the list (or play the next audio)
`P`                     | show/hide the [P]laylist
`Ctrl-P`                | toggle [P]laylist filtering on and off
`Q`                     | play the previous media file in the [Q]ueue/playlist
`L`                     | re[L]oad the playlist with all supported media files from the current folder. Useful when you have knowingly changed the folder contents while the app is running. Saves having to restart the app to access the new folder contents
**Rotating** | 
`Pg Up`                 | rotate video left/anti-clockwise (you must have first rotated right at least once)
`Pg Dn`                 | rotate video right/clockwise (you must do this at least once before you can rotate left)
`4`                     | reset rotation to normal/not rotated
**Saturation** | 
`{`                     | decrease saturation. Can be held down for rapid decrease
`}`                     | increase saturation. Can be held down for rapid increase
`;`                     | reset saturation to normal
**Screenshots** | 
`F5`                    | save a screenshot of the current frame or the currently displayed image
**Speed** | 
`Numpad -`              | decrease playback speed. Can be held down for rapid decrease
`\`                     | decrease playback speed. Can be held down for rapid decrease
`Numpad +`              | increase playback speed. Can be held down for rapid increase
`/`                     | increase playback speed. Can be held down for rapid increase
`1`                     | reset the playback speed to normal, i.e. [1]00%
**Subtitles** |
`Ctrl-S`                | toggle subtitles on/off
`F7`                    | cycle through all subtitle tracks in the media file
**Tabbing** | 
`T`                     | [T]ab through the media file a 100th (default) of its duration. Can be held down for rapid tabbing
`Ctrl-T`                | [T]ab back through the media file a 100th (default) of its duration. Can be held down for rapid tabbing
`TAB`                   | tab forwards 1 second. Can be tapped repeatedly or held down to do a "fast forward" through the media file
`Ctrl-TAB`              | tab backwards 1 second. Can be tapped repeatedly or held down to do a "fast reverse" through the media file
`Caps Lock-T`           | tab forwards 200th of media duration. Can be tapped repeatedly or held down for "fast forward"
`Caps Lock Ctrl-T`      | tab backwards 200th of media duration. Can be tapped repeatedly or held down for "fast reverse" 				
`Shift-T`               | tab forwards 50th of media duration
`Ctrl-Shift-T`          | tab backwards 50th of media duration
**Volume / Sound** | 
`Up Arrow`              | increase the volume. Can be held down for rapid increase
`Down Arrow`            | decrease the volume. Can be held down for rapid decrease
`Vol+ media key`        | increase the volume. Can be held down for rapid increase
`Vol- media key`        | decrease the volume. Can be held down for rapid decrease
`Vol-mute media key`    | mute/unmute sound
`E`                     | [E]ars - mute/unmute sound
`F6`                    | cycle through all audio tracks in the media file
`Ctrl-V`                | set System Volume to Max
**Window Control** | 
`ESCape`                | exit Fullscreen mode, or exit the app if not in fullscreen mode
`F`                     | show/cancel [F]ullScreen mode
`G`                     | [G]reater window size. Can be held down for rapid increase
`Ctrl-G`                | reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease
`H`                     | position the window [H]orizontally (and Vertically) in the center of the screen
`M`                     | Maximize the video to the largest allowable by the screen whilst maintaining the correct aspect ratio. Very occasionally, the window might be positioned so that the bottom edge is behind the Windows taskbar. This corrects that
`N`                     | mi[N]imize window to the Windows taskbar
`V`                     | synchronize all [V]ideo windows
`Ctrl-9`                | Auto-arrange up to 4 (or more) simultaneous videos
`Ctrl-0`                | close all running instances of MMP
`Left button down`      | drag the media window around the screen
**Zoom** | 
`I`                     | zoom [I]n. Can be held down for rapid zooming in
`O`                     | zoom [O]ut. Can be held down for rapid zooming out
`U`                     | [U]nzoom, i.e. re-fit the video to the window
**Additional** |
`F5`                    | save a screenshot of the current frame or the currently displayed image
`F6`                    | cycle through all audio tracks in the media file
`F7`                    | cycle through all subtitle tracks in the media file
`F8`                    | skip to the previous chapter, if there is one
`F9`                    | skip to the next chapter, if there is one
`F10`                   | open media file in PotPlayer (assumes default installation folder) - can be overridden in .conf
`F11`                   | open media file in LossLessCut (assumes default installation folder) - can be overridden in .conf            
`F12`                   | open media file in ShotCut editor (assumes default installation folder) - can be overridden in .conf
`Ctrl-A`                | show [A]bout Box. Checks for updates if _autoUpdates=yes_ is in .conf
`Ctrl-E`                | toggle the media timeline [E]diting tools for lossless editing
`Ctrl-F`                | open your File Explorer at the current folder
`Ctrl-H`                | show [H]elp panel of all keyboard and mouse controls
`Ctrl-I`                | open an image in the `[I]mage & Thumbnail Browser`
`Ctrl-R`                | toggle infinite [R]epeat of the current media file
`Ctrl-V`                | set System [V]olume to Max.
`INSERT`                | copy media file name (without the extension) to the clipboard. e.g. for saving the project file after F12
`BACKSPACE`             | reset brightness, contrast, gamma, pan, rotation, saturation, speed, zoom to their defaults
`SPACEBAR`              | pause/resume playback of audio/video. pause/resume slideshow of images
`T`                     | on an image, display the `Image & Thumbnail Browser`; on an audio or video file, Tab as normal
`Y`                     | display the `Image & Thumbnail Browser` regardless of what type of media file is currently showing

Media File Formats
------------------
``MMP: Minimalist Media Player`` uses MPV/FFMPEG for its media playback and manipulation functions and will therefore play the overwhelming majority of media files (audio/video/image).

Dependencies
------------
1. ``MMP: Minimalist Media Player`` requires libmpv-2.dll to be in the same folder as the executable. If you have an mpv.conf file in the same folder, MPV will take it into account when initializing, for example to specify your audio output preferences.

2. Each release comes with a program called "rot.exe". When deleting a file or the contents of a folder from within ``MMP: Minimalist Media Player``, it is actually rot.exe that carries out the deletion. This allows the deletion to be done in a separate process which results (in my experience) in less chances of Windows complaining that the file is in use (by MPV); although, having said that, MPV seems to be much better behaved in that regard than Windows Media Player on which the previous incarnation of ``MMP: Minimalist Media Player`` was based. If you wish, you can checkout rot.exe's many features by running it from the command line. Many of you will recognize it, I'm sure ;) If you want to completely disable the ability to delete files, simply delete rot.exe. Although ``MMP: Minimalist Media Player`` will still ask for confirmation, nothing will happen.

3. Massive credit and thanks are due to Edward Guo for his excellent port of the MPV API (libmpv) to Delphi, https://github.com/nbuyer/libmpvdelphi.

4. The VCL component in MarkdownHelpViewer (https://github.com/EtheaDev/MarkdownHelpViewer) allowed the original RTF help file to be replaced with much more convenient and flexible .md markdown resources.

5. Acknowledgement is due to the author of Perpetual Notes v4.05 (https://www.enselsoftware.com/product/PerpetualNotes.html) for being the only RTF editor I could find on the entire internet that could actually [,accurately] read back in what it had written out! (And I tested some big players during my search!)
