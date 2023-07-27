Minimalist Media Player
=====================

A simple but very effective media player built around MVP's API, libMPV-2.dll

``Minimalist Media Player`` provides both a minimalist/keyboard- and mouse-operated user interface (so that on-screen controls don't detract from the video) and the controls with which to view the video in a minimalist window with the option to have no borders, window title bar/caption, progress bar, video timestamp, etc, and with the window resized to fit the video perfectly, eliminating ugly black borders/bands around the video, particularly above and below. This provides an immersive viewing experience even when played in window mode rather than full-screen.

### Keyboard Controls
---------------------
Although there are a lot of controls to remember, I have tried to use each alphabetic letter intuitively. The action description should help to associate the action with the letter.

Control | Action
------- | ------
`SHIFT key (either)`    | show or hide the Help window listing all the keyboard functions
`ALT-SPACEBAR`			| activate the system menu to get to the About Box
`ESCape`				| exit Fullscreen mode, or exit the app if not in fullscreen mode
`SPACEBAR` 				| pause/resume playback
`Numlock and mouse`     | with NumLock on, run your mouse along the progress bar to quickly "scrub" backwards and forwards through the video
`Ctrl and mouse`        | alternatively, hold a Ctrl key down to temporarily "scrub" along the progress bar with the mouse
`=`						| copy media file name (without the extension) to clipboard. e.g. for saving the file after F12
`A`						| play the first media file in the playlist (Z/END plays last)
`HOME`					| play the first media file in the playlist (Z/END plays last)
`B`						| [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar [B]righter/more prominent.
`C`						| show/Hide on-screen [C]ontrols (media info) and media file timestamp
`Ctrl-C`				| show/Hide all on-screen controls, media file timestamp and media metadata
`D` and `DEL`			| [D]elete current media file (after confirmation)
`Ctrl-D and Ctrl-DEL`	| [D]elete all files in the current media file's folder (after confirmation) - subfolders not affected
`E`						| [E]ars - Mute/Unmute sound
`F`						| show/cancel [F]ullScreen mode
`G`						| [G]reater window size. Can be held down for rapid increase.
`Ctrl-G`				| reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease.
`H`						| position the window [H]orizontally (and Vertically) in the center of the screen
`I`						| zoom [I]n. Can be held down for rapid zooming.
`J`						| ad[J]ust the window's aspect ratio to match the video's aspect ratio. Gets rid of borders.
`K`						| mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`L`						| re[L]oad the list of supported media files from the current folder
`M`						| [M]aximize / restore window
`N`						| mi[N]imize window to the Windows taskbar
`O`						| zoom [O]ut. Can be held down for rapid zooming.
`P` 					| pause the media file and play it instead with [P]otplayer, if installed
`Q`						| play the previous media file in the [Q]ueue/playlist
`R`						| [R]ename the current media file
`S`						| re[S]tart the current media file from the beginning, aka [S]tartover
`T`                     | Tabbing. See below.
`U`						| [U]nzoom, i.e. re-fit the video to the window
`V`						| maximize / restore [V]iew, same as [M]
`W`						| [W]atch the next video in the list (or play the next audio)
`Numpad ENTER`			| [W]atch the next video in the list (or play the next audio)
`X`						| e[X]it the application
`Y`						| 
`Z`						| play the last media file in the playlist (A/HOME plays the first)
`END`    				| play the last media file in the playlist (A/HOME plays the first)
`0`						| briefly show media caption (the filename and its position in the playlist)
`1`						| reset the playback speed to normal, i.e. [1]00%
`2`						| reset panning to normal
`3`						| reset brightness to normal
`4`						| reset rotation to normal/not rotated
`5`						| save/bookmark the current media file timestamp to an INI file
`6`						| retrieve a saved/bookmarked media file timestamp from an INI file and continue playback from that point
`7`						| delete any previously saved/bookmarked media file timestamp for the current media file
`8`                     | Decrease brightness. Can be held down for rapid decrease.
`9`						| Increase brightness. Can be held down for rapid increase.
### By Category
---------------
Control | Action
------- | ------
Brightness |
`8`                     | Decrease brightness. Can be held down for rapid decrease.
`9`						| Increase brightness. Can be held down for rapid increase.
`3`						| reset brightness to normal
File Control |
`=`						| copy media file name (without the extension) to clipboard. e.g. for saving the file after F12
`D` and `DEL`			| [D]elete current media file (after confirmation)
`K`						| mark this media file as [K]eep (renames with a leading underscore to bring to top of folder)
`R`						| [R]ename the current media file
Freeze Frame |
`RIGHT ARROW`			| pauses the video and steps forwards one frame. Can be held down for rapid continuous stepping.
`LEFT ARROW`			| pauses the video and steps backwards one frame. Can be held down for rapid continuous stepping.
Mouse |
`Left Click on the window and hold`	| the window can be dragged/repositioned
`Left double-click video` | toggle fullscreen mode
`Right double-click video` | toggle fullscreen mode
`Right single-click video` | Pause/resume playback
On-Screen Display |
`0`						| briefly show media caption (the filename and its position in the playlist)
`B`						| [B]lackout/restore progress [B]ar
`Ctrl-B`                | Make the progress [B]ar [B]righter/more prominent.
`C`						| show/Hide on-screen [C]ontrols (media info) and media file timestamp
`Ctrl-C`				| show/Hide all on-screen controls, media file timestamp and media metadata
Panning |
`Ctrl-Up Arrow`			| pan up. Can be held down for continuous panning.
`Ctrl-Down Arrow`		| pan down. Can be held down for continuous panning.
`Ctrl-Left Arrow`       | pan left. Can be held down for continuous panning.
`Ctrl-Right Arrow`      | pan right. Can be held down for continuous panning.
`2`						| reset panning to normal.
Playlist |
`A`						| play the first media file in the playlist (Z/END plays last)
`HOME`					| play the first media file in the playlist (Z/END plays last)
`Z`						| play the last media file in the playlist (A/HOME plays the first)
`END`    				| play the last media file in the playlist (A/HOME plays the first)
`W`						| [W]atch the next video in the list (or play the next audio)
`Numpad ENTER`			| [W]atch the next video in the list (or play the next audio)
`Q`						| play the previous media file in the [Q]ueue/playlist
`L`						| re[L]oad the list of supported media files from the current folder
Rotating |
`Pg Up`                 | rotate video left/anti-clockwise (you must have first rotated right at least once)
`[`                     | rotate video left/anti-clockwise (you must have first rotated right at least once)
`Pg Dn`                 | rotate video right/clockwise (you must do this at least once before you can rotate left)
`]`                     | rotate video right/clockwise (you must do this at least once before you can rotate left)
`4`						| reset rotation to normal/not rotated
Speed |
`Numpad +` 	  	        | increase playback speed. Can be held down for rapid increase.
`/`						| increase playback speed. Can be held down for rapid increase.
`Numpad -`		        | decrease playback speed. Can be held down for rapid decrease.
`\`						| decrease playback speed. Can be held down for rapid decrease.
`1`						| reset the playback speed to normal, i.e. [1]00%
Tabbing |
`T`						| [T]ab through the media file a 100th (default), 200th or 10th of its duration (use ALT (10th) and CAPS LOCK (200th) to modify). Can be held down for rapid tabbing.
`Ctrl-T`				| [T]ab back through the media file a 100th (default), 200th or 10th of its duration (use ALT (10th) and CAPS LOCK (200th) to modify). Can be held down for rapid tabbing.
`TAB`					| tab forwards 1 second. Can be tapped repeatedly or held down to do a "fast forward" through the media file
`Ctrl-TAB`				| tab backwards 1 second. Can be tapped repeatedly or held down to do a "fast reserve" through the media file
Volume |
`Up Arrow`				| increase the volume. Can be held down for rapid increase.
`Down Arrow`			| decrease the volume. Can be held down for rapid decrease.
`Vol+ media key`        | increase the volume. Can be held down for rapid increase.
`Vol- media key`        | decrease the volume. Can be held down for rapid decrease.
`Vol-mute media key`    | Mute/Unmute sound
`E`						| [E]ars - Mute/Unmute sound
Window Control |
`ESCape`				| exit Fullscreen mode, or exit the app if not in fullscreen mode
`F`						| show/cancel [F]ullScreen mode
`G`						| [G]reater window size. Can be held down for rapid increase.
`Ctrl-G`				| reduce, i.e. un[G]reater, the window size. Can be held down for rapid decrease.
`H`						| position the window [H]orizontally (and Vertically) in the center of the screen
`J`						| ad[J]ust the window's aspect ratio to match the video's aspect ratio. Gets rid of borders (usually done automatically)
`M`						| [M]aximize / restore window
`N`						| mi[N]imize window to the Windows taskbar
`V`						| maximize / restore [V]iew, same as [M]
Zoom |
`I`						| zoom [I]n. Can be held down for rapid zooming in.
`O`						| zoom [O]ut. Can be held down for rapid zooming out.
`U`						| [U]nzoom, i.e. re-fit the video to the window
Additional |
`F10`                   | open media file in PotPlayer (assumes default installation folder)
`F11`                   | open media file in LossLess Cut (assumes default installation folder)            
`F12`					| open media file in ShotCut editor (assumes default installation folder)

Media File Formats
------------------
``Minimalist Media Player`` uses MVP for its media playback and manipulation functions. MVP supports more media file formats than you can shake a stick at. As such, it's a fairly safe bet that you can associate all your media files with ``Minimalist Media Player`` and they will play. If you're unsure, just run the executable and drop a media file onto the window to check it plays before permanently associating the file extension with ``Minimalist Media Player`` using "Open with..." in Windows File Explorer or whichever alternative 3rd party file manager software you are (aka should be!) using instead, like Directory Opus :D

Dependencies
------------
1/ ``Minimalist Media Player`` requires libmvp-2.dll to be in the same folder as the executable. If you have an mvp.conf file in the same folder, mvp will take it into account when initializing, for example to specify your audio output preferences but as with all these things, some experimentation may be required.

2. Each release comes with a program called "rot.exe". When deleting a file or the contents of a folder from within ``Minimalist Media Player``, it is actually rot.exe that carries out the deletion. This allows the deletion to be done in a separate process and I find that doing this there's less chance of Windows complaining that the file is in use (by MPV). You can check rot.exe's many features by running it from the command line. Many of you will recognize it, I'm sure :D


Notes concerning the code - a warning for the overly sensitive :D
-------------------------
Rather than following the herd, I like to experiment with different ways to layout my code and make it more readable and more easily understood by those looking at it for the first time, and for myself when returning to it months or even years later. I also enjoy exploring different programming paradigms.

1. I use a Delphi IDE with a 180-character-wide code editor. It strikes me as daft that we seem not to make full use of our high-resolution widescreen monitors when editing code (Delphi still automatically wraps event handler procedure signatures at column 56, regardless of how I have defined the margins!), and we all seem loathe to exceed column 80, which dates back to Fortran coding sheets and punched cards!! :D 
So, firstly, I make full use of all 180 columns of my editor.

2. I don't like nested IF statements. I think they're ugly, difficult to follow and very prone to human error when modifying/extending them. Consequently, I don't use IF statements at all and always use a particular form of Delphi's CASE statement instead. I consider them much neater and more elegant-looking, significantly easier to follow and amend, and altogether far superior to their nested-IF equivalents. I have no doubt many will disagree.

3. I like to experiment with different ways of separating out the User Interface definition from the general program logic and how application-wide variables should be handled. My only criticism of Delphi, going all the way back to v1, is the way it (and all the many books) encourages all code to be contained within the form's unit and event handler procedures. It's taken us too long to finally break away from that paradigm, in my opinion. My view is that you should write so that an entirely new UI can be created and then just hooked up to the back-end program logic.

4. Despite falling in love with Delphi since the moment I unboxed v1 (and immediately reboxed VB v3!), I loathe and detest BEGIN...END with a passion {I definitely have Curly Brace Envy}. As such, I try to find ways to make my code look much neater, which includes not having every END on a separate line, as you will see :D

5. All methods that I write (as opposed to IDE-created event handler procedures, etc.) are defined as functions with a default boolean return value. This makes the code easier to expand on when method requirements change.

6. All form and component settings are done in-code so that everything is obvious to the reader. Almost nothing is set in the RAD Studio/Delphi Object Inspector.

I present all this merely as food for thought; I'm not expecting the industry to suddenly adopt my bizarre coding strategies. You may find some little inspiration to do things slightly differently, though, to how you've traditionally written code. And if not, as least you've been forewarned! 

Why did I write ``Minimalist Media Player``?
--------------------------------------------
1. A good question, considering how many other perfectly good Windows media players there are, including my absolute favorite, PotPlayer (VLC has been usurped!). This pet project grew out of a single requirement: I had 100s of small, mostly funny, video clips that I had accumulated over the years, downloaded from social media platforms. I needed a way to quickly go through my entire collection of clips and decide whether to keep or delete each clip. This was proving to be very laborious using a standard media player as most of them don't give you the ability to delete the video being played. I was having to play the clip, close it, delete it in Explorer, then return to the media player to continue with the next clip. With ``Minimalist Media Player`` I was able to go through my collection very quickly, skimming through each video, deciding whether to keep it or not, and then move on to the next clip. When looking through the code and seeing some of the operations that are possible, please bear in mind that this application started life allowing an entire collection of video clips to be very quickly examined and kept or deleted, without having to repeatedly leave and restart the app.

2. I like to immerse myself in a movie. I find the usual visual clutter that comes with most media players annoying. With a couple of keystrokes in ``Minimalist Media Player`` you can have nothing but the moving images. If, like me, you also have a black desktop with no icons on it, it's even better.

Pull Requests
-------------
If you find ``Minimalist Media Player`` useful and you have suggestions for improvements, I am willing to consider implementing them myself or via pull requests. I would be very grateful if you would follow my coding etiquette though and have CASE statements rather than IF statements; they're really easy to adopt when you get used to using them exclusively over IFs.


And finally, Esther...
------------------
I am gradually working through my many Delphi projects and making them suitable for sharing publicly.

I am eternally grateful to all the many developers who have inspired me to release my code as open source.
