Minimalist Media Player
=====================

A simple but very effective media player built around the Windows Media Player ActiveX control.

``Minimalist Media Player`` provides both a minimalist/keyboard-operated user interface (so that on-screen controls don't detract from the video) and the controls with which to view the video in a minimalist window with the option to have no borders, window title bar/caption, progress bar, video timestamp, etc, and with the window resized to fit the video perfectly, eliminating ugly black borders/bands around the video, particularly above and below. This provides an immersive viewing experience even when played in window mode rather than full-screen.

### Keyboard Controls
---------------------
Although there are a lot of controls to remember, I have tried to use each alphabetic letter intuitively. The action description should help to associate the action with the letter.

Control | Action
------- | ------
`ALT-SPACEBAR`			| Activate the system menu to get to the About Box
`ESCape`				| exit Fullscreen mode, or exit the app if in window mode
`SPACEBAR` 				| pause/resume playback (also left mouse double-click on the video, or right mouse single-click) (all media windows, spacebar only)
`Ctrl-Up Arrow` 		| increase playback speed 10%
`/`						| increase playback speed 10%
`Ctrl-Down Arrow`		| decrease playback speed 10%
`\`						| decrease playback speed 10%
`F12`					| open media file in third party video editor (currently ShotCut)
`=`						| copy media file name to clipboard
`A`						| play the first media file in the playlist (Z plays last)
`B`						| [B]lackout/restore progress [B]ar
`C`						| show/Hide on-screen [C]ontrols and media file timestamp (all media windows)
`Ctrl-C`				| show/Hide all on-screen controls, media file timestamp and media metadata (all media windows)
`D` and `DEL`			| [D]elete current media file (after confirmation)
`Ctrl-D and Ctrl-DEL`	| [D]elete all files in the current media file's folder (after confirmation)
`E`						| [E]ars - Mute/Unmute sound
`F`						| show/cancel [F]ullScreen mode
`G`						| [G]reater window size
`Ctrl-G`				| reduce, i.e. un[G]reater, the window size
`H`						| position the window [H]orizontally (and Vertically) in the center of the screen
`I`						| zoom [I]n by 10% of the video's height and width
`J`						| ad[J]ust the window's aspect ratio to match the video's aspect ratio
`K`						| mark this media file as [K]eep
`L`						| re[L]oad the list of supported media files from the current folder
`M`						| [M]aximize / restore window
`N`						| mi[N]imize window to the Windows taskbar
`O`						| zoom [O]ut by 10% of the video's height and width
`P` 					| pause the media file and play it instead with [P]otplayer, if installed
`Q`						| play the previous media file in the [Q]ueue/playlist
`R`						| [R]ename the current media file
`S`						| re[S]tart the current media file from the beginning, aka [S]tartover (all media windows)
`T`						| [T]ab through the media file a 200th, 100th (default), 50th, 20th or 10th of its duration (use SHIFT-ALT, ALT, SHIFT, CAPS LOCK to modify) (all media windows)
`Ctrl-T`				| [T]ab back through the media file a 100th (default), 50th, 20th or 10th of its duration (use ALT, SHIFT, CAPS LOCK to modify) (all media windows)
`TAB`					| tab forwards 1/200th the duration of the media file (only the selected media window)
`Ctrl-TAB`				| tab backwards 1/200th the duration of the media file (only the selected media window)
`U`						| [U]nzoom, i.e. re-fit the video to the window
`V`						| maximize / restore [V]iew, same as [M]
`W`						| [W]atch the next video in the list (or play the next audio)
`X`						| e[X]it the application
`Y`						| tr[Y]out the media file by sampling it at various stages
`Z`						| play the last media file in the playlist (A plays the first)
`Up Arrow`				| increase the volume by 1%
`Down Arrow`			| decrease the volume by 1%
`Ctrl-Up Arrow`			| increase the playback speed by 10%
`Ctrl-Down Arrow`		| decrease the playback speed by 10%
`0`						| briefly show media caption (all media windows)
`1`						| reset the playback speed to normal, i.e. [1]00%
`2`						| resize the window so that 2 instances of the application can be placed side-by-side
`4`						| resize to a mini-window in the top-right corner of the screen
`Ctrl-4`				| move to top-right corner of the screen but maintain current window size
`5`						| save/bookmark the current media file timestamp to an INI file
`6`						| retrieve a saved/bookmarked media file timestamp from an INI file and continue playback from that point
`7`						| delete any previously saved/bookmarked media file timestamp
`8`						| set the video to 1-pixel larger than the window on all four sides
`9`						| resize the window to the width of the video
`RIGHT ARROW`			| step forwards one frame
`LEFT ARROW`			| step backwards one frame
Zoom|
`Ctrl-RIGHT ARROW`		| when zoomed in/out, move video RIGHT inside the window
`Ctrl-LEFT ARROW`		| when zoomed in/out, move video LEFT inside the window
`Ctrl-UP ARROW`			| when zoomed in/out, move video UP inside the window
`Ctrl-DOWN ARROW`		| when zoomed in/out, move video DOWN inside the window
Additional |
`Left Click on the window background and hold`	| the window can be dragged/repositioned without a window caption title bar
`Left double-click video` | pause / resume playback
`Right single-click video` | pause / resume playback
`SHIFT key (either)` | show or hide the Help window listing all the keyboard functions
`Ctrl-9` | auto-arrange up to 12 instances of ``Minimalist Media Player`` in a 4x3 grid (all media windows)
`Ctrl-0` | close all running instances of ``Minimalist Media Player`` (all media windows)

Media File Formats
------------------

Rather than simply taking Microsoft's word that the Windows Media Player ActiveX control supports their published list of media file formats, I have tested many file formats, and ``Minimalist Media Player`` explicitly supports the following formats and file extensions:

`.wmv` `.mp4` `.avi` `.flv` `.mpg` `.mpeg` `.mkv` `.3gp` `.mov` `.m4v` `.vob` `.ts` `.webm` `.divx` `.m4a` `.mp3` `.wav` `.aac` `.m2ts` `.flac` `.mts` `.rm` `.asf`

More formats and file extensions can be easily added when they're confirmed to work.
WMP has problems playing some FLV files which, bizarrely, go away if you rename the file to another format, e.g. MP4

WMPLib_TLB.pas
--------------
In order for you to be able to open the project and display the main form containing the Windows Media Player ActiveX component, from the menu in RAD Studio/Delphi you will first need to do "Component / Import Component... / Import ActiveX Control", and select the Windows Media Player from the list; RAD Studio will then generate this unit automatically and install the component to the ActiveX palette.

Notes concerning the code - a warning for the overly sensitive :D
-------------------------
Rather than following the herd, I like to experiment with different ways to layout my code and make it more readable and more easily understood by those looking at it for the first time, and for myself when returning to it months or even years later.

1. I use a Delphi IDE with a 180-character-wide code editor. It strikes me as daft that we seem not to make full use of our high-resolution widescreen monitors when editing code (Delphi still automatically wraps event handler procedure signatures at column 56, regardless of how I have defined the margins!), and we all seem loathe to exceed column 80, which dates back to Fortran coding sheets and punched cards!! :D 
So, firstly, I make full use of all 180 columns of my editor.

2. I don't like nested IF statements. I think they're ugly, difficult to follow and very prone to human error when modifying/extending them. Consequently, I don't use IF statements at all and always use CASE statements instead. I consider them much neater and more elegant-looking, significantly easier to follow and amend, and altogether far superior to their nested-IF equivalents. I have no doubt many will disagree.

3. I like to experiment with different ways of separating out the User Interface definition from the general program logic and how application-wide variables should be handled. For this project, all three are still located in main.pas, but on a much larger project with multiple units, the way I've separated them out could be adopted, placing them all in separate units. My only criticism of Delphi, going all the way back to v1, is the way it (and all the many books) encourages all code to be contained within the form's unit and event handler procedures. It's taken us too long to finally break away from that paradigm, in my opinion. My view is that you should write so that an entirely new UI can be created and then just hooked up to the back-end program logic.

4. Despite falling in love with Delphi since the moment I unboxed v1, I loathe and detest BEGIN...END with a passion {I definitely have Curly Brace Envy}. As such, I try to find ways to make my code look much neater, which includes not having every END on a separate line, as you will see :D

5. All methods that I write (as opposed to IDE-created event handler procedures, etc.) are defined as functions with a default boolean return value.

I present all this merely as food for thought; I'm not expecting the industry to suddenly adopt my bizarre coding strategies. You may find some little inspiration to do things slightly differently, though, to how you've traditionally written code. And if not, as least you've been forewarned! 

Why did I write ``Minimalist Media Player``?
--------------------------------------------
A good question, considering how many other perfectly good Windows media players there are, including my absolute favorite, PotPlayer (VLC has been usurped!). This pet project grew out of a single requirement: I had 100s of small, mostly funny, video clips that I had accumulated over the years, downloaded from social media platforms. I needed a way to quickly go through my entire collection of clips and decide whether to keep or delete each clip. This was proving to be very laborious using a standard media player as most of them don't give you the ability to delete the video being played. I was having to play the clip, close it, delete it in Explorer, then return to the media player to continue with the next clip. With ``Minimalist Media Player`` I was able to go through my collection very quickly, skimming through each video, deciding whether to keep it or not, and then move on to the next clip. When looking through the code and seeing some of the operations that are possible, please bear in mind that this application started life allowing an entire collection of video clips to be very quickly examined and kept or deleted, without having to repeatedly leave and restart the app.

Pull Requests
-------------
If you find ``Minimalist Media Player`` useful and you have suggestions for improvements, I am willing to consider implementing them myself or via pull requests. I would be very grateful if you would follow my coding etiquette though and have CASE statements rather than IF statements; they're really easy to adopt when you get used to using them exclusively over IFs.

MPlayer vs Microsoft Media Foundation vs Windows Media Player
--------------
Ultimately, I would like to replace Windows Media Player as the video renderer and make ``Minimalist Media Player`` a front-end for MPlayer with its superior video-handling capabilities, or possibly Microsoft Media Foundation / MfPack (I would appreciate some advice on which would be the most up-to-date approach). For instance, WMP really doesn't do Frame Backwards properly (it goes back 1 second not 1 frame!), and I also had to comment out the video scrubbing facility (dragging the mouse along the progress bar to quickly scan through a video - WMP gets in a right pickle!). If you would like to implement MPlayer or MMF/MfPack (whichever would provide the richest functionality going forward) to replace WMP, please be my guest!!

And finally, Esther...
------------------
This is my first ever attempt at releasing a source code project as Open Source. Please be gentle :P If this goes well, I have many more projects that I may be willing to release into the wild.

I am eternally grateful to all the many developers who have inspired me to start releasing my code.
