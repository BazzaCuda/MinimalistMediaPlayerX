[Config Dialog: Playlist Filter](ConfigDialog:Playlist_Filter)<br />
[Config Dialog: Next Folder on End](ConfigDialog:Playlist)<br />
[Config Dialog: Next Folder on Empty](ConfigDialog:Playlist)<br />

When you launch MMP by opening a media file from your File Explorer
a playlist will be created containing that file and all the media files in the
same folder that match your Playlist Filter Format

- MMP will always play the media file you used to launch MMP
regardless of your playlist filter*
- if you [or MMP] move to another folder
the new playlist will only contain files that match your filter
- your selected playlist filter only applies to the Main Media Window
- the **Image & Thumbnail Browser** operates its own [hidden] playlist of the image files in each folder you navigate to
- you view this playlist by switching to the [**T**]humbnail view of the current folder

\* [Best Practice](Image_Browser) is to
- set the Playlist Filter to Audio, Video or Audio & Video
- set "Open Image in Browser" and "Exit Browser = Exit MMP"

Note:<br />
_With the Playlist Filter format set to Image, Ctrl-[P]laylist Filter on/off is now the same as [Spacebar] slideshow on/off_
<br />
<br />
_For audio and video formats, Ctrl-[P] in the **Main Media Window** is now redundant because the Main Media Window playlist will always match your Playlist Filter_

<br />
<br />

**When navigating to another folder, MMP will search for the next folder on the drive that contains media files that match your Playlist Filter**
- this is regardless of the direction in which you navigate, up or down, in your drive's folder structure
<br />
<br />


[Next Folder on End](ConfigDialog:Playlist) and [Next Folder on Empty](ConfigDialog:Playlist)
<br />
<br />


| Control | Action |
| ------- | ------ |
| **[&rarr;]** right arrow | play the next audio or show the next image in the playlist |
| **[&larr;]** left arrow |  play the previous audio or show the previous image in the playlist |
| &nbsp; ||
| **[W]**                     | [W]atch the next media file in the playlist |
| **[ENTER]** (inc. Numpad)   | [W]atch the next media file in the playlist<br />_unless the Playlist Panel is open, in which case play the highlighted item_ |
| **[Q]**                     | play the previous media file in the [Q]ueue/playlist |
| &nbsp; ||
| **[A]**                     | play the first media file in the playlist - [Z] / [END] plays last |
| **[HOME]**                  | play the first media file in the playlist - [Z] / [END] plays last |
| &nbsp; ||
| **[Z]**                     | play the last media file in the playlist - [A] / [HOME] plays the first |
| **[END]**                   | play the last media file in the playlist - [A] / [HOME] plays the first |
| &nbsp; ||

**Folder Navigation**
| Control | Action |
| ------- | ------ |
| Image & Thumbnail Browser ||
| **[&uarr;]** up arrow |  move up to the next folder that contains an image |
| **[&darr;]** down arrow |  move down to the next folder that contains an image |
| &nbsp; ||
| **[D]** | navigate [D]own to the next [D]irectory/folder on the drive that contains media files that match the Playlist Filter |
| Ctrl-**[D]** | navigate Up the previous [D]irectory/folder on the drive that contains media files that match the Playlist Filter |
| &nbsp; ||
| Image & Thumbnail Browser ||
| **[E]** | navigate to the previous [D]irectory/folder on the drive that contains media files that match the Playlist Filter<br />_In the Main Media Window [E]ars mutes/unmutes the audio stream_ |

**Additional**
| Control | Action |
| ------- | ------ |
| Shift-**[Home]**            | toggle shuffle mode on and off |
| **[P]**                     | show/hide the [P]laylist Panel _(Main Media Window only)_ |
| **[L]**                     | re[L]oad the playlist with all the supported media files from the current folder that match your Playlist Filter<br />Useful when you have knowingly changed the folder contents while **MMP** is running, or you've changed your Playlist Filter<br />Saves having to restart **MMP** to access the new folder or playlist contents |
| &nbsp; ||
| Main Media Window ||
| Ctrl-**[P]**                | toggle [P]laylist filtering on/off<br />_(for images only and is now synonymous with [spacebar] play/pause/resume slideshow)_ |

