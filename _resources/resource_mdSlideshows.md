Wiki Page: [Slideshows](https://minimalistmediaplayer.com/image/slideshow)<br />

Help Page: [Playlist Filter](Playlist)<br />
<br />
Config Dialog: [Playlist Filter](ConfigDialog:Playlist_Filter)<br />
<br />

Both the **Main Media Window** and the **Image & Thumbnail Browser** provide slideshows

- the slideshow in the **Image & Thumbnail Browser** is more specialized towards images and has more controls
- the slideshow in the main **MMP** is more generalized (and will include audio and video files with a mixed folder and a mixed playlist filter)
- both are useful depending on your requirements at the time
- both slideshows make use of a setting in the _minimalistmediaplayer.conf_ file: _slideshowIntervalMs=500_, followed by the number of milliseconds to display each image.
- if you set this duration, both slideshows will take this as the starting speed
- if the duration is not set, both slideshows will start at 2 seconds (2000 milliseconds) per image

**Main Media Window**

In the Main Media Window's slideshow, the following slideshow control is available:
| Control | Action |
| ------ | ------ |
| **[Spacebar]** | play/pause/resume slideshow |

- if the Main Media Window slideshow gets to the end of the playlist, **MMP** will exit at the end of the final item if it isn't an image, unless you have _Next Folder On End_ set
- if you have set infinite repeat with **Ctrl [R]**, this will still affect audio and video files, but not images
- playlist controls are still available, such as FIRST, NEXT, PREVIOUS, and LAST. The slideshow will continue
- if you turn Shuffle Mode on, the slideshow will play continuously
- if you are running a slideshow in the Main Media Window and you switch to the **Image & Thumbnail Browser** (Ctrl-**[I]**, **[T]** or **[Y]**) the slideshow will be paused automatically

**Image & Thumbnail Browser**

The following slideshow controls are available in the **Image & Thumbnail Browser**

| Control | Action |
| ------ | ------ |
| **[Spacebar]**      | pause/resume slideshow  |
| Ctrl-**[Spacebar]** | switch the slideshow direction and resume the slideshow |
| **[/]**             | speed Up by 100ms       |
| **[\\]**            | speed Dn by 100ms       |
| **[1]**             | reset speed to default set in [Config Dialog](ConfigDialog:Slideshows) |

- the slideshow direction is shown when you press **[Spacebar]** or Ctrl-**[Spacebar]**
- likewise, when you alter the speed, the new speed will be displayed in the bottom right corner of the window
- note that when you slow down the slideshow with **[\]**, the milliseconds that the image will be displayed will increase
- conversely, when you speed up the slideshow with **[/]**, the milliseconds delay will decrease
<br />
- if the slideshow gets to the end of the playlist, it will loop back to [and continue from] the first image
- if a reverse slideshow gets to the beginning of the playlist, it will loop to the end of the playlist and continue
- you can use the folder navigation controls to change the folder as the slideshow is playing
- there is currently no option to run either of the slideshows in fullscreen mode. This is planned for a future release of **MMP**





