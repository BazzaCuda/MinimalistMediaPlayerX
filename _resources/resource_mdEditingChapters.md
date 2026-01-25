[Wiki Page: Video Editing](https://minimalistmediaplayer.com/video/editing)<br />
[Wiki Page: Video Editing Example](https://minimalistmediaplayer.com/video/editingexample)<br />
[Wiki Page: 4-Key Edits](https://minimalistmediaplayer.com/video/fourkeyedits)<br />
[Help Page: Main Editing Topic](Editing)<br />
[Help Page: Keyframes](Keyframes)<br />
[Help Page: Editing Troubleshooting](Editing_Troubeshooting)<br />
<br />
[Config Dialog: Keyframes](ConfigDialog:Keyframes)<br />
<br />

**Editing Chapters**

When exporting your edited audio or video file from the **Audio & Video Timeline Editor** you can turn your audio/video segments into chapters
- when playing a media file with chapters you can skip backwards and forwards between chapters with **[F8]** and **[F9]**
- chapters can be given titles which will be displayed in the [Notification Area](Notification_Area) when you jump to them
- not all media container formats support chapters, so **MMP** will always create an MKV file
- if your audio file contains cover art, this will be extracted to a "cover.jpg" file at the start of the export
- it will then be re-attached at the end of the export when the chapter metadata is added to your edited file
- this is necessary to prevent FFmpeg from turning your cover art into a video stream when concatenating your segments
- the filename, "cover.jpg", is mandatory to ensure that it becomes the audio file's cover art
- the Ctrl-Shift-[**C**]leanup process will not delete any "cover.jpg" files in the current folder
- as this is the standard filename for audio cover art, it cannot assume that every "cover.jpg" file it finds was created by **MMP**

In the **Audio & Video Timeline Editor** you can turn on chapter display just while you're editing but not have the chapters written to the exported file
- this is dependent on three new settings in the _**MinimalistMediaPlayer.conf**_ file
    - chaptersShow=yes | no
    - chaptersAudioWrite=yes | no
    - chaptersVideoWrite=yes | no
<br />

- The default settings are
    - chaptersShow=yes
    - chaptersAudioWrite=no
    - chaptersVideoWrite=no
<br />
<br />

*
