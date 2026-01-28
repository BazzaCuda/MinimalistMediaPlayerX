[Wiki Page: Video Editing](https://minimalistmediaplayer.com/video/editing)<br />
[Wiki Page: Video Editing Example](https://minimalistmediaplayer.com/video/editingexample)<br />
[Wiki Page: 4-Key Edits](https://minimalistmediaplayer.com/video/fourkeyedits)<br />
[Help Page: Main Editing Topic](Editing_Audio_&_Video)<br />
[Help Page: Editing Audio](Editing_Audio)<br />
[Help Page: Editing Chapters](Editing_Chapters)<br />
[Help Page: Keyframes](Keyframes)<br />
<br />
[Config Dialog: Keyframes](ConfigDialog:Keyframes)<br />
<br />

**Editing - Export TroubleShooting**

FFmpeg is a very robust application and is able to process practically any media file
- **MMP** takes advantage of this robustness by using FFmpeg in a manner which is more than likely to be successful for a majority of audio and video files
- nevertheless, occasionally, problems do arise
- if an Export of a segment fails, you can click "Rerun" to view the FFmpeg error messages
- a Windows Console will open so you can watch FFmpeg run and view its error messages
- the progress dialog will wait until you close the console window before proceeding with the export
- many problems that FFmpeg encounters are caused by the media file being created in a non-standard way by some third party software
- quite often, this type of problem can be resolved by simply having FFmpeg create its own copy of the media file
- the **Audio & Video Timeline Editor** in **MMP** provides you with a way to do this with one click of a button...

**Copying the Source File**

Letting FFmpeg make its own copy of a media file
- hold down a **[Ctrl]** key and hover your mouse over the Export button
- the Export button will change to **Copy**
- when you click the **Copy** button, a window will open so you can watch FFmpeg make a copy of the file
- if the copy is successful, the Editor will switch to editing the copy
- any existing .mmp file and .key file will also be copied so your edits will be automatically applied to the copy
- the Editor will reposition the cursor at the exact point you were at when you clicked **Copy**
- if the media file was playing, the copy will continue at the exact same point
- if you had the media file muted while you were editing, the copy will be muted too
- quite often with this category of FFmpeg problem, you can immediately click the Export button and your export will succeed
- occasionally, FFmpeg seems to have a problem creating .m4v files. For this reason **MMP** will always copy a .m4v file to a .mp4 file

**Joining Existing .Segnn. Files**

Occasionally, simply copying the source file does not resolve the problem and you will need to investigate why an individual segment didn't export or why FFmpeg wasn't able to Join/Concatenate the segments despite successfully exporting them
- if any stage of the export process fails, you can click **Rerun** to view the **FFmpeg** error messages
- a Windows Console will open so you can watch FFmpeg try to export the segment
- the progress dialog will wait until you close the console window before proceeding with the export
- if the problem is with an individual segment, you may be able to correct the problem and re-export a segment yourself
- the .log file can be very handy for seeing the **FFmpeg** commands that **MMP** ran to export each segment and then concatenate them
- if you are able to correct the .segnn. file for a segment you can instruct **MMP** to only perform the final **Join** stage
- in this case you can hold down a **[Ctrl]** key while hovering your mouse over the Export button
- the Export button will change to **Join**
- when you click the **Join** button, **MMP** will not re-export your segments and will leave your manual corrections as they are
- the export process will jump straight to the Join/Concatenate stage which joins all the files listed in the .seg file

**.flac Lossless Audio Files**
You can certainly edit and export excerpts of .flac audio files but you will likely find that the _reported_ duration of every exported segment and the final exported file is the same as the duration of the original file, even though the _actual_ duration isn't
- this is because FFmpeg will only rewrite the .flac header if you re-encode the audio stream
- **MMP** deliberately doesn't support re-encoding of audio and video files
- this is what makes editing and exporting of editing files so quick; it is basically a copy/paste operation
- the option to re-encode files might be considered if there is enough demand for it
<br />
<br />


