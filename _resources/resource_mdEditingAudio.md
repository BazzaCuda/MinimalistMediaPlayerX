[Wiki Page: Video Editing](https://minimalistmediaplayer.com/video/editing)<br />
[Wiki Page: Video Editing Example](https://minimalistmediaplayer.com/video/editingexample)<br />
[Wiki Page: 4-Key Edits](https://minimalistmediaplayer.com/video/fourkeyedits)<br />
[Help Page: Main Editing Topic](Editing)<br />
[Help Page: Keyframes](Keyframes)<br />
[Help Page: Editing Chapters](Editing_Chapters)<br />
[Help Page: Editing Troubleshooting](Editing_Troubeshooting)<br />
<br />
[Config Dialog: Keyframes](ConfigDialog:Keyframes)<br />
<br />

**Editing Audio**

**MMP** provides excellent audio and video editing facilities<br />
- for the user, editing is really fast and non-fiddly
- you can edit and export the finished product without even stopping playback
- editing is "lossless" and doesn't require that the exported audio/video be re-encoded
- in effect, you are doing a copy/paste from your original file to the edited file
- consequently, the quality of the exported file is identical to the original
<br />

FFmpeg's production of audio files is a little more involved than its handling of video files
- there are certain aspects of the audio and container formats, both of the input and the output files, that need to be taken into consideration
- if an audio file contains a "cover art" image, this also adds another complication depending on the audio file container format, .mp3, .m4a, .mkv, etc
- FFmpeg requires different parameters to "attach" the image, depending on the container format
- and if you're converting Editor segments to chapters, _plus_ cover art, this adds further complications
- this is particularly true when trying to devise "generic" instructions to FFmpeg which will work for the majority of audio files
- nevertheless, **MMP** and FFmpeg combine to do a sterling job together
- where possible, **MMP** will create an audio container with the same file extension as the file being edited
- where this is not possible, **MMP** and FFmpeg will create an MKV container with a .mkv file extension
- when adding chapters, the output file will always be an MKV container

**Cover Art**


**Adding Cover Art**



**.flac Lossless Audio Files**

You can certainly edit and export excerpts of .flac audio files but you will likely find that the _reported_ duration of every exported segment and the final exported file is the same as the duration of the original file, even though the _actual_ duration isn't
- this is because FFmpeg will only rewrite the .flac header if you re-encode the audio stream
- **MMP** deliberately doesn't support re-encoding of audio and video files
- this is what makes editing and exporting of editing files so quick; it is basically a copy/paste operation
- the option to re-encode files might be considered if there is enough demand for it
- also, you can if you wish retrieve the FFmpeg command from the .log file and rerun it to include re-encoding
<br />
<br />

