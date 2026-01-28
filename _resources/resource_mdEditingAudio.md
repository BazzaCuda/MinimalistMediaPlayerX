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
- you can edit and export the finished product without even stopping playback!
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

**Album MetaData**
- all metadata (Title, Album, Artist, etc) present in the audio file being edited will be copied to the exported audio file

**Cover Art**
- if your audio file contains Cover Art in the form of an image attachment, this will be copied to your exported audio file
- the image is extracted first to a "cover.jpg" file
- the individual exported segments will not contain the Cover Art image
- the Cover Art will be re-attached during the final export stage
- if the folder already contains a "cover.jpg" file, **MMP** will **_not_** overwrite it
- if the existing "cover.jpg" is not what you want, you should copy the audio file to a different folder and edit it there
- any "cover.jpg" file that exists in the folder during the final export stage will be attached to the exported audio file
- consequently, you can replace the audio file's current Cover Art image with a new one

**Adding Cover Art**
- if the audio file does _not_ contain Cover Art in the form of an image attachment, you can add it
- simply have any "cover.jpg" file in the same folder as your edited audio file
- the Cover Art image will be attached during the final stage of the export
- if you don't want add Cover Art, just ensure that you're editing the file in a folder that doesn't already contain a "cover.jpg" file

**.flac Lossless Audio Files**

You can certainly edit and export excerpts of .flac audio files but you will likely find that the _reported_ duration of every exported segment and the final exported file is the same as the duration of the original file, even though the _actual_ duration isn't
- this is because FFmpeg will only rewrite the .flac header if you re-encode the audio stream
- **MMP** deliberately doesn't support re-encoding of audio and video files
- this is what makes editing and exporting of editing files so quick; it is basically a copy/paste operation
- the option to re-encode files might be considered if there is enough demand for it
- also, you can if you wish retrieve the FFmpeg command from the .log file and rerun an amended version of it in a Console Window to include re-encoding
<br />
<br />

