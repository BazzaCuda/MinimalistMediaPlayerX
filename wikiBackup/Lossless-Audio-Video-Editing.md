Version 2 of **Minimalist Media Player** provides timeline editing features, along with media file stream-selection.

As with any audio or video editor, segments of the media file can be defined and either included or excluded from the output file.

And because it's "lossless", no re-encoding takes place. So it's quick. Very quick! Just a few seconds, regardless of how many segments you chop up your media file into, or how many streams you include from your original media file.

![editing1](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/705baa13-4b38-45ed-b2f7-235ae45c3f2d)

Video files, especially movies, often contain multiple streams with all the soundtracks in various languages, and numerous subtitle tracks, also in various languages. **Minimalist Media Player** lets you very quickly and easily select which streams will be included in the output file. Generally speaking, you will probabaly want to only include the video stream, the audio stream in your native language and the subtitles in your native language - you might also wish to remove a director's commentary track, along with all the foreign language subtitles. This is very easy. It simply requires you to click on a stream to include or exclude it.

![streams3](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/a9ea181e-bda5-4026-a91f-63be8b1bd4a6)

_You can scroll up and down these lists using either the scroll bars or the mouse wheel._

If fact, if all you want to do is remove particular streams from your file, you can open the timeline editing window (Ctrl-[E]), deselect the streams you don't want, and click "Export". An exact copy of your media file will be created, minus the streams you deselected. Your original media file is always left intact.

As some segments can be too small for their full details to be displayed on the timeline, the side panel duplicates the segment information with some additional information about segment duration.

Clicking the timecode display will switch it between displaying just seconds (to correspond to the start and end displays on each segment) to displaying the seconds as HH:MM:SS

Every change you make to the timeline is immediately saved to disk in a .mmp file named after the media file. If you switch to another media file and then return to a previous one, your most recent editing timeline will be loaded from the .mmp file. In switching to a new media file, however, you will wipe the list of changes for the Undo/Redo operations.

At any time, you can close the timeline editing facility (Ctrl-[E]), use the media player functions as you wish, and then reopen the timeline again with Ctrl-[E]. _(Of course, this is only strictly necessary if you want to use the timeline key functions that use the same keys as the main media window.)_ Your edits will immediately be restored when the timeline reopens. As before though, doing this will wipe the Undo/Redo list. 

**Keyboard and Mouse Controls**

All the keyboard controls of the main media display are available to control the media file as usual except for those keys used to control the timeline. These will override their corresponding normal key function.

The vertical cursor 
![cursor](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/e2855c98-3980-4f0a-81a8-9aab50a28ea0)
 shows the current position within the media file. This can be grabbed with the mouse and moved to reposition the audo/video file. Alternatively, the usual tabbing functions and playback functions, frame forwards and frame backwards etc. can be used to set the position of the media file. The progress bar can also be clicked or ctrl-mouse-moved or Numlock ON-mouse-moved to reposition the playback position and the cursor.

**Ctrl-[Z]** Undo the previous action.

**Ctrl-[Y]** Redo the previously undone action.

**[C]**: **C**ut the segment under the vertical cursor into two segments.

**Ctrl-[C]**: **C**ut the segment under the vertical cursor into two segments and automatically mark the lefthand segment as deleted. This is very useful for finding the section of the segment you want to include and discarding everything that precedes it.

**[I]**: create an **I**n  point. Currently, this can only be done when there is just one segment for the entire media file. Find the point at which you want your edited media file to start and press **[I]**.

**[O]**: create an **O**ut point. Currently, this can only be done when there is either just one or two segments for the entire media file. Find the point at which you want your edited media to end and press **O**. If you want both an In point and an Out point, you must set the In point first.

**_For the remaining operations you must first select a segment by clicking it._**

**[X]**: e**X**clude (i.e. delete) the segment from the output file. The trashcan icon will be displayed on all such segments to easily see which segments will be included or excluded from the output file. Note that eXcluded segments can still be **C**ut in two and operated on. In that case, the lefthand segment of the two will still be marked as deleted, while the righthand one will not. You may of course alter this if you wish.

_N.B. Because [X] usually eXits the app, if you have the timeline open, you can exit the app by either using the [ESC]ape key or by pressing Ctrl-[0], which closes all running instances of the app. Of course, you can also press Ctrl-[E] to close the timeline and then press [X] to close the app._

**[R]**: **R**estore an excluded/deleted segment.

**[M]**: **M**erge the segment with the segment to its right. Generally speaking, you don't want adjoining segments as they will cause repitition/overlap in your edited file (see the section below on Exporting). It's better to join consecutive segments into one.

**[N]**: **M**erge the segment with the segment to its left. (see the comment above)

**[S]**: **S**horten the selected segment by one second. This operation cannot be performed on the final segment - to shorten the final segment, lengthen the penultimate segment. You can hold down the key for rapid repetition.

**[L]**: **L**engthen the selected segment by one second. This operation cannot be performed on the final segment - to lengthen the final segment, shorten the penultimate segment. You can hold down the key for rapid repetition.

**Chapters**

If a media file contains chapter information (and no current .mmp file exists for the media file), segments will be automatically created from the start and end times of the chapters. Each segment will display its chapter's title. The titles are not currently saved to the .mmp file. A future version will make this chapter import optional and save the chapter titles to the .mmp file.

**Exporting to a new media file**

**Minimalist Media Player** uses FFMPEG to carry out the actual creation of your edited media file. FFMPEG will determine the start and end of each segment according to the location of keyframes in the original media file, so they are unlikely to correspond exactly to your cut points: an exported segment is likely to start slightly early. Consequently, if you have two adjacent segments with no intervening eXcluded segment, there is likely to be some overlap (duplicated frames and audio) in the resultant file. The only way to have no overlaps at all is to either export only one segment (which may be the entire media file, or one segment defined by In and Out points) or to have gaps in the timeline. If you need exact cutting of clips down to the frame level, you will need to use a standard high-end video editor. This is not the purpose of the timeline editing functions in **Minimalist Media Player** which is to provide quick-cut editing of the source file.

Unless you happen to make a cut exactly on a keyframe, FFMPEG will make the cut at the nearest keyframe _before_ your cut position. Generally speaking then, you can be confident that the start of a segment will be included in the resulting exported segment of the media file, and you should probably leave a few seconds (e.g. two?) at the end of a segment to be sure that it includes a keyframe that FFMPEG can make the cut at; that way the segment won't get cut short of what you want to include. As MMP doesn't currently delete the exported segments, you can check that each one includes everything you want. (_Feel free to ask any questions over at [Discussions](https://github.com/BazzaCuda/MinimalistMediaPlayerX/discussions) and I'll do my best to answer them._

Each segment in your timeline is exported to a separate segment file, with the individual segment files listed in a .seg file. All the segments are then joined into the resulting output file.

![exportseg](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/aa2e646b-f3e1-4a21-b547-15ab94605e4e)
![exportjoin](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/4d3b3ea1-66d3-4599-9926-250bd96f8eab)


Currently, each of these segment files will remain on disk for you to delete manually. If you repeat the export, they will be overwritten, as will the output file. A future release of **Minimalist Media Player** will give you the option to delete the segment files if the join is completed successfully. _There will never be the option to automatically delete the original media file being edited._

If the export of a particular segment fails...

![exportfail](https://github.com/BazzaCuda/MinimalistMediaPlayerX/assets/22550919/22bb88ef-45fe-4e4d-aa98-2592077998a7)

...you will be given the option to ignore the error and continue with the next segment. Alternatively, if you click "Rerun", the app will open a command line window and rerun the FFMPEG command so that you can view the FFMPEG errors directly.
**_If necessary, you can correct FFMPEG's creation of individual segments and then CTRL-click the Export button - this will run only the "Joining Segments" stage to recreate your finished output file._**

**DISCLAIMER**

**ALWAYS**, and I mean always! check that your output file matches the edits you wanted to make, before you delete the original file. You can always reopen the timeline and adjust your edits. If you are familiar with FFMPEG, you also have the option of examining the FFMPEG command lines recorded in the .log file, and then manually running the commands to adjust how FFMPEG operates on your media file.


 
_**Neither myself nor the FFMPEG developers will be held responsible if you delete your original file prematurely.**_

**Credits**

The timeline editing facility was inspired by the excellent [LosslessCut](https://github.com/mifi/lossless-cut). It is not my intention to implement all the features that LosslessCut provides. However, there are many standard video and audio formats that the internal [html5?] player in LosslessCut does not support, requiring LosslessCut to convert them to a supported format before editing can begin. Because **Minimalist Media Player** uses MPV to play media files, I have yet to find any audio or video format that it cannot play. Consequently, for the standard editing functions, **Minimalist Media Player** has distinct advantages over LosslessCut. Both use FFMPEG to do the actual editing.









