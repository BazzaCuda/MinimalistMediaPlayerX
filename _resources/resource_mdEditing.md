[Wiki Page: Video Editing](https://minimalistmediaplayer.com/video/editing)<br />
[Wiki Page: Video Editing Example](https://minimalistmediaplayer.com/video/editingexample)<br />
[Wiki Page: 4-Key Edits](https://minimalistmediaplayer.com/video/fourkeyedits)<br />
[Help Page: Editing2](Editing2)<br />
[Help Page: Keyframes](Keyframes)<br />
<br />
Shift-[**\\**] Config Dialog: Keyframes<br />
<br />

**MMP** provides excellent audio and video editing facilities<br />
- for the user, editing is really fast and non-fiddly
- you can edit and export the finished product without even stopping playback
- editing is "lossless" and doesn't require that the exported audio/video be re-encoded
- in effect, you are doing a copy/paste from your original file to the edited file
- consequently, the quality of the exported file is identical to the original
<br />

- press Ctrl-**[E]** to open the **Audio & Video Timeline Editor**
- for general editing purposes, the "rough cut" approach is often enough
- for videos, if you want to be more specific about where an included segment starts, you should use Keyframes
- the vertical cursor coincides with the currently viewed position of the video file (or the audio file being listened to)
- a segment will always contain at least the audio/video content you require, give or take a few frames - keyframes are not a factor in where segments end
<br />
<br />

| Control | Action |
| ------- | ------ |
| Ctrl-**[E]**     | show/hide the **Audio & Video Timeline Editor** for fast, lossless editing
<br />
<br />

N.B. **_All other controls operate on the **Main Media Window** as normal_**
<br />

Control | Action
------- | ------
**Exit** ||
**[ESC]** ape                | exit dialog or **MMP**
Ctrl-**[0]** (zero)         | close all running instances of **MMP**
Ctrl-**[E]**                | show/hide the audio/video Timeline [E]diting tools for lossless editing
Shift-**[E]**               | play the [E]xported, [E]dited file in a new **MMP** window
**[X]**                     | see below
| &nbsp; ||
**Segment under the vertical cursor** ||
**[I]**                     | [I]n point: split the egment under the cursor and mark the left-hand segment as e[X]cluded, i.e. deleted
**[O]**                     | [O]ut point: split the segment under the cursor and mark the right-hand segment as e[X]cluded, i.e. deleted
**[C]**                     | [C]ut: split the segment under the cursor into two segments
| &nbsp; ||
**select a Segment, then...** ||
**[L]**                     | [L]engthen the selected segment by one second - can be held down for rapid repeat
**[S]**                     | [S]horten the selected segment by one second - can be held down for rapid repeat
**[M]**                     | [M]erge the selected segment with the segment to its right
**[N]**                     | [N]erge the selected segment with the segment to its left
**[X]**                     | e[X]clude the selected segment from the final edit
**[R]**                     | [R]estore the selected e[X]cluded segment
| &nbsp; ||
**Additional** ||
**right-click** a segment   | toggle include/exclude/restore for that segment (same as [X] and [R])
**[F]**                     | toggle key[F]rames on and off
**\`** (back tick)          | restart/startover from beginning of media file
Ctrl-**[R]**                | [R]ename file
Ctrl-**[S]**                | toggle [S]kip excluded segments during playback
**Ctrl**                    | change the Export button to Join after a manual adjustment to the .seg file and/or the .segnn exported segment files, to bypass the Export stage
**Ctrl-Shift**              | change the Export button to Copy after export errors, to work on an FFmpeg-copied duplicate of the original file

