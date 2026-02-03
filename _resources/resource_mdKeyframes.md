Wiki Page: [Video Editing](https://minimalistmediaplayer.com/video/editing)<br />
Wiki Page: [Video Editing Example](https://minimalistmediaplayer.com/video/editingexample)<br />
Wiki Page: [4-Key Edits](https://minimalistmediaplayer.com/video/fourkeyedits)<br />

Help Page: [Main Editing Topic](Editing_Audio_&_Video)<br />
<br />
Config Dialog: [Keyframes](ConfigDialog:Keyframes)<br />
<br />

**MMP** provides excellent audio and video editing facilities<br />
- for the user, editing is really fast and non-fiddly
- you can edit and export the finished product without even stopping playback
- editing is "lossless" and doesn't require that the exported audio/video be re-encoded
- in effect, you are doing a copy/paste from your original file to the edited file
- consequently, the quality of the exported file is identical to the original
<br />
<br />

- press Ctrl-**[E]** to open the **Audio & Video Timeline Editor**
- for general editing purposes, the "rough cut" approach is often enough
- for **videos**, if you want to be more specific about where an included segment starts, you should use **Keyframes**

**Keyframes**

- to toggle keyframe processing on/off, use **[F]**
- keyframe processing is now so fast that it is viable to leave keyframe processing turned on by default in the [Config Dialog](ConfigDialog:Keyframes)
- with keyframes turned on, the vertical cursor will change color to indicate how accurate a cut will be if you start a segment at the cursor position
- the vertical cursor coincides with the currently viewed position of the video file
- a segment will always contain at least the video content you require, give or take a few frames - keyframes are not a factor in where segments end
- whether or not a segment might also include some content just prior to where you want the segment to start will be indicated by the vertical cursor:
<br />
<br />

- a white cursor  (rough cut) indicates that the exported segment will start at least 1.0 seconds prior to where your segment starts
- a yellow cursor (good cut)  indicates that the exported segment will start 0.5 seconds to 1.0 seconds prior to where your segment starts
- a purple cursor (best cut)  indicates that the exported segment will start 0.0 to 0.5 seconds prior to where you segment starts
<br />

| Control | Action |
| ------- | ------ |
| **[F]** | toggle key[F]rames on/off |


