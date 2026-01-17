[Config Dialog: File Deletion](ConfigDialog:File_Deletion)<br />

With the **MMP** media library management facilities, you get a lot of control over what media files are called and where they reside

**Renaming Files**

Control | Action
------- | ------
**[R]**                     | [R]ename the current media file
Ctrl-Shift-**[R]**          | [R]ename to a clean file name by replacing dirty characters with spaces - see _Clean File Name_ in the Config Dialog
**[F1]**                    | rename media file with category prefix 1 defined in _MinimalistMediaPlayer.conf_
**[F2]**                    | rename media file with category prefix 2 defined in _MinimalistMediaPlayer.conf_
**[F3]**                    | rename media file with category prefix 3 defined in _MinimalistMediaPlayer.conf_
**[F4]**                    | rename media file with the suffix defined in _MinimalistMediaPlayer.conf_

**Moving or Copying Files**

Control | Action
------- | ------
Main Media Window ||
Ctrl-**[M]**                | [M]ove the current media file to the folder specified in the _moved=_ user folder in .conf file
Shift-**[M]**               | [M]ove the current media file to the folder specified in the _moved=_ user folder in .conf file
Shift-**[S]**               | [S]ave the current media file to the folder specified in the _saved=_ user folder in .conf file
&nbsp; ||
Image & Thumbnail Browser ||
**[C]** | Save/Copy the image file to the _Copied=_ folder
**[S]** | Save/Move the image file to the _Saved=_ folder
Ctrl-**[S]** | Save the altered version of the image to a new image file
**[F1]-[F12]** | Save/Move to a user-defined folder - see _User-Defined Folders_ in the Config Dialog
Ctrl-**[U]** | undo Save/Move


**Deleting Files**

In order to delete a file or the contents of an entire folder
1. that functionality must be enabled either from the Ctrl-**[\\]** Config Dialog, or manually in the _MinimalistMediaPlayer.conf_ file
2. you must confirm, at least once, that you wish **MMP** to proceed

Control | Action
------- | ------
**[DEL]**                   | [D]elete current media file (after confirmation)
Ctrl-**[DEL]**              | [D]elete all files in the current media file's folder (after user confirmation)<br />subfolders are not affected<br />deletion functions can be disabled in _MinimalistMediaPlayer.conf_
Ctrl-Shift-**[C]**          | [C]leanup leftover audio and video editing files in the current playlist folder (.key, .log, .mmp, .seg, etc.)
Ctrl-**[K]**                | [K]eep/delete: delete all but the [K]ept files in a folder

**Miscellaneous**

Control | Action
------- | ------
| **[INSERT]**                | copy the media file name (without the extension) to the clipboard<br />useful for maybe saving a project file after opening the media file in an external app using [F10], [F11] or [F12]
| Ctrl-**[F]**                | open your [F]ile Explorer at the current [F]older |

