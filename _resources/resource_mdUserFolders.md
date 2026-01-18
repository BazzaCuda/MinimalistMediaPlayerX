<br />
[Config Dialog: User-Defined Folders](ConfigDialog:User-Defined_Folders)<br />
<br />

**User-Defined Folders for Managing your Image Library**

The majority of this functionality is only available in the **Image & Thumbnail Browser**<br />
A small subset (save, move, keep) is available from the Main Media Window.<br />

**Image & Thumbnail Browser**

- in the **Image & Thumbnail Browser** all of the Function Keys are reserved for you to assign folders to
- up to 15 user folders can be set for copying and moving your files with one keystroke
- this allows you to go through your image collection and organize the images into folders based on category or subject matter, or whatever designations you require
- Each folder can be specified as either the fully-qualified path to a folder, e.g. C:\Pictures\...
- ... or it can be the name of a subfolder within the Base Folder, e.g. "Funny Memes"
- the Base Folder is always a fully-qualified path to a folder, e.g. "B:\Image Library\"
- you can mix-and-match these settings according to your requirements
- **MMP** will create the required folder for you the first time you use its corresponding key
- you can copy or move a file to any folder, anywhere on your computer.
<br />

**The 15 User-Defined Folders**

In the copy of the _MinimalistMediaPlayer.conf_ file that ships with **MMP** you can see the blank entries for the Base Folder and the 15 User-Defined Folders that are the subject of the Move, Save, Copy operations:<br />
<br />
baseFolder=<br />
copied=<br />
saved=<br />
moved=<br />
folder1=<br />
folder2=<br />
folder3=<br />
folder4=<br />
folder5=<br />
folder6=<br />
folder7=<br />
folder8=<br />
folder9=<br />
folder10=<br />
folder11=<br />
folder12=<br />
<br />
- you only need to define the Base Folder if you set any of the other folders as the name of a sub-folder
- if all the folders you use are fully-qualified paths then you don't need the Base Folder at all
- if you _do_ set the Base Folder then, strictly speaking, you're not required to set any of the others
- in that case, when you Move, Save or Copy a file, **MMP** will automatically create subfolders called 'moved', 'saved', 'copied', 'folder1', 'folder2', etc., within the Base Folder


**Main Media Window**

| Control | Action |
| ------- | ------ |
| **[K]** | Mark the current file as "[K]ept" - the file will be renamed with a leading "! "<br />This will bring the file to the top of the folder in your File Explorer software |
| Ctrl-**[K]** | Delete all except the "[K]ept" files in the current folder - requires user-confirmation
| Ctrl-**[M]** | [M]ove the file to the "moved=" user folder
| Shift-**[M]** | _same_ - alias for Ctrl-[M]
| Shift-**[S]** | [S]ave/move the file to the "saved=" user folder
| &nbsp; | There is no undo function in the **Main Media Window** |

**Image & Thumbnail Browser**

| Control | Action |
| ------- | ------ |
| **[K]** | Mark the current file as "[K]ept" - the file will be renamed with a leading "! "<br />This will bring the file to the top of the folder in your File Explorer software, separated from the other files |
| Ctrl-**[K]** | Delete all except the "[K]ept" files in the current folder - requires user-confirmation |
| **[C]** | copy the current image to the _copied=_ folder |
| **[S]** | save/move the current image to the _saved=_ folder |
| **[F1]** to **[F12]** | move the current image to the corresponding _folder1=_, _folder2=_ folder |
| Ctrl-**[U]** | [U]ndo any "Move" operation and move the image back to the folder you're working on<br />**MMP** maintains an unlimited undo list |
| Ctrl-**[Z]** | _same_ - alias for Ctrl-{U] |
