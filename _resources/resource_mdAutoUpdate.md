
You can turn on _Auto Update_ either from the Shift-**[\\]** Config Dialog, or manually in the _MinimalistMediaPlayer.conf_ file

Setting | Effect
------  | ------
[autoUpdate=yes] | opening the About Box with Ctrl-**[A]** will check for updates |

- with _Auto Update_ turned on, when you then open the Ctrl-**[A]** About Box, **MMP** will check the version you are currently running against the latest version that has been released
- the ability to check online is dependent on two files being in your **MMP** folder: _libeay32.dll_ and _ssleay32.dll_
- without these two files, **MMP** is unable to make a secure connection to the internet
- if your **MMP** version is out of date, **MMP** will automatically download the latest .zip file and unpack _MinimalistMediaPlayer.exe_
- your currently-running version will be renamed and replaced by the newly-unpacked version
- when you restart **MMP** you will be running the latest version
<br />
<br />

- there are other files in the .zip file which are essential for running **MMP**
- however, _Auto Update_ will not overwrite your existing copies of these files so as not to potentially break your **MMP** installation by overwriting files which you have previously determined are more suitable for your particular Windows setup
- you should unpack these other files yourself if you decide to upgrade them
- when you do, you are advised to make backup copies of your existing files in case you decide to revert to them
<br />
<br />

- the release notes for each new version of **MMP** lists the versions of all the files in the latest .zip file
- for example, the remaining text is taken directly from the 2026/01/05 Release Notes for v5.4.0

**Asset files**<br />
The "...full.zip" release file below contains everything you need to run **MMP: Minimalist Media Player**<br />

Binary | Version | Release Date
------- | ------: | ------
| MinimalistMediaPlayer.exe | v5.4.0 | 2026/01/05 |
| libmpv-2.dll (`MPV v0.40.0`)  | v2.0.0 | 2025/12/15 |
| MediaInfo.dll | v25.10 | 2025/11/05 |
| ffmpeg.exe    | v8.0.1 | 2025/11/20 |
| ffprobe.exe   | v8.0.1 | 2025/11/20 |
| libeay32.dll  | v1.0.2.15 | 2024/02/17 |
| ssleay32.dll  | v1.0.2.15 | 2024/02/17 |

_N.B. Only MinimalistMediaPlayer.exe will be updated automatically. If the zip file contains newer versions of the other files, you should update and test these for yourself to ensure they will run on your Windows system. **MMP** will not make that assumption and overwrite your working versions. You are advised to make a copy of the older versions in case you need to revert to them._

Binary | Purpose
------ | ------
| libmpv-2.dll  | **MPV** as a library |
| MediaInfo.dll | extracts metadata from each media file (duration, dimensions, chapters, etc) |
| ffmpeg.exe    | used by the `Audio & Video Timeline Editor` to create the edited audio or video file |
| ffprobe.exe   | used by the `Audio & Video Timeline Editor` to extract keyframe information from a video file |
| libeay32.dll  | used to provide a secure connection to the internet |
| ssleay32.dll  | used to provide a secure connection to the internet |


