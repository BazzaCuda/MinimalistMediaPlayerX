Wiki Page: [Multi-View](https://minimalistmediaplayer.com/video/multiview)<br />

Help Page: [Aspect Ratio](Aspect_Ratio)<br />

You can view and control any number of **MMP** instances simultaneously

Your keyboard will need to have a **[Numlock]** key to do this<br />
_(you can also use Ctrl-**[N]** to toggle Numlock on and off)_<br />
<br />
With **Numlock** ON, there are several **_universal_** commands which will be sent to all **MMP** windows

Firstly, you can have multiple instances of **MMP** running anyway:
- you could have **MMP** playing music in the background while you manage your image library
- or you could have **MMP** playing through a playlist in the background while
    - you edit videos or
    - you go through your video collection and decide which video clips you want to keep<br /><br />
- in these and similar cases, you will want to keep **Numlock** OFF so that each **MMP** window is controlled independently of all the others
<br />
<br />

Secondly, you can play two or more videos side by side:
- maybe to compare two copies of the same movie to compare image quality and resolution to decide which you want to keep
- or maybe to check if two video clips are identical
- or, maybe you really do just want to watch all four John Wick movies at the same time!<br />
- in these cases, it can very handy to pause/resume all the videos, or to restart them all, or to have them all play from the same point, etc
<br />
<br />

It was specifically for reasons such as these that the **_universal_** controls were created

**Launching Multiple instances of MMP**

There are two ways to launch more than one instance of **MMP**.
1. select more than one video file in your File Explorer and press **[ENTER]**
    - this will launch all the videos simultaneously<br />
2. double-click a single video file in your File Explorer to launch it in **MMP**
    - repeat with other video files

Or a combination of the above: launch two, add a third and a fourth, etc

**Auto-Arrange MMP Windows**

You can auto-arrange up to four **MMP** windows using Ctrl-**[9]**<br />
_In the original incarnation of **MMP** is was 9 windows, hence the [9] key_

- **MMP** will arrange them in a 2 x 2 grid, properly sized to respect the [Aspect Ratio](Aspect_Ratio) of each video
- if you try to auto-arrange more than four **MMP** windows
    - **MMP** will display them in a single row across the desktop
    - you can then arrange and resize them as you want

**Controlling Multiple Videos - Universal Controls**

A number of the existing controls that you use to control **MMP** video playback are **_universal_**
  - universal controls can be sent to all running instances of **MMP**
  - having **[Numlock]** set ON or OFF determines whether a control affects one **MMP** window or all **MMP** windows
  - you can of course turn **[Numlock]** OFF temporarily to control one video in isolation, if required

With **NUMLOCK** ON, the following controls will be sent to all **MMP** windows

| Control | Action |
| ------ | ------ |
| **[Spacebar]** | pause / resume<br />_you can right-click a window to pause / resume an individual video_ |
| **[E]** | [E]ars: mute / unmute<br />_the volume of an individual window can still be controlled independently using the up **[&uarr;]** / down **[&darr;]** arrow keys or the mouse wheel_ |
| &nbsp; ||
| [Tabbing](Tabbing) ||
| **[Tab]** | Tab forwards 1 second. plus **[Shift]** = 2 seconds |
| Ctrl-**[Tab]** | Tab backwards 1 second. plus **[Shift]** = 2 seconds |
| &nbsp; ||
| **[T]** | Tab forwards 1/100th the duration of the video |
| Ctrl-**[T]** | Tab backwards 1/100th the duration of the video |
| &nbsp; ||
| Shift-**[T]** | Tab forwards 1/50th the duration of the video |
| Ctrl-Shift-**[T]** | Tab backwards 1/50th the duration of the video |
| &nbsp; ||
| CAPS LOCK ON-**[T]** | Tab forwards 1/200th the duration of the video |
| CAPS LOCK ON-Ctrl-**[T]** | Tab backwards 1/200th the duration of the video |
| &nbsp; ||
| [Captions](Captions) ||
| **[#]** | redisplay the playlist caption and the file's number in the playlist |
| **[C]** | show / hide the timestamp caption |
| Ctrl-**[C]** | toggle video metadata and timestamp caption show/hide |
| &nbsp; ||
| [Window Control](Window_Control) ||
| Ctrl-**[E]** | toggle the **Audio & Video Timeline Editor** opened or closed in each **MMP** window<br />_This is useful if you have [Start in Editor](ConfigDialog:General) set and forget when you launch multiple videos_ |
| **[G]** | [G]reater window size |
| Ctrl-**[G]** | un[G]reater (i.e. smaller) window size |
| &nbsp; ||
| [Playback](Playback) ||
| Ctrl-**[R]** | toggle infinite [R]epeat on and off |
| **[S]** | re[S]tart video / [S]tartover / re[S]et to the beginning  |
| **[`]** (back tick) | _same_ |
| **[V]** | sync all [V]ideos to the same timestamp<br />_e.g. click on one window's progress bar to set its timestamp position, then press **[V]** to sync all the other videos to the same timestamp_ |

The following commands are always sent to all **MMP** windows, regardless of whether **NUMLOCK** is ON or OFF
| Control | Action |
| ------ | ------ |
| Ctrl-**[0]** | close all **MMP** windows |
| Ctrl-**[9]** | auto-arrange all **MMP** windows on the desktop |

If your keyboard doesn't have a **[NUMLOCK]** key:
| Control | Action |
| ------ | ------ |
| Ctrl-**[N]** | toggle **[NUMLOCK]** ON and OFF |

Additional
| Control | Action |
| ------ | ------ |
| Mouse **right-click** | pause / resume playback of _just_ the clicked window |


