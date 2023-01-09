# XMonad Config

This is my config file for my XMonad setup. It has one external dependency
(which I think is actually included as standard in the latest unstable
release). I include a local copy of that dependency in the repo so this should
be a totally vanilla install into `$HOME/.xmonad`.

## A Manifesto

XMonad is a tiling windows manager. If you are reading this, I likely don't
need to explain to you what that is, why you might use one, or the problems with the
"desktop metaphor". For the sake of the argument, though: suffice it to
say that forcing users to interact with their computer as if it is the actual
top of a desk is great in the early 90s when none of the general public know
what a computer is or what it can do, since it bridges that conceptual gap.
However, I know what a computer can do and how to interact with it. I don't
need to play-act at shuffling papers around on my desk (that is, manage
freeform application windows), and interacting with them is a total pain once
more than two or three are in play.

### Computers are not GUIs

Designers of operating systems have tried several ways of making this easier.
Tab-switching between windows is okay, again if there are only two or three of
them in play, but the history and how it changes with use becomes impossible to
keep track of very quickly. So modern operating systems often allow
snap-locking windows to different halves of the screen based on keyboard
shortcuts (so you're telling me you can look at two things at once without
having to physically drag them there? and they completely fill the space
without overlapping each other?? how revolutionary; how visionary!!), as well
as introducing the concept of "workspaces" -- extra desktops that you can
switch between, where different applications can be running on different
workspaces (so you're telling me I can clutter my headspace with windows on
multiple desktops at once? how revolutionary; how visionary!!)

The sarcasm might have tipped you off to the fact that I am not impressed with
these advancements. They either:
  * replicate a feature that already exists in tiling managers, but badly, or
  * do nothing to address the problem with having freeform windows, or
  * both.
The underlying issue --- that a graphical user interface for managing the
processes running on your device where every process you've run gets its own
viewwscreen that can exist anywhere within the canvas of the GUI is an
efficiency nightmare --- is left unchanged. 

#### The Truth

The truth is that the desktop metaphor is really holding you back. Click and
point is good for your grandmother, because WYSIWYG is a great paradigm for
non-technical people who don't have the time to learn their way around a CLI
and who don't really care about the internals or even the real capabilities of
their device. But for people like software engineers, for example, who just
(graphcally, at least) need access to a box containing the text of their
current project, and maybe a browser window on the other screen pointing to
stackoverflow, welll --- what do they care about the mouse pointer? Your
grandmother has fifteen different applications all minimised to the taskbar
including solitaire and for some reason three empty excel spreadsheets; you
just have emails to check and jira tickets to fill.

Most of your time spent in the desktop metaphor consists of navigating through
the GUI to the point where you don't have to deal with the damn GUI anymore. Am
I right? You get your IDE and your browser and alt-tab between them. Why bother
yourself with the GUI? Why not jump straight to the good part?

### Enter the [T I L E]

The solution is obvious (?). You need the window to see the text or whatever,
but you don't need it sliding around all over the place on top of other
windows.  Just remove that part, and include some logic for how to shuffle the
windows around if you open a new one. Now interacting with windows is a
keyboard shortcut action, not a mouse action. And that's a tiling windows
manager, and XMonad is one of those.

#### Workspaces, Headspaces, and Context Switching

I don't know about you but when I am working on a project I need at most three
or four things visible at once. I'm only human, so I can only type in or look
at one thing at a time, and if I have too many conccurrent background things my
workspace, my workflow, and my headspace get cluttered, and I slow down.

A "workspace" is basically a desktop without the background picture and the
clicky clicky bits. In my world, a workspace should have only the applications
running which are needed to complete the current task. That's maybe an editor
window, a terminal window, and a browser window. A workspace fills exactly one
monitor screen, and I have two screens, so I can view two workspaces at once.
Does that mean I work on two projects at once? No. It means I have two
workspaces available per project and hence twice the real estate for my windows
(I put internet stuff on one screen and text stuff on the other, for example).

#### The Master and Context Switching

Regardless of what the project actually is, there is almost always a "master"
window --- the one window that all the others are there in aid of. In a
software project, for example, that's likely the IDE or other box containing
the text of my code. If you find yourself with two "master" project windows,
you're probably trying to shove too much content into a single workspace. You
might find your workflow easier if you break out into more than one workspace.

So if I need to do something else "just quickly", I go to a new workpace and do
it there. If I need to switch projects entirely, I might close down the current
workspace and open up the new project where I am, but more likely I'll flick
over to a new set of workspaces and go from there. Whenever the thing I'm about
to do has a different conceptual "master" window, that's the indication that
there needs to be a new workspace!

## Controls

The controls are designed for keyboard and conceptual comfort. There are a few
patterns to how they work, and an effort has been made to be Vim-like wherever
that made sense.

### Action Patterns
 
  * Vim-like bindings for directional motions
  * If it makes sense, adding `Shift` to an action will bring windows with you
  * Don't try to do too much or be too clever
  * There should be fewer hard-coded efffects
  * There should be more generic, composable ones
  * Respect the already extant capabilities (e.g. CLI)
  * Try and keep the vocabulary of actions internally self-consistent.

### List

Herein `A`, `C`, `M`, `S` stand for `Alt`, `Control`, `Mod`, and `Shift`
respectively. `<Dir>` stands for the arrow keys or the Vim directions.

  * `M-<Dir>`: Move around the visible windows. Can cross screens.
  * `M-S-<Dir>`: Move the _current active window_ around, as above.
  * `M-w`: Switch focus to the other screen.
  * `M-S-w`: Swap the current workspace to the other screen.
  * `M-n`: Swap to a new blank workspace.
  * `M-S-n`: As above, but bring the current window along too.
  * `M-a`: Switch back to the previously active workspace.
  * `M-[0-9]`: Switch focus to the numbered workspace.
  * `M-S-[0-9]`: Send the active window to the numbered workspace.
  * `M-<Space>`: Open a top bar to search/run executables (dmenu).
  * `M-<Enter>`: Open a terminal window.
  * `M-\`: Open a new browser window.
  * `M-<Backspace>`: Kill the current window.
  * `M-S-<Backspace>`: Kill all windows on the current workspace.
  * `M-q`: Recompile xmonad.hs and restart (no processes affected).
  * `M-S-q`: Kill the current XMonad session (prompt to confirm).

There are more, but I rarely use them. Some relate to merging windows together
into sub-workspaces but this is almost always unnecessary for me (I like to
KISS, and my poor brain struggles to keep tabs --- pun intended ---  on them
all). Some relate to cycling through layout styles but again I don't really
care since I try to keep my workspaces context relevant and only have one or
two windows open at a time, so the layouts don't look very different to me.

## Summary

You don't really need me to summarise this, but I wanted to put an end note
anyway so that the document didn't just stop abruptly.
