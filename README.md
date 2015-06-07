# Ham

**Ham** is a language agnostic 'make' replacement and a virtual build environment.

It aims to provide an easily replicable build and development environment
that can be setup automatically on any Windows, OSX & Linux machines.

Once the repository has been cloned **ham** will automatically fetch all the tools
needed to complete a build of various kind of software. No install required !

Currently supported languages/framework/targets:

* **Bash** 3.1+ on Windows, Linux, OSX
* **C/C++** on Windows, Linux, OSX, iOS, Android (x86 & ARM), iOS and HTML5 (via Emscripten)
* **ObjectiveC** on OSX and iOS
* **Java** on Windows, Linux, OSX and Android
* **NodeJS** on Windows, Linux and OSX
* **Python 2.6 & 2.7** on Windows, Linux and OSX
* **Perl 5.6** on Windows, Linux and OSX
* **Git**, **SVN** and **Mercurial** on Windows, Linux and OSX
* **XSLT 1&2** on Windows, Linux, OSX
* **Emacs** & **Emacs Muse** on Windows, Linux, OSX
* **TeX** on Windows, Linux, OSX
* **Doxygen** on Windows and OSX
* **ffmpeg** on Windows
* **winutils** and **windbg** on Windows

Ham is MIT licensed, see LICENSE.txt.

# Getting Started

This will NOT use or mess with any of your OS's setup ; it is completely self contained so that you can experiment safely and more importantly so that builds are easily reproducible on other computers/VMs.

If you plan to use all the toolchains you should have at least 5GB of free space where you'll be putting ham. The core stuff is only about 10-20MB.

If you don't have git download the latest version directly from:
https://github.com/prenaux/ham/archive/master.zip

Otherwise clone or pull the latest version of the ham repository:
```git clone https://github.com/prenaux/ham.git```

## Windows

No setup is required, just run ```ham-shell.cmd``` from the ```ham/bin``` directory and you'll be dropped in a Bash shell which has access to all the bash and ham commands. You can also use ```ham.cmd``` to run ham from third-party applications or from cmd.exe.

## OSX / Linux

Add this to your shell's .profile:

```bash
export WORK="${HOME}/My Work"
export HAM_HOME="$WORK/ham"
export PATH="${PATH}:$HAM_HOME/bin"
```

Note that the previous snippet is for the bash shell, if you're using another shell you should modify accordingly.

# Toolsets

A toolset contains a set of tools, its also commonly called a toolchain.

The following instructions all assume that you are in a 'ham shell' (see the Getting Started section).

The general command to import/setup a toolset is:
```
. ham-toolset TOOLSET1 ... TOOLSET2
```

Most of the time you will only use a single toolset since most will automatically import all their dependencies:
```
. ham-toolset TOOLSET
```

'hat' is a shorthand for 'ham-toolset':
```
. hat TOOLSET
```

When you import a toolset all the necessary binaries will be downloaded so in most case you won't need to install or download any third-party package to get them working.

## Default

The default toolset will setup the OS's default C and C++ compilers, the Java JDK 1.6 and the XSLT tools.

Type:
```
. hat default
```

## Repos (Git & SVN)

The repos toolset makes sure git and SVN are available. It will also make Mercurial (hg) accessible if it can be found on the system.

Type:
```
. hat repos
```

## Emacs

Ham includes an emacs toolset on all supported platform. The emacs toolsets contains
a good default environment that is suitable to edit all the languages supported by ham (and more).

On Windows the Emacs binaries are included in the toolset, on OSX & Linux you should install Emacs 23+.

### .emacs
Example ~/.emacs file:
```el
(setenv "WORK" "/Users/Pierre/MyWork")
(setenv "HAM_HOME" "/Users/Pierre/MyWork/ham")
(setenv "EMACS_DEVENV" (getenv "HAM_HOME"))
(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs"))
(add-to-list 'load-path (concat (getenv "HAM_HOME") "/sources/emacs/site"))

(require 'ni-user-pierre)
;; (require 'ni-theme-wombat)

;; (ni-turn-off-christmas)
```

Note that on Windows the emacs toolset is will the above .emacs if none exists.

### Keyboard

```
C-x 1 -> Only one window
C-x 2 -> Split window horizontally
C-x 3 -> Split window vertically
C-x 0 -> Close current window
C-1 -> Jump to next window
C-2 -> Jump to previous window

M-w   -> Add to yank ring (Copy)
C-w   -> Add to yank ring and kill region (Cut)
C-y   -> Yank (paste)
M-y   -> Cycle through other yank in ring (after C-y)
C-c y -> Yank (paste) menu

C-RET -> Open a ham-shell in another frame and run the previouos command in
         the shell's command history. This is meant to make it more convenient
         to run test cases and the likes quickly after modifying some code.

C-M-Up -> Jump to previous occurence of word at point.
C-M-Dn -> Jump to next occurence of word at point.

C-Up, C-{ -> Jump to previous paragraph.
C-Dn, C-} -> Jump to next paragraph.

S-Up -> Uncomment line and move up one line
S-Dn -> Comment line and moove down one line

M-Up -> Increment the number at point
M-Dn -> Decrement the number at point

C-x C-r  -> Revert buffer

C-m, RET -> New line and indent

C-v   -> Page down
C-S-v -> Page up

M-0 -> Erase/clear buffer

C-6 -> Toggle word wrap
M-6 -> Toggle whitespace mode

C-= (C +) -> Increase font size
C-- (C -) -> Decrease font size

C-c C-g, C-l -> goto line

C-z -> Undo

M-s -> Regex search forward
M-r -> Regex search backward

C-h C-h -> Search and replace regex in current buffer

M-/ -> Expand immediatly, press multiple time to cycle through possible expensions
C-/ -> Auto-complete popup

M-5 -> Go to matching parenthesis, *only* if you're on a paren character
C-. -> Go to matching parenthesis, matching to cloesest paran if cusor isnt on one

C-M-g   -> insert uuid1 to buffer (ex: AE09D861-3631-244F-809B-7E45E696BA98)
C-M-S-g -> insert uuid2 to buffer (ex: 0x5384343a,0xb2d9,0xd946,0x85,0xf2,0xe7,0xb9,0xbe,0x3d,0x4d,0x03)
M-S-g   -> insert uuid3 to buffer (ed: 39433690_B6F6_8E4C_836C_8BA30C8EA91D)

C-S-a   -> Begining of buffer
C-S-e   -> End of buffer

C->     -> Mark next occurence (Multimark/cursor)
C-<     -> Mark prev occurence (Multimark/cursor)
C-*     -> Mark all occurences
C-x r t -> Inline rectangle edit

C-M-insert, C-S-insert -> overwrite mode, insert is disabled by default

C-S-i   -> Indent the whole buffer
C-M-\   -> Indent selection
```

```
C-F5 -> Compile, specify compilation command
F5   -> Compile using the last compilation command specified
M-3, F6 -> Jump to previous match or compilation error
M-4, F7 -> Jump to next match or compilation error

C-F4 -> Start flymake on current buffer
C-F3 -> Restart flymake on current buffer
F3 -> Previous flymake error
F4 -> Next flymake error
```

#### Direx
Direx opens a dired buffer with expandable sub directories (a tree view).

```
C-x C-d -> Open Direx

o -> Open file in other window
e -> Print item infos (file size, access rights, write date, ...)
RET -> Open file in current window
TAB -> Toggle item (expand / close directory)
g -> Refresh tree
E -> Expand all sub directories
C -> Copy file
R -> Rename file
D -> Delete file
M -> chmod
G -> chgrp
O -> chown
T -> touch
```

#### Magit
Magit is a special mode to manage git repositories from within emacs.
See https://vimeo.com/2871241 & http://magit.vc.

```
C-x g -> Start magit, looks for .git folder for the file opened in the current buffer
```

For renamed files to be displayed as such make sure that the diff.renames option is set in gitconfig:
```
git config --global diff.renames true
```
or in ~/.gitconfig:
```
[diff]
  renames = true
```

## Emscripten

Ham + Emscripten requires about 1GB of disk space.

- Import the Emscripten toolset in the shell with ```. ham-toolset emscripten```. This can take a while the first time as it will download all the dependencies needed to run Emscripten ; that is: emscripten, nodejs, java_jdk16 and python_27.

- If the download of the packages is interrupted just re-run ```. ham-toolset emscripten``` it will resume where it left off.

- Once the toolset has been imported successfully it will print the toolset's info, it should look like this:

```
I/Imported toolset 'python_27'.
I/Imported toolset 'nodejs'.
I/Imported toolset 'java_jdk16'.
I/Imported toolset 'xslt_tools'.
I/Imported toolset 'emscripten'.
I/Toolset 'emscripten' setup successful.
=======================================================
=== Main Toolset ======================================
=======================================================
HAM_IMPORTED_TOOLSET = emscripten
HAM_TOOLSET = EMSCRIPTEN
HAM_TOOLSET_VER = 1_25
HAM_TOOLSET_NAME = emscripten
HAM_TOOLSET_DIR = /Users/USERNAME/My Work/ham/toolsets/emscripten
=======================================================
=== Tools Version =====================================
=======================================================
--- python_27 ------------------------
Python 2.7.6
--- nodejs ------------------------
v0.10.32
--- java_jdk16 ------------------------
--- java ---
java version "1.8.0_25"
Java(TM) SE Runtime Environment (build 1.8.0_25-b17)
Java HotSpot(TM) 64-Bit Server VM (build 25.25-b02, mixed mode)
--- javac ---
javac 1.8.0_25
--- xslt_tools_1 ----------------------
--- xsltproc ---
xsltproc was compiled against libxml 20900, libxslt 10128 and libexslt 817
--- saxon ------
Saxon-HE 9.3.0.5J from Saxonica
--- emscripten ------------------------
emcc (Emscripten GCC-like replacement) 1.25.0 (commit c40c7c218c3cfac5d832607733cf1cbc6ba47dab)
Copyright (C) 2014 the Emscripten authors (see AUTHORS.txt)
This is free and open source software under the MIT license.
There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

=======================================================
```
- Now you can use 'emcc' and all the Emscripten command line tools from the shell's command line.

### Test
```
. ham-toolset emscripten
cd "$HAM_HOME/sources/ham/tests/pi"
emcc pi.c -o pi.html
```
This should output pi.html in the current directory.

Open pi.html in your web browser, it should print in the console view:
```
PI version 1.0.0 (argc:1,argv:0x501020)
pi ~= 3.14159065, with 500000 iterations
```

# Jam/FT-Jam

Ham (the make replacement part) is a derivative of "FT-Jam" which is itself a
derivative of the Jam build tool, based and 100% compatible with Jam 2.5.

- The FT-Jam homepage: http://www.freetype.org/jam/
- The original Jam homepage: http://www.perforce.com/jam/jam.html

http://www.perforce.com/jam/jam.html is a good resource to get familiar with the Jam language, however "Ham" has made quite a few change to default rules. See ``ham/rules/base.ham``` for an overview.
