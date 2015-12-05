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

Ham is MIT licensed, see the LICENSE section.

# Getting Started

This will NOT use or mess with any of your OS's setup ; it is completely self contained so that you can experiment safely and more importantly so that builds are easily reproducible on other computers/VMs.

If you plan to use all the toolchains you should have at least 5GB of free space where you'll be putting ham. The core stuff is only about 10-20MB.

If you don't have git download the latest version directly from:
https://github.com/prenaux/ham/archive/master.zip

Otherwise clone or pull the latest version of the ham repository:
```git clone https://github.com/prenaux/ham.git```

## Windows

No setup is required, just run ```ham-shell.cmd``` from the ```ham/bin``` directory and you'll be dropped in a Bash shell which has access to all the bash and ham commands. You can also use ```ham.cmd``` to run ham from third-party applications or from cmd.exe.

## OSX

Add this to ~/.profile:
```bash
export WORK="$HOME/My Work"
export HAM_HOME="$WORK/ham"
export PATH="${PATH}:$HAM_HOME/bin"
```

So that programs started from the Dock (such as Xcode) have access to $WORK, add this to launchd.conf:
```bash
# Open launchd.conf
sudo vi /etc/launchd.conf
# Add this in launchd.conf
setenv WORK /Users/USERNAME/Work
# Reload the env vars
sudo egrep "^setenv\ " /etc/launchd.conf | sudo xargs -t -L 1 launchctl
```

Note that launchd doesnt seem to be applied on reboot anymore in OSX 10.10 (Yosemite), so... there's yet another way to do that in OSX (environment.plist), but since it'll probably break next time Apple update the OS I won't bother - also as an added bonus its a real pain to setup. So the simplest way is to run "sudo egrep "^setenv\ " /etc/launchd.conf | sudo xargs -t -L 1 launchctl" after the OS booted. Its manual, and in 2015 it sucks that this simple stuff doesn't work, but that how it is.

Run 'ham-install-os-packages' to install the required dependencies using homebrow, this only needs to be done once:
```bash
ham-install-os-packages
```

## Linux

Add this to ~/.bashrc:
```bash
export WORK="${HOME}/My Work"
export HAM_HOME="$WORK/ham"
export PATH="${PATH}:$HAM_HOME/bin"
```

Run 'ham-install-os-packages' to install the required dependencies using apt-get, this only needs to be done once:
```bash
ham-install-os-packages
```

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

#### Git
```
C-x g -> Git status
```

## Emscripten

Ham + Emscripten requires about 1GB of disk space.

- Import the Emscripten toolset in the shell with ```. ham-toolset emscripten```. This can take a while the first time as it will download all the dependencies needed to run Emscripten ; that is: emscripten, nodejs, java_jdk16 and python_27.

- If the download of the packages is interrupted just re-run ```. ham-toolset emscripten``` it will resume from where it left off.

- Once the toolset has been imported it will print the toolset's info, it should look like this:

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

AUTHORS
=======

The following authors have all licensed their contributions to ham
under the licensing terms detailed in the LICENSE section.

* Pierre Renaux <pierre@talansoft.com>

CONTRIBUTING
============

We actively welcome your pull requests.

To contribute submit a pull request and add your name & email to the AUTHORS
section of this file. We will not accept contributions if your name & email
isn't added to the AUTHORS section as it serves as proof of acknowledgement
that you have read and accepted the contribution terms described below.

1. The term "contribution" means any source code, object code, patch, tool,
   sample, graphic, specification, manual, documentation, or any other
   material posted or submitted by you to the project.
2. With respect to any worldwide copyrights, or copyright applications and
   registrations, in your contribution:
   - you assign to us joint ownership through your contribution, and to the
     extent that such assignment is or becomes invalid, ineffective or
     unenforceable, through your contribution you grant to us a perpetual,
     irrevocable, non-exclusive, worldwide, no-charge, royalty-free,
     unrestricted license to exercise all rights under those copyrights. This
     includes, at our option, the right to sublicense these same rights to
     third parties through multiple levels of sublicensees or other licensing
     arrangements;
   - you agree that each of us can do all things in relation to your
     contribution as if each of us were the sole owners, and if one of us
     makes a derivative work of your contribution, the one who makes the
     derivative work (or has it made) will be the sole owner of that
     derivative work;
   - you agree that you will not assert any moral rights in your contribution
     against us, our licensees or transferees;
   - you agree that we may register a copyright in your contribution and
     exercise all ownership rights associated with it; and
   - you agree that neither of us has any duty to consult with, obtain the
     consent of, pay, or give an accounting to the other for any use or
     distribution of your contribution.
3. With respect to any patents you own, or that you can license without
   payment to any third party, through your contribution you grant to us a
   perpetual, irrevocable, non-exclusive, worldwide, no-charge, royalty-free
   license to: make, have made, use, sell, offer to sell, import, and
   otherwise transfer your contribution in whole or in part, alone or in
   combination with or included in any product, work or materials arising out
   of the project to which your contribution was submitted, and at our option,
   to sublicense these same rights to third parties through multiple levels of
   sublicensees or other licensing arrangements.
4. Except as set out above, you keep all right, title, and interest in your
   contribution. The rights that you grant to us under these terms are
   effective on the date you first submitted a contribution to us.
5. With respect to your contribution, you represent that it is an original
   work and that you can legally grant the rights set out in these terms; it
   does not to the best of your knowledge violate any third party's
   copyrights, trademarks, patents, or other intellectual property rights; and
   you are authorized to accept this agreement on behalf of your company (if
   your contribution is on behalf of a company).

LICENSE
=======

## ham

The MIT License (MIT)

Copyright (c) 2012 ham, see the AUTHORS section.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

## FT-Jam and Jam 2.5

The original license from FT-Jam and Jam 2.5.

```
/*
 * /+\
 * +\	Copyright 1993-2002 Christopher Seiwald and Perforce Software, Inc.
 * \+/
 *
 * This file is part of jam.
 *
 * License is hereby granted to use this software and distribute it
 * freely, as long as this copyright notice is retained and modifications
 * are clearly marked.
 *
 * ALL WARRANTIES ARE HEREBY DISCLAIMED.
 */
 ```
