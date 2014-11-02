Ham
===

**Ham** is a language agnostic 'make' replacement and a virtual build environment.

Its aim is to provide an easily replicable build and devlopement environment
that can be setup on any Windows, OSX & Linux machine in a few minutes.

Once the repository has been cloned **ham** will automatically fetch all the tools
needed to complete a build of various kind of software. No install required !

Currently supported languages/framework/targets:

* **Bash** 3.1+ on Windows, Linux, OSX
* **TeX** on Windows, Linux, OSX
* **XSLT** on Windows, Linux, OSX
* **Emacs** & **Emacs Muse** (for docs) on Windows, Linux, OSX
* **C/C++** on Windows, Linux, OSX, iOS, Android (x86 & ARM), Flash11 (via FlasCC) and HTML5 (via Emscripten)
* **Java** on Windows, Linux, OSX and Android
* **ObjectiveC** on OSX and iOS
* **NodeJS** on Windows, Linux and OSX
* **Python 2.6 & 2.7** on Windows, Linux and OSX
* **Perl 5.6** on Windows, Linux and OSX
* **Git**, **Mercurial** and **Svn** on Windows, Linux and OSX

Ham is MIT licensed, see LICENSE.txt.

Jam/FT-Jam
==========

Ham (the make replacement part) is a derivative of "FT-Jam" which is itself a
derivative of the Jam build tool, based and 100% compatible with Jam 2.5.

- The FT-Jam homepage: http://www.freetype.org/jam/
- The original Jam homepage: http://www.perforce.com/jam/jam.html

Setup
=====

## Windows

No setup is required, just run ham-shell.cmd from the ham/bin directory and you'll be dropped in a Bash shell which has access to all the bash and ham commands. You can also use ham.cmd to run ham from third-party applications or from cmd.exe.

## OSX / Linux

Add this to your shell's .profile:

    export WORK="${HOME}/My Work"
    export HAM_HOME="$WORK/ham"
    export PATH="${PATH}:$HAM_HOME/bin"

Note that the previous snippet is for the bash shell, if you're using another shell you should modify accordingly.
