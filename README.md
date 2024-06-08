# Ham

**Ham** is a language agnostic 'make' replacement and a virtual build environment.

It aims to provide an easily replicable build and development environment
that can be setup automatically on any Windows, OSX & Linux machines.

Once the repository has been cloned **ham** will fetch the tools
needed to complete a build of various kind of software.

Ham is MIT licensed, see the LICENSE section.

# Getting Started

This will NOT use or mess with any of your OS's setup ; it is completely self contained so that you can experiment safely and more importantly so that builds are easily reproducible on other computers/VMs.

If you plan to use all the toolchains you should have at least 5GB of free space where you'll be putting ham. The core stuff is about 100MB.

If you don't have git download the latest version directly from:
https://github.com/prenaux/ham/archive/master.zip

Otherwise clone or pull the latest version of the ham repository:
```git clone https://github.com/prenaux/ham.git```

## Windows

No setup is required, just run ```ham-shell.cmd``` from the ```ham/bin``` directory and you'll be dropped in a Bash shell which has access to all the bash and ham commands. You can also use ```ham.cmd``` to run ham from third-party applications or from cmd.exe.

## OSX

Set bash as your default shell:
```
chsh -s /bin/bash
```

Add this to ~/.profile:
```bash
export WORK="$HOME/My Work"
export HAM_HOME="$WORK/ham"
export PATH="${PATH}:$HAM_HOME/bin"

export BASH_SILENCE_DEPRECATION_WARNING=1
export CLICOLOR=1
```

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

The default toolset will setup the OS's default C and C++ compilers, the Java JDK and the XSLT tools.

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

# Jam/FT-Jam

Ham (the make replacement part) is a derivative of "FT-Jam" which is itself a
derivative of the Jam build tool, based and 100% compatible with Jam 2.5.

- The FT-Jam homepage: http://www.freetype.org/jam/
- The original Jam homepage: http://www.perforce.com/jam/jam.html

http://www.perforce.com/jam/jam.html is a good resource to get familiar with the Jam language, however "Ham" has made quite a few change to default rules. See `ham/rules/base.ham` for an overview.

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

Copyright (c) 2012-2024 ham, see the AUTHORS section.

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
