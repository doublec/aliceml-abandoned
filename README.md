Overview
========

Alice ML is a functional programming language based on Standard ML, extended with rich support for concurrent, distributed, and constraint programming. Alice ML extends Standard ML with several new features:

* Futures: laziness and light-weight concurrency with implicit data-flow synchronisation
* Higher-order modules: higher-order functors and abstract signatures
* Packages: integrating static with dynamic typing and first class modules
* Pickling: higher-order type-safe, generic & platform-independent persistence
* Components: platform-independence and type-safe dynamic import & export of modules
* Distribution: type-safe cross-platform remote functions and network mobility
* Constraints: solving combinatorical problems using constraint propagation and programmable search 

More about Alice ML is available from the original project page: http://www.ps.uni-saarland.de/alice/

Building on Linux
=================

The following packages were required to build on Ubuntu 13.10:

    $ sudo apt-get install smlnj smlnj-runtime ml-lex ml-lpt ml-yacc     
    $ sudo apt-get install g++ libsqlite3-dev libgtk2.0-dev libgmp-dev \
         gawk libtool libc6-dev-i386 libgnomecanvas2-dev gtk2-engines-pixbuf \
         libxml2-dev autoconf texinfo

To build:

    $ git clone git://github.com/doublec/aliceml
    $ cd aliceml/make
    $ make setup
    #...add directory displayed to the PATH...
    $ export PATH=/path/displayed/in/setup:$PATH
    $ make all

The final system will be in the `distro` subdirectory and instructions are printed at the end of `make all` on how to add to the `PATH` and run.

Building on Mac OS X
====================

Support for building on Mac OS X is in the early stages and slightly more manual
than building on Linux due to errors I haven't worked out. Help for Mac OS X
users would be useful.

The following packages were used from [Homebrew](http://brew.sh):

    $ brew install smlnj gmp libgnomecanvas gtk+

To build:

    $ git clone git://github.com/doublec/aliceml
    $ cd aliceml/make
    $ export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig
    $ make setup
    #...add directory displayed to the PATH...
    $ export PATH=/path/displayed/in/setup:$PATH
    $ make all

On my machine the `make all` fails some time in the build when compiling `NativeLibs.cc` with this error:

    distro/bin/seamtool: line 63: echo: write error: Resource temporarily unavailable

The build can be continued with:

    $ PATH=/path/to/aliceml/distro/bin:$PATH make -C ../alice/lib/gtk/seam/
    $ make all

The final system will be in the `distro` subdirectory and instructions are printed at the end of `make all` on how to add to the `PATH` and run. Unfortunately the GUI tools won't work so `inspect` and other commands fail with:

    NativeCore.cc:744 error 'dlopen(libgtk-x11-2.0.so, 9): image not found'

This is due to Alice ML not using 'dylib' instead of 'so' for shared libraries from the looks of it. This should be relatively simple to fix but I haven't yet got to it.

Original Source
===============

This git repository was created by converting [Gareth Smith's mercurial repositories](https://bitbucket.org/gareth0) to git and splicing them into one repository. Instructions for building Gareth's original code is from [a post on the Alice ML mailing list](http://www.ps.uni-saarland.de/pipermail/alice-users/2012/001042.html) with minor bitrot fixes. Note that Gareth's repository contains many changes to the original [Alice ML CVS repository](http://www.ps.uni-saarland.de/alice/download.html) but is the only one I've found buildable on modern Linux systems.

During the `make setup` process the source to [gecode](http://www.gecode.org) will be downloaded. I've not decided whether this should be spliced into this git repository at this stage.
