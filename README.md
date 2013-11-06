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

Building
========

The following packages were required to build on Ubuntu 13.10:

    $ sudo apt-get install smlnj smlnj-runtime ml-lex ml-lpt ml-yacc     
    $ sudo apt-get install g++ libsqlite3-dev libgtk2.0-dev libgmp-dev \
         gawk libtool libc6-dev-i386 libgnomecanvas2-dev gtk2-engines-pixbuf \
         libxml2-dev autoconf texinfo

To build:

    $ git clone git://github.com/doublec/aliceml
    $ cd aliceml/make
    $ make setup
    #...add directory display to the PATH...
    $ export PATH=/path/displayed/in/setup:$PATH
    $ make all

The final system will be in the `distro` subdirectory and instructions are printed at the end of `make all` on how to add to the `PATH` and run.

Original Source
===============

This git repository was created by converting [Gareth Smith's mercurial repositories](https://bitbucket.org/gareth0) to git and splicing them into one repository. Instructions for building Gareth's original code is from [a post on the Alice ML mailing list](http://www.ps.uni-saarland.de/pipermail/alice-users/2012/001042.html) with minor bitrot fixes. Note that Gareth's repository contains many changes to the original [Alice ML CVS repository](http://www.ps.uni-saarland.de/alice/download.html) but is the only one I've found buildable on modern Linux systems.

During the `make setup` process the source to [gecode](http://www.gecode.org) will be downloaded. I've not decided whether this should be spliced into this git repository at this stage.
