#
# Global Makefile for building Alice and Seam under Windows
# 2004-2005 Andreas Rossberg
#
# Note: Building under Windows is a PITA: it is horribly slow and very fragile.
#
# Usage:
# - 'make setup' to checkout everything from CVS and create an appropriate
#   directory structure.
# - 'make update' to update from CVS.
# - 'make build' to build everything.
# - 'make clean' to remove everything built.
# - 'make cleanbuild' to build from scratch.
# After setup you must include seam-support in your path to be able to build:
#   PATH=$(PREFIX)/seam-support/install/bin:$PATH
# (See $PREFIX below.)
#
# The system consists of several parts:
# - seam: the virtual machine framework (uses autoconf)
# - gecode: the constraint library (uses autoconf)
# - alice-ll: the Alice language layer for Seam (uses autoconf)
# - alice-bootstrap: Alice compiler and libraries
# All parts can be built or cleaned individually, using 'make build-XXX' and
# 'make clean-XXX' targets. The build targets ought to be fool-safe, but can
# take a while, as they reconfigure everything. There are also
# 'make rebuild-XXX' targets, which avoid some of the work and are useful after
# local changes.
#
# After a build is complete, you can use Alice by setting $PATH and $ALICE_HOME
# properly:
#   PATH=<dir>/alice-distribution/bin:<dir>/alice-distribution/lib/seam:$PATH
#   ALICE_HOME=<dir>/alice-distribution/share/alice
# For Gtk to function properly you must also have its installation directory in
# path:
#   PATH=<gtk-dir>/bin:<gtk-dir>/lib:$PATH
#
# To build the documentation, the generated HTML pages must be prepared in the
# $DOC directory specified below. The actual generation is not done in this
# Makefile, since it requires PHP. Invoking 'make docs' builds a compiled
# help file (.chm) from it.
#
# To prepare the distribution, you must have a build and the documentation.
# Invoking 'make distro' will build special Windows .exe wrappers (to be able to
# use Alice without Cygwin) and copy everything to the InstallShield directory.
#
# Troubleshooting:
# - Check $PATH and $ALICE_HOME. In particular, $ALICE_HOME must use proper
#   Windows syntax, not Cygwin (e.g. for drives)!
# - After having performed 'make distro' it is no longer possible to invoke
#   Alice from a Cygwin shell or do a 'make build-alice-bootstrap', because
#   the shell scripts have been replaced by .exe files. Invoke 'make unbuild-win'
#   to enable it again.
# - Sometimes something just keeps failing for incomprehensible reasons. In that
#   case it's best to do a 'make cleanbuild'.
# - If your system becomes notably slower you might have some orphaned Alice
#   processes hanging around (e.g. because some Ctrl-C might have failed to kill
#   subprocesses properly, which is a frequent problem on Windows/Cygwin).
#   Kill them.
#

# Configure this properly

CVSROOT = :pserver:anoncvs:anoncvs@ps.uni-sb.de:/services/alice/CVS
GECODECVSROOT = rossberg@ps.uni-sb.de:/services/gecode/CVS
DOC = /cygdrive/z/root/home/ps/httpd/html/alice/manual

# From here on no change should be needed

PWD := $(shell pwd)
PREFIX = $(PWD)/alice-distribution

all:
	@echo To build distro, run:
	@echo make cleanbuild
	@echo make docs
	@echo make distro
	@echo Optionally, run \`make update\' first.
	@echo Run \`make setup\' for initial checkout.

setup:
	cvs -d $(CVSROOT) login
	cvs -d $(CVSROOT) get seam-support
	(cd seam-support && build.sh)
	mkdir seam
	mkdir seam/build
	(cd seam && cvs -d $(CVSROOT) get seam && mv seam sources)
	mkdir gecode
	mkdir gecode/build
	(cd gecode && cvs -d $(GECODECVSROOT) get gecode && mv gecode sources)
	mkdir alice
	mkdir alice/build
	(cd alice && cvs -d $(CVSROOT) get alice && mv alice sources)
	@echo Setup complete.
	@echo Include `pwd`/seam-support/install/bin into your PATH.

update:
	(cd seam-support && cvs -q -d $(CVSROOT) update -dP) && \
	(cd seam/sources && cvs -q -d $(CVSROOT) update -dP) && \
	(cd gecode/sources && cvs -q -d $(GECODECVSROOT) update -dP) && \
	(cd alice/sources && cvs -q -d $(CVSROOT) update -dP)

clean-distro:
	rm -rf alice-distribution
clean-seam:
	(cd seam/build && rm -rf *)
clean-gecode:
	(cd gecode/build && rm -rf *)
clean-alice-ll:
	(cd alice/build && rm -rf *)
clean-alice-bootstrap:
	(cd alice/sources && make distclean)

clean: clean-distro clean-seam clean-gecode clean-alice-ll clean-alice-bootstrap

build-seam-support:
	(cd seam-support && build.sh)

build-seam:
	(cd seam/sources && make -f Makefile.cvs) && \
	(cd seam/build && $(PWD)/conf-seam && make install)
rebuild-seam:
	(cd seam/build && make install)

build-gecode:
	(cd gecode/sources && make -f Makefile.cvs) && \
	(cd gecode/build && $(PWD)/conf-gecode && make install)
rebuild-gecode:
	(cd gecode/build && make install)

build-alice-ll:
	(cd alice/sources/vm-seam && make -f Makefile.cvs) && \
	(cd alice/build && $(PWD)/conf-alice && make install)
rebuild-alice-ll:
	(cd alice/build && make install)

build-alice-bootstrap:
	(cp alice/build/Makefile.bootstrap alice/sources/vm-seam) && \
	(cd alice/sources && \
	 make PREFIX="$(PREFIX)" \
	      TARGET=seam \
	      GECODEDIR=$(PWD)/gecode/install \
	      bootstrap-smlnj reinstall-seam)
rebuild-alice-bootstrap:
	(cd alice/sources && \
	 make PREFIX="$(PREFIX)" \
	      TARGET=seam \
	      GECODEDIR=$(PWD)/gecode/install \
	      reinstall-seam)

build: build-seam build-gecode build-alice-ll build-alice-bootstrap
	@echo Build complete.
	@echo Try running $(PREFIX)/bin/alice.
	@echo You probably have to set:
	@echo PATH=$(PREFIX)/bin:PATH
	@echo ALICE_HOME=$(PREFIX)/share/alice

cleanbuild: clean build

docs:
	rm -rf docs && \
	cp -r $(DOC) docs && \
	cp alice/sources/doc/manual/Alice.hh? docs && \
	(cd docs && /c/Programme/HTML\ Help\ Workshop/hhc Alice || true) && \
	echo Docs built.
docs-offline:
	cp alice/sources/doc/manual/Alice.hh? docs && \
	(cd docs && /c/Programme/HTML\ Help\ Workshop/hhc Alice || true)

build-win:
	(cd alice/sources/vm-seam/bin/windows && make all PREFIX=$(PREFIX) install)
unbuild-win:
	rm -f $(PREFIX)/bin/alice*.exe &&
	make rebuild-alice-ll

distro: build-win
	(rm -rf ../InstallShield/Files/Alice) && \
	(cp -r alice-distribution ../InstallShield/Files/Alice) && \
	(mkdir ../InstallShield/Files/Alice/doc) && \
	(cp docs/Alice.chm ../InstallShield/Files/Alice/doc/) && \
	echo Distro prepared. Run InstallShield/Scripts/Alice/Alice.ism.

run:
	ALICE_HOME="$(PREFIX)/share/alice" && \
	PATH="$(PREFIX)/bin:$(PATH)" && \
	alice

runraw:
	alicerun x-alice:/compiler/ToplevelMain
