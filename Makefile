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
#   PATH=<dir>/distro/bin:<dir>/distro/lib/seam:$PATH
#   ALICE_HOME=<dir>/distro/share/alice
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
DOC = /cygdrive/z/root/home/ps/httpd/html/alice/manual-devel

# From here on no change should be needed

PWD := $(shell pwd)
PREFIX = $(PWD)/distro

PKG_CONFIG_PATH := $(PWD)/seam-support/install/lib/pkgconfig:$(PWD)/gecode/install/lib/pkgconfig:$(PKG_CONFIG_PATH)
export PKG_CONFIG_PATH

########### Global ############

.PHONY:	info
info:
	@echo To build distro, \`make clean all\'.
	@echo To test, \`make rungui\'.
	@echo Optionally, \`make update\' first.
	@echo For initial checkout, \`make setup\'.

.PHONY:	setup
setup:
	cvs -d $(CVSROOT) login
	cvs -d $(CVSROOT) get seam-support
	(cd seam-support && build.sh)
	mkdir gecode
	mkdir gecode/build
	(cd gecode && cvs -d $(GECODECVSROOT) get gecode && mv gecode sources)
	mkdir seam
	mkdir seam/build
	(cd seam && cvs -d $(CVSROOT) get seam && mv seam sources)
	mkdir alice
	mkdir alice/build
	(cd alice && cvs -d $(CVSROOT) get alice && mv alice sources)
	@echo Setup complete.
	@echo Include `pwd`/seam-support/install/bin into your PATH.

.PHONY:	update
update:
	(cd seam-support && cvs -q -d $(CVSROOT) update -dP) && \
	(cd gecode/sources && cvs -q -d $(GECODECVSROOT) update -dP) && \
	(cd seam/sources && cvs -q -d $(CVSROOT) update -dP) && \
	(cd alice/sources && cvs -q -d $(CVSROOT) update -dP)

.PHONY:	clean
clean: clean-distro clean-gecode clean-seam clean-alice-ll clean-alice-bootstrap

.PHONY:	build
build: build-gecode build-seam build-alice-ll build-alice-bootstrap
	@echo Build complete.
	@echo Try running $(PREFIX)/bin/alice.
	@echo You probably have to set:
	@echo PATH=$(PREFIX)/bin:PATH
	@echo ALICE_HOME=$(PREFIX)/share/alice

.PHONY: all
all:	build docs distro

########### Support ############

.PHONY:	build-seam-support
build-seam-support:
	(cd seam-support && build.sh)

########### Gecode ############

.PHONY:	clean-gecode
clean-gecode:
	(cd gecode/build && rm -rf *)

.PHONY: setup-gecode
setup-gecode:
	(cd gecode/sources && autoconf)

.PHONY:	configure-gecode
configure-gecode:
	(cd gecode/build && \
	 ../sources/configure \
		CXX='g++ -mno-cygwin' \
		CC='gcc -mno-cygwin' \
		--enable-static \
		--disable-examples --disable-search --disable-minimodel \
		--prefix='$(PWD)/gecode/install')

.PHONY:	rebuild-gecode
rebuild-gecode:
	(cd gecode/build && make install)

.PHONY:	build-gecode
build-gecode: setup-gecode configure-gecode rebuild-gecode

########### Seam ############

.PHONY:	clean-seam
clean-seam:
	(cd seam/build && rm -rf *)

.PHONY: setup-seam
setup-seam:
	(cd seam/sources && make -f Makefile.cvs)

.PHONY:	configure-seam
configure-seam:
	(cd seam/build && \
	 ../sources/configure \
		CXX='g++ -mno-cygwin -DS_IXOTH=S_IXUSR -DS_IXGRP=S_IXUSR' \
		CXXFLAGS='-mcpu=pentium3 -march=pentium3' \
		CC='gcc -mno-cygwin -DS_IXOTH=S_IXUSR -DS_IXGRP=S_IXUSR' \
		CFLAGS='-mcpu=pentium3 -march=pentium3' \
		--prefix='$(PREFIX)' \
		--with-warnings=yes \
		--with-zlib='$(PWD)/seam-support/install')

.PHONY:	rebuild-seam
rebuild-seam:
	(cd seam/build && make install)

.PHONY:	build-seam
build-seam: setup-seam configure-seam rebuild-seam

########### Alice Language Layer ############

.PHONY:	clean-alice-ll
clean-alice-ll:
	(cd alice/build && rm -rf *)

.PHONY: setup-alice-ll
setup-alice-ll:
	(cd alice/sources/vm-seam && make -f Makefile.cvs)

.PHONY:	configure-alice-ll
configure-alice-ll:
	PATH="$(PREFIX)/bin:$(PATH)" && \
	(cd alice/build && \
	 ../sources/vm-seam/configure \
		--prefix='$(PREFIX)' \
		--with-warnings=yes \
		--with-gmp='$(PWD)/seam-support/install')

.PHONY:	rebuild-alice-ll
rebuild-alice-ll:
	PATH="$(PREFIX)/bin:$(PATH)" && \
	(cd alice/build && make install)

.PHONY:	build-alice-ll
build-alice-ll: setup-alice-ll configure-alice-ll rebuild-alice-ll

########### Alice Compiler & Library ############

.PHONY:	clean-alice-bootstrap
clean-alice-bootstrap:
	(cd alice/sources && make distclean)

.PHONY: setup-alice-bootstrap
setup-alice-bootstrap:
	(cp alice/build/Makefile.bootstrap alice/sources/vm-seam) && \
	PATH="$(PREFIX)/bin:$(PATH)" && \
	(cd alice/sources && \
	 make PREFIX="$(PREFIX)" \
	      TARGET=seam \
	      bootstrap-smlnj)

.PHONY:	rebuild-alice-bootstrap
rebuild-alice-bootstrap:
	PATH="$(PREFIX)/bin:$(PATH)" && \
	(cd alice/sources && \
	 make PREFIX="$(PREFIX)" \
	      TARGET=seam \
	      reinstall-seam)

.PHONY:	build-alice-bootstrap
build-alice-bootstrap: setup-alice-bootstrap rebuild-alice-bootstrap

########### Documentation ############

.PHONY: docs
docs:
	rm -rf docs && \
	cp -r $(DOC) docs && \
	cp alice/sources/doc/manual/Alice.hh? docs && \
	(cd docs && /c/Programme/HTML\ Help\ Workshop/hhc Alice || true) && \
	echo Docs built.

.PHONY:	docs-offline
docs-offline:
	cp alice/sources/doc/manual/Alice.hh? docs && \
	(cd docs && /c/Programme/HTML\ Help\ Workshop/hhc Alice || true)

########### Windows Binaries ############

.PHONY:	build-win
build-win:
	PATH="$(PREFIX)/bin:$(PATH)" && \
	(cd alice/sources/vm-seam/bin/windows && make all PREFIX=$(PREFIX) install)

.PHONY:	unbuild-win
unbuild-win:
	rm -f $(PREFIX)/bin/alice*.exe &&
	make rebuild-alice-ll

########### Distribution ############

.PHONY:	clean-distro
clean-distro:
	rm -rf distro

.PHONY:	build-xml-dll
build-xml-dll:
	cp $(PWD)/seam-support/install/bin/cygxml2-2.dll distro/bin

.PHONY: distro
distro: build-win build-xml-dll
	(rm -rf ../InstallShield/Files/Alice) && \
	(cp -r distro ../InstallShield/Files/Alice) && \
	(mkdir ../InstallShield/Files/Alice/doc) && \
	(cp docs/Alice.chm ../InstallShield/Files/Alice/doc/) && \
	echo Distro prepared. Run InstallShield/Scripts/Alice/Alice.ism.

########### Test Run ############

.PHONY:	run
run:
	@export ALICE_HOME && \
	ALICE_HOME=`cygpath -m "$(PREFIX)/share/alice"` && \
	PATH="$(PREFIX)/bin:$(PREFIX)/lib/seam:/c/Programme/GTK2-Runtime/bin:/c/Programme/GTK2-Runtime/lib:/c/Programme/GTK2-Runtime/lib/gtk-2.0/2.4.0/engines:$(PATH)" && \
	alice

.PHONY:	rungui
rungui:
	@export ALICE_HOME && \
	ALICE_HOME=`cygpath -m "$(PREFIX)/share/alice"` && \
	PATH="$(PREFIX)/bin:$(PREFIX)/lib/seam:/c/Programme/GTK2-Runtime/bin:/c/Programme/GTK2-Runtime/lib:/c/Programme/GTK2-Runtime/lib/gtk-2.0/2.4.0/engines:$(PATH)" && \
	alicewin

selectgui:
	PATH="/c/Programme/GTK2-Runtime/bin:/c/Programme/GTK2-Runtime/lib:$(PATH)" && \
	gtk2_prefs
