CVSROOT = :pserver:anoncvs:anoncvs@ps.uni-sb.de:/services/alice/CVS
GECODECVSROOT = rossberg@ps.uni-sb.de:/services/gecode/CVS
DOC = /cygdrive/z/root/home/ps/httpd/html/alice/manual-devel

PWD := $(shell pwd)
PREFIX = $(PWD)/alice-distribution

.PHONY: all build docs distro

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
	(cd seam/sources && cvs -q -d $(CVSROOT) update -dP) && \
	(cd gecode/sources && cvs -q -d $(GECODECVSROOT) update -dP) && \
	(cd alice/sources && cvs -q -d $(CVSROOT) update -dP)

clean-distro:
	rm -rf alice-distribution
clean-seam:
	(cd seam/build && rm -rf *)
clean-gecode:
	(cd seam/build && rm -rf *)
clean-alice-ll:
	(cd alice/build && rm -rf *)
clean-alice-bootstrap:
	(cd alice/sources && make distclean)

clean: clean-distro clean-seam clean-gecode clean-alice-ll clean-alice-bootstrap

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

build-win:
	(cd alice/sources/vm-seam/bin/windows && make all PREFIX=$(PREFIX) install)

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
