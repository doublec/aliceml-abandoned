##
## Alice build and installation (dumb)
##

PWD := $(shell pwd)

GLOBAL_PREFIX = /opt/alice-devel
PREFIX = $(PWD)/install
DEBUG = 0

GECODEDIR = /opt/gecode

OPTS1= # '--dump-phases' # --dump-abstraction-result' # --dump-intermediate'
OPTS2= # '--dump-phases'
OPTS3= # '--dump-phases' # --dump-intermediate'

TARGET=<no-target-specified>
DEFAULT_TARGET=seam

ifeq ($(DEBUG), 0)
    CONFIGURE_OPTS =
else
    CONFIGURE_OPTS = --enable-checked
endif

PLATFORM = $(shell bootstrap/platform.sh smlnj)
ifeq ($(PLATFORM:%win32=win32), win32)
    WINDOWS = 1
else
    WINDOWS = 0
endif

TIME = /usr/bin/time
TIMEDIR = $(PWD)/time
TIMEO = $(shell ($(TIME) -o /dev/null ls >/dev/null && echo "yes" ))
ifeq ($(TIMEO),yes)
    TIMECOMMAND1 = $(TIME) -po $(TIMEDIR)1
    TIMECOMMAND2 =
    TIMECOMMAND3 = $(TIME) -po $(TIMEDIR)3
else
    TIMECOMMAND1 =
    TIMECOMMAND2 =
    TIMECOMMAND3 =
endif

export PREFIX TARGET WINDOWS GECODEDIR

.PHONY: clean clean-common clean-mozart clean-seam \
	install install-prelude install-common install-global install-mozart install-seam \
	bootstrap-smlnj bootstrap-mozart bootstrap-seam build-seam \
	libs-mozart libs-seam \
	doc man


##
## Do it!
##
install:
	make PREFIX=$(PREFIX) TARGET=$(DEFAULT_TARGET) DEBUG=$(DEBUG) install-rec
install-rec: install-$(TARGET)-rec man
	@echo -------------------------------------------------------------------------------
	@echo Installation of Alice for $(PLATFORM) complete.
	@echo Time for build 1:
	@cat $(TIMEDIR)1
	@echo Time for build 3:
	@cat $(TIMEDIR)3

install-common: install-prelude bootstrap-smlnj doc

install-prelude:
	@echo Installing Alice to $(PREFIX) for $(PLATFORM)...
	@echo -------------------------------------------------------------------------------

##
## Sync the global installation with local one
##
install-global:
	make TARGET=$(DEFAULT_TARGET) PREFIX=$(GLOBAL_PREFIX)
#	(cd $(PREFIX) && tar -cf - *) | \
#	(cd $(GLOBAL_PREFIX) && tar -xvf -)

##
## Build the bootstrap compiler on SML/NJ
##
bootstrap-smlnj:
	rm -f bootstrap/alicec-*.$(PLATFORM) #bootstrap/alicedep.$(PLATFORM)
	(cd bootstrap && make $(TARGET)) || exit 1

##
## Documentation
##
doc:
	(cd doc/manual && make PREFIX=$(PREFIX)/share/alice/doc TARGET=$(TARGET)) || exit 1

# this requires help2man, see http://www.gnu.org/software/help2man/
man:
	(cd doc/man && make PREFIX=$(PREFIX) all install) || exit 1

##
## Clean-up
##
clean: clean-$(DEFAULT_TARGET)

clean-mozart: clean-common
	rm -f bootstrap/alicec-mozart.$(PLATFORM) #bootstrap/alicedep.$(PLATFORM)
	(cd vm-mozart && make distclean) || exit 1
	(cd lib/constraints && make distclean) || exit 1
	(cd lib/distribution && make distclean) || exit 1
	(cd lib/test && make distclean) || exit 1
	(cd lib/gtk/mozart && ([ -f Makefile ] && make distclean || exit 0)) || exit 1
	(cd lib/tools/inspector/mozart && make distclean) || exit 1

clean-seam: clean-common
	rm -f bootstrap/alicec-seam.$(PLATFORM) #bootstrap/alicedep.$(PLATFORM)
	(cd vm-seam && make clean WINDOWS=$(WINDOWS)) || exit
	(cd vm-seam && make -f Makefile.bootstrap distclean) || exit 1
	(cd lib/distribution && make TARGET=seam distclean) || exit 1
	(cd lib/test && make distclean) || exit 1
	(cd lib/gtk/seam && make distclean) || exit 1
	(cd lib/tools/inspector/seam && make distclean) || exit 1

clean-common:
	(cd bootstrap && make clean) || exit 1
	rm -f time[1-3]

veryclean: clean-mozart clean-seam
	(cd bootstrap && make veryclean) || exit 1
	rm -rf */CM */*/CM */*/*/CM */.cm */*/.cm */*/*/.cm

distclean: veryclean
	rm -rf */NJ */*/NJ */*/*/NJ
	rm -rf install


################################################################################
## Mozart-specific stuff
################################################################################

##
## Install Alice on Mozart
##
install-mozart:
	make PREFIX=$(PREFIX) TARGET=mozart DEBUG=$(DEBUG) install-mozart-rec
install-mozart-rec: install-common bootstrap-mozart libs-mozart

bootstrap-mozart:
	unset ALICE_HOME ;\
	(cd vm-mozart && make depend) || exit 1 ;\
	(cd vm-mozart && $(TIMECOMMAND1) \
		make ALICEC_EXTRA_OPTS="$(OPTS1)" build1-install) || exit 1 ;\
	(cd vm-mozart && $(TIMECOMMAND2) \
		make ALICEC_EXTRA_OPTS="$(OPTS2)" build2-all) || exit 1 ;\
	(cd vm-mozart && $(TIMECOMMAND3) \
		make ALICEC_EXTRA_OPTS="$(OPTS3)" build3-install) || exit 1 ;\
	(cd vm-mozart && make PREFIX=$(PREFIX) install) || exit 1

libs-mozart:
	unset ALICE_HOME ;\
	export PATH="$(PREFIX)/bin:$(PATH)" ;\
	(cd lib/tools/inspector/mozart && make depend) || exit 1 ;\
	(cd lib/tools/inspector/mozart && make all PREFIX=$(PREFIX) install) || exit 1 ;\
	(cd lib/constraints && make depend) || exit 1 ;\
	(cd lib/constraints && make all PREFIX=$(PREFIX) install) || exit 1 ;\
	(cd lib/distribution && make depend) || exit 1 ;\
	(cd lib/distribution && make all PREFIX=$(PREFIX) install) || exit 1 ;\
	(cd lib/test && make SH_EXT=ozf depend && \
	 make SH_EXT=ozf PREFIX=$(PREFIX) all install) || exit 1 ;\
	(cd lib/gtk/mozart && autoconf && \
	 ./configure --with-gtk-canvas-dir=/opt/gtk-canvas) || exit 1 ;\
	(cd lib/gtk/mozart && make all PREFIX=$(PREFIX) install) || exit 1
	#(cd lib/gtk && make depend) || exit 1


################################################################################
## Seam-specific stuff
################################################################################

##
## Install Alice on Seam
##
install-seam:
	make PREFIX=$(PREFIX) TARGET=seam DEBUG=$(DEBUG) GECODEDIR=$(GECODEDIR) \
		install-seam-rec
install-seam-rec: install-common build-seam bootstrap-seam libs-seam

reinstall-seam:
	make PREFIX=$(PREFIX) TARGET=seam DEBUG=$(DEBUG) GECODEDIR=$(GECODEDIR) \
		reinstall-seam-rec
reinstall-seam-rec: bootstrap-seam libs-seam

bootstrap-seam:
	unset ALICE_HOME ;\
	export TIMEDIR ;\
	(cd vm-seam && make -f Makefile.bootstrap depend) || exit 1 ;\
	(cd vm-seam && $(TIMECOMMAND1) \
		make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$(OPTS1)" \
			build1-install) || exit 1 ;\
	(cd vm-seam && $(TIMECOMMAND2) \
	        make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$(OPTS2)" \
			build2-install) || exit 1 ;\
	(cd vm-seam && $(TIMECOMMAND3) \
		make -f Makefile.bootstrap ALICEC_EXTRA_OPTS="$(OPTS3)" \
			build3-install) || exit 1 ;\
	(cd vm-seam && make -f Makefile.bootstrap install) || exit 1

libs-seam:
	unset ALICE_HOME ;\
	export PATH="$(PREFIX)/bin:$(PATH)" ;\
	(cd lib/distribution && make TARGET=seam depend) || exit 1 ;\
	(cd lib/distribution && \
	 make TARGET=seam all PREFIX=$(PREFIX)/share/alice install) || exit 1 ;\
	(cd lib/test && make SH_EXT=alc depend && \
	 make SH_EXT=alc all PREFIX=$(PREFIX)/share/alice install) || exit 1 ;\
	(cd lib/gtk/seam && ./BUILD_ALL) || exit 1 ;\
	(cd lib/gtk/seam && make install) || exit 1 ;\
	(cd lib/tools/inspector/seam && make depend) || exit 1 ;\
	(cd lib/tools/inspector/seam && make all PREFIX=$(PREFIX) install) || exit 1 ;\
	(cd tools/yacc && make all) || exit 1 ;\
	(cd lib/gecode && make depend GECODEDIR=$(GECODEDIR)) || exit ;\
	(cd lib/gecode && make all install GECODEDIR=$(GECODEDIR)) || exit ;\
	(cd lib/postscript && make all install) || exit 1 ;\
	(cd lib/tools/explorer/seam && touch ExplorerGUI.aml ExplorerGUISignals.aml) || exit 1 ;\
	(cd lib/tools/explorer/seam && make depend) || exit 1 ;\
	(cd lib/tools/explorer/seam && make all install) || exit 1


##
## Build Seam
##
build-seam:
	(cd vm-seam && \
	 make -f Makefile.cvs && \
	 ./configure --prefix=$(PREFIX) $(CONFIGURE_OPTS) && \
	 make install) || exit 1
