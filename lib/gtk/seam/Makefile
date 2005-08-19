###
# GTK+ 2.0 Interface for Alice - Makefile

VERSION = 1.2

# The generator creates full code for the following name spaces
COMPS = Gdk Gtk Canvas
COMPSCAP = GDK GTK CANVAS

# The generator creates only enum components for the following name spaces
ENUMCOMPS = Pango Misc
ENUMCOMPSCAP = PANGO MISC

# names of generated files
ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml) \
	$(ENUMCOMPSCAP:%=%_ENUMS-sig.aml) $(ENUMCOMPS:%=%Enums.aml)
UNSAFE = $(COMPSCAP:%=%_UNSAFE-sig.aml) $(COMPS:%=%Unsafe.aml)
NATIVECOMPS = $(COMPS:%=Native%) $(COMPS:%=NativeFields%)
NATIVES = $(NATIVECOMPS:%=%.asig) $(NATIVECOMPS:%=%.cc)
GENFILES = $(NATIVES) $(ENUMS) $(UNSAFE) Key.aml
GENSIGS = $(COMPSCAP:%=%-sig.aml) 

# C input file
INPUTFILE = gtkclean.c
# temporary output directory
OUTDIR = output

PREPAREFILES = $(INPUTFILE) PositiveList.sml

# support file indicating last generator build
LASTGENERATE = lastgenerate

# GtkBuilder
BUILDER = GTK_BUILDER-sig GtkBuilder

# names of all files to compile
CORE = CORE-sig.aml Core.aml Key.aml
MAINFILES = $(COMPS:%=%.aml) $(BUILDER:%=%.aml)
CPPSOURCES = $(NATIVECOMPS:%=%.cc) NativeCore.cc
ALICESOURCES = $(CORE) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

# names of object files
DLLOBJECTS = $(CPPSOURCES:%.cc=%.o) $(CPPSOURCES:%.cc=%.dll)
ALICEOBJECTS = $(ALICESOURCES:%.aml=%.alc)

INSTALLDLLTARGETS = $(CPPSOURCES:%.cc=%.dll)
INSTALLALICETARGETS = $(ALICESOURCES:%.aml=%.alc)

PLATFORM := $(shell ./platform.sh smlnj)
ifeq ("$(PLATFORM)", "")
  $(error Unknown platform)
endif

GENERATOR = GtkBinding.$(PLATFORM)
# platform specific settings
ifeq ("$(PLATFORM)", "x86-win32")
CC_OPTS = -mno-cygwin -mms-bitfields
SML = cmd /c sml
else
ifeq ("$(PLATFORM)", "ppc-darwin")
CC_OPTS = -D__ppc__
SML = sml
else
CC_OPTS =
SML = sml
endif
endif

ifndef PC_OPTS
PC_OPTS = gtk+-2.0 libgnomecanvas-2.0
endif

ALICEC = alicec 
ALICEC_OPTS = --no-warn-conventions --no-warn-unused-imports
ALICETOOL = alicetool
ALICEDEP = alicedep
INSTALL = ./myinstall -v -m 644

INSTALLDIR = `alice-config --alicelibdir`/gtk
ALICEDLL=`alice-config --alicedll`
DISTFILES = $(CPPSOURCES) Makefile debian platform.sh \
	MyNativeAuthoring.hh NativeUtils.hh NativeGdkSpecial.hh \
	NativeGnomeCanvasSpecial.hh NativeGtkSpecial.hh \
	ExtraMarshaller.hh myinstall
distdir = alice-gtk-$(VERSION)

#################################################

.PHONY: all generate compile depend docs clean veryclean distclean install

all: compile


### SOURCE CODE GENERATION ######################

#$(GENFILES): $(LASTGENERATE)
generate:	$(GENFILES)

$(GENFILES): $(INPUTFILE) $(GENERATOR) 
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	$(SML) $(OUTDIR) $(INPUTFILE) @SMLload=$(GENERATOR)
	./prepare-keyval.sh && mv Key.aml ${OUTDIR} || exit
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)

$(GENERATOR):
	rm -f $(GENERATOR)
	case `echo 'OS.Process.exit OS.Process.success' | $(SML) | grep "110\.0\.[0-9]"` in \
	  *Jersey*) \
	    echo "CM.make'" '"sources.cm"; GtkBinding.compile();' | $(SML) \
	    ;; \
          *) \
	    echo 'CM.make "sources.cm"; GtkBinding.compile();' | $(SML) \
	    ;; \
	esac
	test -e $(GENERATOR) || exit 1


$(PREPAREFILES):
	( cd prepare && $(MAKE) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk $< > $@


### COMPILATION #################################

compile: compiledll compilealice
compiledll: $(DLLOBJECTS)
compilealice: $(ALICEOBJECTS)

%.dll: %.o
	$(ALICETOOL) -v link $< -o $@ \
	 $(CC_OPTS) `pkg-config --libs $(PC_OPTS)`

%.o: %.cc
	$(ALICETOOL) -v compile -c $< -o $@ \
	 $(CC_OPTS) `pkg-config --cflags $(PC_OPTS)`

%.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) $< -o $@

ExtraMarshaller.hh:
	echo "VOID:INT,INT,STRING" | glib-genmarshal --header --body > $@

### DEPENDENCIES ################################

depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	$(ALICETOOL) makedepend `pkg-config --cflags $(PC_OPTS)` \
	    $(CPPSOURCES) > Makefile.depend
	echo 'use "smldepend.sml"; depend("Makefile.depend","$$(GENERATOR)");'\
                                                                   | $(SML)
	$(ALICEDEP) $(ALICESOURCES) >> Makefile.depend

-include Makefile.depend


### SAMPLE FILES ################################

sample-files:
	(cd samples && $(MAKE) depend && $(MAKE) )

### DOCUMENTATION ###############################

docs:
	(cd doc && $(MAKE) )


### CLEAN #######################################

# clean: clean this directory, and the samples
clean:
	rm -f $(DLLOBJECTS)
	rm -f $(ALICEOBJECTS)
	(cd samples && $(MAKE) clean )

# clean: also clean the generated sources
veryclean: clean
	rm -f $(GENERATOR)
	rm -f $(GENFILES)
	rm -f $(GENSIGS)
	rm -f $(LASTGENERATE)

# distclean: clean also all subdirs (including CKIT), 
#            and remove Makefile.depend and generator input file
distclean: veryclean
	rm -f *~
	rm -f Makefile.depend
	find -type d | grep '/CM$$' | xargs rm -fr
	rm -f $(PREPAREFILES)
	(cd prepare && $(MAKE) distclean )
	(cd samples && $(MAKE) distclean )
	(cd doc && $(MAKE) distclean )

### INSTALLATION ################################
#
install: all installdll installalice
installdll: compiledll
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLDLLTARGETS) $(INSTALLDIR)
installalice: compilealice
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLALICETARGETS) $(INSTALLDIR)

### DISTRIBUTION OF THE C SOURCES FOR PACKAGING
#
distdir:
	rm -rf $(distdir)
	mkdir $(distdir)
	cp -r $(DISTFILES) $(distdir)

dist:	distdir
	tar chzf alice-gtk-$(VERSION).tar.gz $(distdir)
	rm -rf $(distdir)
