###
# GTK+ 2.0 Interface for Alice - Makefile

# The generator creates full code for the following name spaces
COMPS = Gdk Gtk Canvas
COMPSCAP = GDK GTK CANVAS

# The generator creates only enum components for the following name spaces
ENUMCOMPS = Pango
ENUMCOMPSCAP = PANGO

# names of generated files
ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml) \
	$(ENUMCOMPSCAP:%=%_ENUMS-sig.aml) $(ENUMCOMPS:%=%Enums.aml)
UNSAFE = $(COMPSCAP:%=%_UNSAFE-sig.aml) $(COMPS:%=%Unsafe.aml)
NATIVECOMPS = $(COMPS:%=Native%) $(COMPS:%=NativeFields%)
NATIVES = $(NATIVECOMPS:%=%.asig) $(NATIVECOMPS:%=%.cc)
GENFILES = $(NATIVES) $(ENUMS) $(UNSAFE)
GENSIGS = $(COMPSCAP:%=%-sig.aml) 

# C input file
INPUTFILE = gtkclean.c
# temporary output directory
OUTDIR = output

# support file indicating last generator build
LASTGENERATE = lastgenerate

# GtkBuilder
BUILDER = GTK_BUILDER-sig GtkBuilder

# names of all files to compile
CORE = CORE-sig.aml Core.aml
MAINFILES = $(COMPS:%=%.aml) $(BUILDER:%=%.aml)
CPPSOURCES = $(NATIVECOMPS:%=%.cc) NativeCore.cc
ALICESOURCES = $(CORE) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

# names of object files
OBJECTS = $(CPPSOURCES:%.cc=%.o) $(CPPSOURCES:%.cc=%.dll) \
	  $(ALICESOURCES:%.aml=%.alc)

INSTALLTARGETS = $(CPPSOURCES:%.cc=%.dll) \
	  $(ALICESOURCES:%.aml=%.alc)

# platform specific settings
ifdef WINDOWS
GENERATOR = GtkBinding.x86-win32
CC_OPTS = -mno-cygwin -mms-bitfields
SML = cmd /c sml
else
GENERATOR = GtkBinding.x86-linux
CC_OPTS =
SML = sml
endif

ifndef PC_OPTS
PC_OPTS = gtk+-2.0 libgnomecanvas-2.0
endif

ALICEC = alicec 
ALICEC_OPTS = --no-warn-conventions
ALICETOOL = alicetool
ALICEDEP = alicedep
INSTALL = ./myinstall -v -m 644

INSTALLDIR = `alice-config --alicelibdir`/gtk

#################################################

.PHONY: all generate compile depend docs clean veryclean distclean install

all: compile


### SOURCE CODE GENERATION ######################

$(GENFILES): $(LASTGENERATE)

$(LASTGENERATE) generate: $(INPUTFILE) $(GENERATOR)
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	$(SML) $(OUTDIR) $(INPUTFILE) @SMLload=$(GENERATOR)
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)
	touch $(LASTGENERATE)

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

$(INPUTFILE):
	( cd prepare && $(MAKE) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk $< > $@


### COMPILATION #################################

compile: $(OBJECTS)

%.dll: %.o
	$(ALICETOOL) -v link -Wl,-S $< -o $@ \
	 $(CC_OPTS) `pkg-config --libs $(PC_OPTS)`

%.o: %.cc
	$(ALICETOOL) -v compile -c $< -o $@ \
	 $(CC_OPTS) `pkg-config --cflags $(PC_OPTS)`

%.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) $< -o $@


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
	rm -f $(GENERATOR)
	rm -f $(GENFILES)
	rm -f $(GENSIGS)
	rm -f $(OBJECTS)
	rm -f $(LASTGENERATE)
	(cd samples && $(MAKE) clean )

# distclean: clean also all subdirs (including CKIT), 
#            and remove Makefile.depend and generator input file
distclean: clean
	rm -f *~
	rm -f Makefile.depend
	find -type d | grep '/CM$$' | xargs rm -fr
	rm -f $(INPUTFILE)
	(cd prepare && $(MAKE) distclean )
	(cd samples && $(MAKE) distclean )
	(cd doc && $(MAKE) distclean )

### INSTALLATION ################################
#
install: all
	mkdir -p -m 775 $(INSTALLDIR) # ./myinstall -d $(INSTALLDIR)
	./myinstall $(INSTALLTARGETS) $(INSTALLDIR)
