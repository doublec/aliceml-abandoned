###
# GTK+ 2.0 Interface for Alice - Makefile

# The generator creates full code for the following name spaces
COMPS = Gdk Gtk GnomeCanvas
COMPSCAP = GDK GTK GNOME_CANVAS

# The generator creates only enum components for the following name spaces
ENUMCOMPS = Pango
ENUMCOMPSCAP = PANGO

# The following native components will be compiled
# (in addition to the generated ones)
NATIVECOMPS = Core

# names of generated files
ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml) \
	$(ENUMCOMPSCAP:%=%_ENUMS-sig.aml) $(ENUMCOMPS:%=%Enums.aml)
UNSAFE = $(COMPSCAP:%=%_UNSAFE-sig.aml) $(COMPS:%=%Unsafe.aml)
NATIVES = $(COMPS:%=Native%) $(NATIVECOMPS:%=Native%)
GENNATIVES = $(COMPS:%=Native%.asig) $(COMPS:%=Native%.cc)
GENFILES = $(GENNATIVES) $(ENUMS) $(UNSAFE)
GENSIGS = $(COMPSCAP:%=%-sig.aml) 

# C input file
INPUTFILE = gtkclean.c
# temporary output directory
OUTDIR = output

# support file indicating last generator build
LASTGENERATE = lastgenerate

# names of all files to compile
CORE = CORE-sig.aml Core.aml
MAINFILES = $(COMPS:%=%.aml)
CPPSOURCES = $(NATIVES:%=%.cc)
ALICESOURCES = $(CORE) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

# names of object files
OBJECTS = $(CPPSOURCES:%.cc=%.o) $(CPPSOURCES:%.cc=%.dll) \
	  $(ALICESOURCES:%.aml=%.stc)

# platform specific settings
ifdef WINDOWS
GENERATOR = GtkBinding.x86-win32
CC_OPTS = -mno-cygwin -mms-bitfields
else
GENERATOR = GtkBinding.x86-linux
CC_OPTS =
endif

PACKAGES = gtk+-2.0 libgnomecanvas-2.0

SML = sml
ALICEC = alicec 
ALICEC_OPTS = --dump-phases # --no-warn-conventions
ALICETOOL = alicetool
ALICEDEP = gawk -f alicedepend.awk
#ALICEDEP = alicedep
INSTALL = ./myinstall -v -m 644

#################################################

.PHONY: all generate compile depend docs clean veryclean distclean

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
	echo 'CM.make(); compile();' | $(SML)
	test -e $(GENERATOR) || exit 1

$(INPUTFILE):
	( cd prepare && $(MAKE) WINDOWS=$(WINDOWS) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk $< > $@


### COMPILATION #################################

compile: $(OBJECTS)

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S $< -o $@ \
	 $(CC_OPTS) `pkg-config --libs $(PACKAGES)`

%.o: %.cc
	$(ALICETOOL) -v cc -c $< -o $@ \
	 $(CC_OPTS) `pkg-config --cflags $(PACKAGES)`

%.stc: %.aml
	$(ALICEC) $(ALICEC_OPTS) $< -o $@


### DEPENDENCIES ################################

depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	gcc -M -I ../../../vm-stockwerk `pkg-config --cflags $(PACKAGES)` \
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
