###
# GTK+ 2.0 Binding for Alice - Makefile

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

# executable names
ifdef WINDOWS
GENERATOR = GtkBinding.x86-win32
EXTRAFLAGS = -mno-cygwin -mms-bitfields
else
GENERATOR = GtkBinding.x86-linux
EXTRAFLAGS =
endif

SML = sml
ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = ./myinstall -v -m 644
ALICEDEP = gawk -f alicedepend.awk
#ALICEDEP = alicedep
CLIBS = gtk+-2.0 libgnomecanvas-2.0

#################################################

.PHONY: all full generate compile depend docs clean veryclean distclean

all: compile

full: generate depend compile docs


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
	(cd prepare ; $(MAKE) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk $< > $@


### COMPILATION #################################

compile: $(NATIVES:%=%.dll) $(COMPS:%=%.stc)

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S $< -o $@ \
	 $(EXTRAFLAGS) `pkg-config --libs $(CLIBS)`

%.o: %.cc
	$(ALICETOOL) -v cc -c $< -o $@ \
	 $(EXTRAFLAGS) `pkg-config --cflags $(CLIBS)`

%.stc: %.aml
	$(ALICEC) $< -o $@


### DEPENDENCIES ################################

depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	gcc -M -I ../../../vm-stockwerk `pkg-config --cflags $(CLIBS)` \
	    $(CPPSOURCES) > Makefile.depend
	echo 'use "smldepend.sml"; depend("Makefile.depend","$$(GENERATOR)");'\
                                                                   | $(SML)
	$(ALICEDEP) $(ALICESOURCES) >> Makefile.depend

-include Makefile.depend


### DOCUMENTATION ###############################

docs:
	(cd doc; $(MAKE) )


### CLEAN #######################################

clean:
	rm -f $(GENERATOR)
	rm -f $(GENFILES)
	rm -f $(GENSIGS)
	rm -f *.stc
	rm -f *.o
	rm -f *~
	rm -f $(LASTGENERATE)

veryclean: clean
	rm -f $(INPUTFILE)
	rm -f $(NATIVES:%=%.dll)
	rm -f $(NATIVES:%=%.dll.def)
	rm -f Makefile.depend
	find -type d | grep '/CM$$' | xargs rm -fr
	(cd prepare ; $(MAKE) clean )
	(cd doc ; $(MAKE) clean )

distclean: veryclean
