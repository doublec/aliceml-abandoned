
COMPS = Gdk Gtk GnomeCanvas
COMPSCAP = GDK GTK GNOME_CANVAS
NATIVES = $(COMPS:%=Native%)
LIBS = gtk+-2.0 libgnomecanvas-2.0

GENERATOR = GtkBinding.x86-linux
ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml)
UNSAFE = $(COMPSCAP:%=%_UNSAFE-sig.aml) $(COMPS:%=%Unsafe.aml)
GENNATIVES = $(NATIVES:%=%.asig) $(NATIVES:%=%.cc)
GENFILES = $(GENNATIVES) $(ENUMS) $(UNSAFE)
GENSIGS = $(COMPSCAP:%=%-sig.aml) 
OUTDIR = output
LASTGENERATE = lastgenerate

COMMON = GTK_TYPES-sig.aml GtkTypes.aml GTK_CORE-sig.aml GtkCore.aml
MAINFILES = $(COMPS:%=%.aml)

CPPSOURCES = $(NATIVES:%=%.cc)
ALICESOURCES = $(COMMON) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

SML = sml
ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = install -C -v -m 666

.PHONY: all generate compile depend docs clean veryclean distclean

all: compile

#################################################

$(GENFILES): $(LASTGENERATE)

$(LASTGENERATE) generate: gtkclean.c $(GENERATOR)
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	$(SML) @SMLload=$(GENERATOR) $(OUTDIR)
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)
	touch $(LASTGENERATE)

$(GENERATOR):
	echo 'CM.make(); compile();' | $(SML)

gtkclean.c:
	(cd prepare ; $(MAKE) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk -v filename=includesig.awk $< > $@

#################################################

compile: $(NATIVES:%=%.dll) $(COMPS:%=%.stc)

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S `pkg-config --libs $(LIBS)` $< -o $@

%.o: %.cc
	$(ALICETOOL) -v cc `pkg-config --cflags $(LIBS)` -c $< -o $@

%.stc: %.aml
	$(ALICEC) $< -o $@

#################################################

depend: Makefile.depend

Makefile.depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	gcc -M -I ../../../vm-stockwerk `pkg-config --cflags $(LIBS)` \
	    $(CPPSOURCES) > Makefile.depend
	echo 'use "smldepend.sml"; depend("Makefile.depend","$$(GENERATOR)");'\
                                                                   | $(SML)
	gawk -f alicedepend.awk $(ALICESOURCES) >> Makefile.depend
#	alicedep --stockwerk $(ALICESOURCES) >> Makefile.depend

include Makefile.depend

#################################################

docs:
	(cd doc; $(MAKE) )

#################################################

clean:
	rm -f $(GENERATOR)
	rm -f $(GENFILES)
	rm -f $(GENSIGS)
	rm -f *.stc
	rm -f *.o
	rm -f *~
	rm -f $(LASTGENERATE)

veryclean: clean
	rm -f gtkclean.c
	rm -f $(NATIVES:%=%.dll)
	rm -f $(NATIVES:%=%.dll.def)
	rm -f Makefile.depend
	find -type d | grep '/CM$' | xargs rm -fr
	(cd prepare ; $(MAKE) clean )
	(cd doc ; $(MAKE) clean )

distclean: veryclean
