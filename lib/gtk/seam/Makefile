
COMPS = Gtk Gdk
COMPSCAP = GTK GDK
NATIVES = $(COMPS:%=Native%)

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

ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = install -C -v -m 666

.PHONY: all generate compile depend docs clean veryclean distclean

all: compile

#################################################

$(GENFILES): $(LASTGENERATE)

$(LASTGENERATE) generate: gtkclean.c
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	echo 'CM.make();main "$(OUTDIR)";' | sml
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)
	touch $(LASTGENERATE)

gtkclean.c:
	(cd prepare ; $(MAKE) )

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk -v filename=includesig.awk $< > $@

#################################################

compile: $(NATIVES:%=%.dll) $(COMPS:%=%.stc)

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S `pkg-config --libs gtk+-2.0` $< -o $@

%.o: %.cc
	$(ALICETOOL) -v cc `pkg-config --cflags gtk+-2.0` -c $< -o $@

%.stc: %.aml
	$(ALICEC) $< -o $@

#################################################

depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	gcc -M -I ../../../vm-stockwerk `pkg-config --cflags gtk+-2.0` \
	    $(CPPSOURCES) > Makefile.depend
	echo  \
         'CM.make();depend("Makefile.depend","$$(LASTGENERATE)",CM.names());' \
         | sml
	gawk -f alicedepend.awk $(ALICESOURCES) >> Makefile.depend

include Makefile.depend

#################################################


docs:
	(cd doc; $(MAKE) )

#################################################


clean:
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
