
COMPS = Gtk Gdk
COMPSCAP = GTK GDK
NATIVES = $(COMPS:%=Native%)

ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml)
UNSAFE = $(COMPSCAP:%=%_UNSAFE-sig.aml) $(COMPS:%=%Unsafe.aml)

GENNATIVES = $(NATIVES:%=%.asig) $(NATIVES:%=%.cc)
GENFILES = $(GENNATIVES) $(ENUMS) $(UNSAFE)
GENSIGS = $(COMPSCAP:%=%-sig.aml) 
OUTDIR = output

COMMON = GTK_TYPES-sig.aml GtkTypes.aml GTK_CORE-sig.aml GtkCore.aml
MAINFILES = $(COMPS:%=%.aml)

CPPSOURCES = $(NATIVES:%=%.cc)
ALICESOURCES = $(COMMON) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = install -C -v -m 666



all: generate depend compile

#################################################

generate: rungenerator $(GENSIGS)

gtkclean.c:
	(cd prepare ; $(MAKE) )

rungenerator $(GENFILES): gtkclean.c
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	echo 'CM.make();main "$(OUTDIR)";' | sml
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk -v filename=includesig.awk $< > $@

#################################################


compile: $(NATIVES:%=%.dll) $(COMPS:%=%.stc)

%.o: %.cc
	$(ALICETOOL) -v cc `pkg-config --cflags gtk+-2.0` -c $< -o $@

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S `pkg-config --libs gtk+-2.0` $< -o $@

%.stc: %.aml
	$(ALICEC) $< -o $@

#################################################

depend: Makefile.depend

Makefile.depend: Makefile $(CPPSOURCES) $(ALICESOURCES)
	gcc -M -I ../../../vm-stockwerk `pkg-config --cflags gtk+-2.0` \
	    $(CPPSOURCES) > Makefile.depend
#	echo 'CM.make();depend("Makefile.depend","$$(GENFILES)",CM.names());' \
#             | sml
	gawk -f alicedepend.awk -v outfile=Makefile.depend $(MAINFILES)

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

cmclean:
	for i in `find -type d | xargs echo`; do     \
            ( cd $$i >& /dev/null && rm -fr CM ) ;   \
        done

veryclean: clean cmclean
	rm -f gtkclean.c
	rm -f $(NATIVES:%=%.dll)
	rm -f $(NATIVES:%=%.dll.def)
	rm -f Makefile.depend
	(cd prepare ; $(MAKE) clean )
	(cd doc ; $(MAKE) clean )

distclean: veryclean
