
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

COMPFILES = $(COMMON) $(ENUMS) $(UNSAFE) $(GENSIGS) $(MAINFILES)

ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = install -C -v -m 666



all: generate compile

#################################################

generate: rungenerator $(GENSIGS)

gtkclean.c:
	(cd prepare ; $(MAKE) )

rungenerator: gtkclean.c
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	echo 'CM.make();main "$(OUTDIR)";' | sml
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)

$(GENFILES): rungenerator ;

%-sig.aml: %-sig.tml %_UNSAFE-sig.aml %_ENUMS-sig.aml
	gawk -f includesig.awk -v filename=includesig.awk < $< > $@

#################################################


compile: $(NATIVES:%=%.dll) $(COMPFILES:%.aml=%.stc)

%.o: %.cc
	$(ALICETOOL) -v cc `pkg-config --cflags gtk+-2.0` -c $< -o $@

%.dll: %.o
	$(ALICETOOL) -v ld -Wl,-S `pkg-config --libs gtk+-2.0` $< -o $@

%.stc: %.aml
	$(ALICEC) $< -o $@

#################################################


docs:
	(cd doc; $(MAKE) )

#################################################


clean:
	rm -f $(GENFILES)
	rm -f $(GENSIGS)
	rm -f $(COMPFILES:%.aml=%.stc)
	rm -f $(NATIVES:%=%.o)
	rm -f *~

cmclean:
	for i in `find -type d | xargs echo`; do     \
            ( cd $$i >& /dev/null && rm -fr CM ) ;   \
        done

veryclean: clean cmclean
	rm -f gtkclean.c
	rm -f $(NATIVES:%=%.dll.def)
	(cd prepare ; $(MAKE) clean )
	(cd doc ; $(MAKE) clean )

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)
