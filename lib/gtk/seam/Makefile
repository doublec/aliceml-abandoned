
COMPS = Gtk Gdk
COMPSCAP = GTK GDK

NATIVES = $(COMPS:%=Native%)

UNSAFE = $(NATIVES:%=%.asig) $(NATIVES:%=%.cc)

CORE = $(COMPSCAP:%=%_CORE-sig.aml) $(COMPS:%=%Core.aml)
ENUMS = $(COMPSCAP:%=%_ENUMS-sig.aml) $(COMPS:%=%Enums.aml)
SAFE = $(ENUMS) $(CORE)
COMMON = GTK_UTILS-sig.aml GtkUtils.aml

MAINSIG = $(COMPSCAP:%=%-sig.aml)

GENFILES = $(UNSAFE) $(SAFE)
GENSIGS = $(MAINSIG)
COMPFILES = $(COMMON) $(SAFE) $(MAINSIG) $(COMPS:%=%.aml)
OUTDIR = output

ALICEC = alicec --dump-phases
ALICETOOL = alicetool
INSTALL = install -C -v -m 666



all: generate compile

#################################################

generate: rungenerator $(MAINSIG)

gtkclean.c:
	(cd prepare ; $(MAKE) )

rungenerator: gtkclean.c
	if test ! -d $(OUTDIR) ; then mkdir $(OUTDIR) ; fi
	echo 'CM.make();main "$(OUTDIR)";' | sml
	$(INSTALL) $(GENFILES:%=$(OUTDIR)/%) .
	rm -f $(OUTDIR)/*
	rmdir $(OUTDIR)

$(GENFILES): rungenerator ;

%-sig.aml: %-sig.tml %_CORE-sig.aml %_ENUMS-sig.aml
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
	rm -f *.stc
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
