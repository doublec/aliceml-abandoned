
ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS =

SOURCES = Debug.aml Utils.aml \
          Common.aml \
          INTERFACE-sig.aml ENGINE-sig.aml MkEngine.aml \
          Test.aml

COMPONENTS = $(SOURCES:%.aml=%.alc)

INSTALLDIR=`alice-config --alicelibdir`/gecode/distributed-search

.PHONY: all msg depend clean veryclean distclean message

all: $(COMPONENTS)

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.predepend
	sed "/\/alice\//d" Makefile.predepend > Makefile.depend

$(COMPONENTS): %.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

##
## Installing
##
install: $(COMPONENTS)
	mkdir -p -m 775 $(INSTALLDIR)
	install -c -m444 $(COMPONENTS) $(INSTALLDIR)

##
## Cleaning Up
##

clean:
	rm -f $(COMPONENTS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

##
## Dependencies
##

-include Makefile.depend
