
ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS =

SOURCES = Debug.aml Utils.aml \
          Common.aml \
          INTERFACE-sig.aml MkEngine.aml \
          Test.aml

COMPONENTS = $(SOURCES:%.aml=%.alc)

#INSTALLDIRS = $(PREFIX)/lib/distribution/search
#INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/search/%)

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

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

$(PREFIX)/lib/distribution/search/%: %
	install -c -m 444 $< $@

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
