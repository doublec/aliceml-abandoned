
ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS =

SOURCES = Debug.aml Utils.aml \
          Common.aml \
          Path.aml Interface.aml \
          PARCEL-sig.aml \
          MANAGER-sig.aml MkManager.aml GManager.aml \
          MkEngine.aml \
          RSPACE-sig.aml MkGRSpace.aml \
          MkWorker.aml \
          RemoteWorker.aml \
          Test.aml

COMPONENTS = $(SOURCES:%.aml=%.stc)

#INSTALLDIRS = $(PREFIX)/lib/distribution/search
#INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/search/%)

.PHONY: all msg depend clean veryclean distclean message

all: $(COMPONENTS)
	@echo "*** Beware: each workers sleeps 520ms at each node."
	@echo "*** This is why the search is so slow."
	@echo

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.predepend
	sed "/\/alice\//d" Makefile.predepend > Makefile.depend

$(COMPONENTS): %.stc: %.aml
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
