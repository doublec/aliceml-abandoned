
ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS =

SOURCES = CHANNEL-sig.aml Channel.aml \
	  Debug.aml Utils.aml \
          Common.aml \
          Path.aml Interface.aml \
          PARCEL-sig.aml \
          MANAGER-sig.aml Manager.aml GManager.aml \
          MkEngine.aml \
          RSPACE-sig.aml MkGRSpace.aml \
          MkWorker.aml \
          RemoteWorker.aml \
          Test.aml

COMPONENTS = $(SOURCES:%.aml=%.stc)

#INSTALLDIRS = $(PREFIX)/lib/distribution/search
#INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/search/%)

.PHONY: all depend clean veryclean distclean message

all: msg $(COMPONENTS)

msg:
	@echo "**************************************"
	@echo "- Reimplement MkRSpace.aml"
	@echo "- Remember, you have sleepms in MkWorker"
	@echo "- When generative types are back, look for ??? in MkEngine."
	@echo "- HANDLE exceptions in THREADS. See MkWorker for inspiration."
	@echo "  Also find WHO should fail : the manager, the worker, both ???"
	@echo "  In particular when a call to 'share' raises an exception."
	@echo "- Write (all?) the interfaces"
	@echo "**************************************"
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
	mkdir -p $@

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
