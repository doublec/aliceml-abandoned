
ALICEC = alicec
ALICEDEP = alicedep
ALICEC_OPTS =

SOURCES = Debug.aml \
          Path.aml Policy.aml \
          SPACENODE-sig.aml MkSpaceNode.aml \
          TREENODE-sig.aml MkTreeNode.aml TreeNode.aml \
          SEARCH-sig.aml MkSearch.aml \
          Test.aml

COMPONENTS = $(SOURCES:%.aml=%.stc)

#INSTALLDIRS = $(PREFIX)/lib/distribution/search
#INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/distribution/search/%)

.PHONY: all depend clean veryclean distclean

all: $(COMPONENTS)
	@echo "Read TODO"

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

#$(PREFIX)/lib/distribution/search/%: %
#	install -c -m 444 $< $@
#

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
