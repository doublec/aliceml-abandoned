SOURCES=CharParser.aml ERROR-sig.aml MkError.aml COMBINATORS-sig.aml MkCombinators.aml MkPrim.aml PRIM-sig.aml TOKEN-sig.aml
ALICEC=alicec
ALICEDEP=alicedep

COMPONENTS=$(addsuffix .alc, $(basename $(SOURCES)))

all: $(COMPONENTS)


%.ozf : %.aml
	$(ALICEC) $< -o $@	

%.alc : %.aml
	$(ALICEC) $< -o $@	

depend: 
	$(ALICEDEP) $(SOURCES) > Makefile.depend

clean:
	rm -f $(COMPONENTS)

-include Makefile.depend
