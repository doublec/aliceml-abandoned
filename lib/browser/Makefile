SRCTOP = ../..

SRCDIR = $(SRCTOP)/inspector
VPATH = $(SRCDIR)

ALICEC = alicec
ALICELINK = alicelink
ALICELINK_OPTS = -x
ALICEDEP = alicedep

SOURCES = type.aml helper.aml dict.aml MkpathTable.aml create_doc.aml newNode.aml layout.aml Window.aml unsafe_insp.aml 

all: type.ozf helper.ozf dict.ozf MkpathTable.ozf create_doc.ozf newNode.ozf layout.ozf Window.ozf unsafe_insp.ozf

depend:
	alicedep $(SOURCES) > Makefile.depend 

ALICELINK_INPUTS = $(SOURCES)

%.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) $<

-include Makefile.depend

clean: 
	-rm *.ozf Makefile.depend
