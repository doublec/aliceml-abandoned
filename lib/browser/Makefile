###
### Authors:
###   Bernadette Blum <blum@ps.uni-sb.de>
###  Marvin Schiller <schiller@ps.uni-sb.de>
###
### Copyright:
###   Bernadette Blum, 2002
###   Marvin Schiller, 2002
###
### Last Change:
###   $Date$ by $Author$
###   $Revision$
###

SRCTOP = ../..

ALICEC = alicec
ALICEC_OPTS = --no-warn-conventions --no-warn-unused-imports
ALICELINK = alicelink
ALICELINK_OPTS = -x
ALICEDEP = alicedep

SIGS		= COLOR CREATE_TREE CREATE_TREE_HELPER DICTIONARY DOC DRAW DRAWING_AREA DICTIONARIES LAYOUT NODE SERVER SETTINGS UNSAFE_INSPECTOR UPDATE WIDGET

STRUCTS		= Color CreateTree CreateTreeHelper Doc Draw Dictionaries Layout MkDictionary MkDrawingArea Node Server Settings UnsafeInspector Update GtkSupport Widget

SIGSOURCES 	= $(SIGS:%=%-sig.aml)
STRUCTSOURCES	= $(STRUCTS:%=%.aml)
SOURCES 	= $(SIGSOURCES) $(STRUCTSOURCES)

SIGFILES	= $(SIGS:%=%-sig.stc)
STRUCTFILES	= $(STRUCTS:%=%.stc)
FILES		= $(SIGFILES) $(STRUCTFILES)

all: $(FILES)

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.depend

ALICELINK_INPUTS = $(SOURCES)

%.stc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

-include Makefile.depend

clean: 
	-rm -f *.stc

distclean: clean
	-rm -f Makefile.depend
