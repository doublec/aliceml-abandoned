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
ALICELINK = alicelink
ALICELINK_OPTS = -x
ALICEDEP = alicedep

SIGS		= TYPES HELPER COLOR MK_COLOR_TABLE MK_DICTIONARY MK_PATH_TABLE NEW_NODE CREATE_DOC LAYOUT UNSAFE_INSPECTOR MK_DRAWING_AREA_ARG MK_DRAWING_AREA MK_WINDOW DRAW_NODES SERVICES WINDOW_MANAGER WINDOW_HELPER SETTINGS NODE_SEARCH DICTIONARIES UPDATE SERVER

STRUCTS		= Types Helper Color ColorMap MkColorTable MkDictionary MainDictionary RelManDictionary TransientDictionary MkPathTable NewNode CreateDoc Layout UnsafeInspector MkDrawingArea MkWindow DrawNodes Services WindowManager WindowHelper Settings NodeSearch Dictionaries Update Server

SIGSOURCES 	= $(SIGS:%=%-sig.aml)
STRUCTSOURCES	= $(STRUCTS:%=%.aml)
SOURCES 	= $(SIGSOURCES) $(STRUCTSOURCES)

SIGFILES	= $(SIGS:%=%-sig.ozf)
STRUCTFILES	= $(STRUCTS:%=%.ozf)
FILES		= $(SIGFILES) $(STRUCTFILES)

all: $(FILES)

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.depend

ALICELINK_INPUTS = $(SOURCES)

%.ozf: %.aml
	$(ALICEC) $<

-include Makefile.depend

clean: 
	-rm *.ozf Makefile.depend
