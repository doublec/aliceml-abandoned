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

SIGS		= COLOR CREATE_DOC DRAW_NODES DRAWING_AREA DICTIONARIES HELPER INIT LAYOUT MK_COLOR_TABLE MK_DICTIONARY MK_PATH_TABLE WINDOW  NEW_NODE NEW_NODE_HELPER NODE_SEARCH SERVER SETTINGS TYPES UNSAFE_INSPECTOR UPDATE GTK_SUPPORT 

STRUCTS		= Color ColorMap CreateDoc DrawNodes Dictionaries Helper Init Layout  MainDictionary MkColorTable MkDictionary MkDrawingArea MkPathTable Window NewNode NewNodeHelper NodeSearch ReinspectedRelDict RelManDictionary Server Settings TransientDictionary Types UnsafeInspector Update GtkSupport 

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
