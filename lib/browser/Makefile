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

SIGS		= COLOR CREATE_DOC DRAW_NODES DRAWING_AREA DICTIONARIES HELPER LAYOUT MK_COLOR_TABLE MK_DICTIONARY MK_PATH_TABLE MK_WINDOW  NEW_NODE NEW_NODE_HELPER NODE_SEARCH SERVER SERVICES SETTINGS TYPES UNSAFE_INSPECTOR UPDATE WINDOW_HELPER WINDOW_MANAGER 

STRUCTS		= Color ColorMap CreateDoc DrawNodes Dictionaries Helper Layout  MainDictionary MkColorTable MkDictionary MkDrawingArea MkPathTable MkWindow NewNode NewNodeHelper NodeSearch RelManDictionary Server Services Settings TransientDictionary Types UnsafeInspector Update WindowHelper WindowManager   

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
