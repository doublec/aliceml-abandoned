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
ALICELINK_OPTS = -v
ALICEDEP = alicedep

SIGS		= COLOR CREATE_TREE CREATE_TREE_HELPER DICTIONARY DOC DRAW DRAWING_AREA DICTIONARIES LAYOUT NODE SERVER SETTINGS UNSAFE_INSPECTOR UPDATE WIDGET INSPECTOR

STRUCTS		= Color CreateTree CreateTreeHelper Doc Draw Dictionaries Layout MkDictionary MkDrawingArea Node Server Settings UnsafeInspector Update GtkSupport Widget Inspector

SIGSOURCES 	= $(SIGS:%=%-sig.aml)
STRUCTSOURCES	= $(STRUCTS:%=%.aml)
SOURCES 	= $(SIGSOURCES) $(STRUCTSOURCES)

SIGFILES	= $(SIGS:%=%-sig.alc)
STRUCTFILES	= $(STRUCTS:%=%.alc)
FILES		= $(SIGFILES) $(STRUCTFILES)

INSTALLTO = `alice-config --alicelibdir`/tools

all: $(FILES) LinkedInspector.alc

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.depend

LinkedInspector.alc: $(FILES)
	$(ALICELINK) $(ALICELINK_OPTS) --include . ./Inspector -o $@

%.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

-include Makefile.depend

clean: 
	-rm -f *.alc

distclean: clean
	-rm -f Makefile.depend

install: LinkedInspector.alc
	mkdir -p -m 775 $(INSTALLTO) #install -d $(INSTALLTO)
	install -c -m444 LinkedInspector.alc $(INSTALLTO)/Inspector.alc
	install -c -m444 stop.xbm $(INSTALLTO)/stop.xbm
