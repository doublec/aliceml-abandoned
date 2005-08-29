### -*- Makefile -*-
###
### Author:
###   Benedikt Grundmann <bgrund@ps.uni-sb.de>
###
### Copyright:
###   Benedikt Grundmann, 2005
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SOURCES= \
	Code.aml \
	SexpTools.aml \
	Lexer.aml \
	StrMap.aml \
	Rename.aml \
	AliceName.aml \
	AliceType.aml \
	BasicTypes.aml \
	TypeWrapper.aml \
	Ignore.aml \
	Defs.aml \
	DefsParser.aml \
	Generate.aml  \
	Naming.aml

WRAPPERS= \
	    GLib.aml \
	    Pango.aml \
	    Atk.aml \
	    Gdk.aml \
	    Gtk.aml \
	    Canvas.aml

WRAPPERSCAP= \
	    GLIB \
	    PANGO \
	    ATK \
	    GDK \
	    GTK \
	    CANVAS

DEFS=atk-types.defs atk.defs canvas.defs gdk-types.defs gdk.defs gtk-extrafuncs.defs gtk-types.defs gtk.defs pango-types.defs pango.defs wrapper.defs types canvas-properties.defs gtk-properties.defs

TARGETS= $(SOURCES:%.aml=%.alc) 

NATIVEWRAPPERS=$(WRAPPERS:%.aml=Native%.cc)
NATIVETARGETS=$(NATIVEWRAPPERS:%.cc=%.o)

WRAPPERS_SIGS_TML=$(WRAPPERSCAP:%=%-sig.tml)
WRAPPERS_SIGS=$(WRAPPERSCAP:%=%-sig.aml)
COMPILED_WRAPPERS=$(WRAPPERS:%.aml=%.alc)
COMPILED_WRAPPERS_SIGS=$(WRAPPERS_SIGS:%.aml=%.alc)
DLLS=$(NATIVEWRAPPERS:%.cc=%.dll)
INSTALLALICETARGETS =$(COMPILED_WRAPPERS_SIGS) $(COMPILED_WRAPPERS) CORE-sig.alc Core.alc Key.alc
INSTALLDLLTARGETS=$(DLLS) NativeCore.dll
INSTALLDIR=`alice-config --alicelibdir`/gtk
#ALICEDLL=`alice-config --alicedll`

PC_OPTS=gtk+-2.0 libgnomecanvas-2.0 glib-2.0

.PHONY: clean all wrappers installdll installalice compiledll compilealice

all: installalice installdll 

%-sig.aml: %-sig.tml
	gawk -f includesig.awk $< > $@

NativeCore.dll:	NativeCore.o
	alicetool -v link $^ $(ALICEDLL) -o $@ \
	`pkg-config $(PC_OPTS) --libs` 

NativeGLib.dll: NativeCore.dll NativeGLib.o 
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

NativePango.dll: NativeGLib.dll NativePango.o
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

NativeAtk.dll: NativeGLib.dll NativeAtk.o
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

NativeGdk.dll: NativeGLib.dll NativePango.dll NativeAtk.dll NativeGdk.o
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

NativeGtk.dll: NativeGLib.dll NativePango.dll NativeAtk.dll NativeGdk.dll NativeGtk.o
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

NativeCanvas.dll: NativeGLib.dll NativePango.dll NativeAtk.dll NativeGdk.dll NativeGtk.dll NativeCanvas.o
	alicetool link $(ALICEDLL) $^ -o $@ \
	 `pkg-config $(PC_OPTS) --libs`

Key.aml:
	./prepare-keyval.sh 
	
%.alc: %.aml
	alicec --no-warn-conventions -c $< -o $@

%.o: %.cc 
	alicetool -v compile -g `pkg-config $(PC_OPTS) --cflags` -c $< -o $@
	
clean:
	rm -f $(TARGETS) $(WRAPPERS) $(WRAPPERS_SIGS) $(WRAPPERS_SIGS:%.aml=%.alc) $(WRAPPERS_SIGS_TML) $(NATIVEWRAPPERS) $(NATIVEWRAPPERS:%.cc=%.hh) $(NATIVEWRAPPERS:%.cc=%.asig) $(NATIVEWRAPPERS:%.cc=%.o) $(COMPILED_WRAPPERS) $(DLLS) NativeCore.dll  NativeCore.o  CORE-sig.alc Key.alc Core.alc
	rm -f Main.alc

distclean: clean
	rm -f Makefile.depend

depend:
	alicedep $(SOURCES) > Makefile.depend

wrappers:	$(COMPILED_WRAPPERS)

$(WRAPPERS_SIGS):	$(WRAPPERS_SIGS_TML)

$(WRAPPERS) $(WRAPPERS_SIGS_TML) $(NATIVEWRAPPERS): Generate.alc $(DEFS)
	alicerun Generate.alc


compiledll:	$(DLLS)

compilealice:	$(INSTALLALICETARGETS)


### INSTALLATION #############################################################
	
install:	all installdll installalice

installdll:	$(DLLS)
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLDLLTARGETS) $(INSTALLDIR)

installalice: $(INSTALLALICETARGETS)
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLALICETARGETS) $(INSTALLDIR)



#### DEPENDENCIES ############################################################

$(COMPILED_WRAPPERS_SIGS) $(COMPILED_WRAPPERS): CORE-sig.alc Core.alc

Core.alc: 	CORE-sig.alc NativeCore.dll
GLib.alc:	GLIB-sig.alc NativeGLib.dll
PANGO-sig.alc:	GLib.alc     NativePango.dll
Pango.alc:	PANGO-sig.alc 
ATK-sig.alc:	NativeAtk.dll
Atk.alc:	ATK-sig.alc
GDK-sig.alc:	Pango.alc Atk.alc NativeGdk.dll
Gdk.alc:	GDK-sig.alc
GTK-sig.alc:	Gdk.alc NativeGtk.dll
Gtk.alc:	GTK-sig.alc
CANVAS-sig.alc:	Gtk.alc NativeCanvas.dll
Canvas.alc:	CANVAS-sig.alc


-include Makefile.depend
