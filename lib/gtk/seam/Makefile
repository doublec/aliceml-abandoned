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

GENERATOR_SOURCES= \
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

GEN_WRAPPERS= \
	    GLib.aml \
	    Pango.aml \
	    Atk.aml \
	    Gdk.aml \
	    Gtk.aml \
	    Canvas.aml

GEN_WRAPPERSCAP= \
	    GLIB \
	    PANGO \
	    ATK \
	    GDK \
	    GTK \
	    CANVAS

VERSION=$(shell alicetool --version 2>&1 | head -n 1  | cut -d ' ' -f 5)

DEFS=atk-types.defs atk.defs canvas.defs gdk-types.defs gdk.defs gtk-extrafuncs.defs gtk-types.defs gtk.defs pango-types.defs pango.defs wrapper.defs types canvas-properties.defs gtk-properties.defs


MUST_GENERATE=yes
GENERATOR_SOURCE_TARGETS=$(GENERATOR_SOURCES:%.aml=%.alc)

# non generated interface files 
COMPILED_NON_GEN_WRAPPERS=CORE-sig.alc Core.alc Key.alc Glade.alc

# generated files
GEN_WRAPPERS_CC=$(GEN_WRAPPERS:%.aml=Native%.cc) NativeLibs.cc
GEN_WRAPPERS_HH=$(GEN_WRAPPERS:%.aml=Native%.hh) 
GEN_WRAPPERS_SIGS_TML=$(GEN_WRAPPERSCAP:%=%-sig.tml)
GEN_WRAPPERS_SIGS=$(GEN_WRAPPERSCAP:%=%-sig.aml)
COMPILED_GEN_WRAPPERS=$(GEN_WRAPPERS:%.aml=%.alc)
COMPILED_GEN_WRAPPERS_SIGS=$(GEN_WRAPPERS_SIGS:%.aml=%.alc)

# compiled c object files
WRAPPER_OBJECT_FILES=$(GEN_WRAPPERS_CC:%.cc=%.o) NativeGlade.o NativeCore.o

INSTALLALICETARGETS =$(COMPILED_GEN_WRAPPERS_SIGS) $(COMPILED_GEN_WRAPPERS) $(COMPILED_NON_GEN_WRAPPERS)
INSTALLDLLTARGETS=NativeLibs.dll
INSTALLDIR=`alice-config --alicelibdir`/gtk
#ALICEDLL=`alice-config --alicedll`
DISTFILES=$(GEN_WRAPPERS_HH) $(GEN_WRAPPERS_CC) NativeGlade.cc NativeCore.cc NativeUtils.hh MyNativeAuthoring.hh ExtraMarshaller.hh Makefile debian myinstall
DISTDIR=alice-gtk-$(VERSION)

ifeq ($(WINDOWS),1)
    CC_OPTS=-mms-bitfields
else
    CC_OPTS=
endif

PC_OPTS=gtk+-2.0 glib-2.0 libgnomecanvas-2.0 libglade-2.0

.PHONY: clean all wrappers installdll installalice compiledll compilealice

all: installalice installdll 

NativeLibs.asig: NativeLibs.tasig NativeCore.asig NativeGlade.asig
	head -n 4 $< > $@
	cat NativeCore.asig NativeGlade.asig >> $@
	tail -n +5 $< >> $@

%-sig.aml: %-sig.tml
	gawk -f includesig.awk $< > $@

NativeLibs.dll: NativeLibs.o NativeGlade.o NativeCanvas.o NativeGtk.o NativeGdk.o NativePango.o NativeAtk.o NativeGLib.o NativeCore.o
	alicetool -v link $^ -o $@ $(CC_OPTS) \
	`pkg-config $(PC_OPTS) --libs`

Key.aml:
	./prepare-keyval.sh 

%.alc: %.aml 
	alicec --no-warn-conventions -c $< -o $@

%.o: %.cc 
	alicetool -v compile -g \
	 $(CC_OPTS) `pkg-config $(PC_OPTS) --cflags`  -c $< -o $@

clean:
	rm -f NativeLibs.dll
	rm -f *.alc *.o

distclean: clean
	rm -f $(GEN_WRAPPERS_HH)
	rm -f $(GEN_WRAPPERS_CC)
	rm -f Makefile.depend

depend:
	alicedep $(GENERATOR_SOURCES) > Makefile.depend

wrappers:	$(GEN_WRAPPERS)

$(GEN_WRAPPERS_SIGS):	$(GEN_WRAPPERS_SIGS_TML)

ifeq "$(MUST_GENERATE)" "yes"
$(GEN_WRAPPERS) $(GEN_WRAPPERS_SIGS_TML) $(GEN_WRAPPERS_CC) NativeLibs.tasig: Generate.alc $(DEFS)
	alicerun Generate.alc > wrapper.log 2>&1
endif

compiledll:	$(DLLS)

compilealice:	$(INSTALLALICETARGETS)


### INSTALLATION #############################################################

install:	all installdll installalice

installdll:	$(INSTALLDLLTARGETS)
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLDLLTARGETS) $(INSTALLDIR)

installalice: $(INSTALLALICETARGETS)
	mkdir -p -m 775 $(INSTALLDIR)
	./myinstall $(INSTALLALICETARGETS) $(INSTALLDIR)


distdir:
	rm -rf $(DISTDIR)
	mkdir $(DISTDIR)
	cp -r $(DISTFILES) $(DISTDIR)

dist:	distdir
	tar chzf alice-gtk-$(VERSION).tar.gz $(DISTDIR)
	rm -rf $(DISTDIR)

#### DEPENDENCIES ############################################################

Core.alc: 	CORE-sig.alc NativeLibs.asig NativeLibs.dll
GLIB-sig.alc:	Core.alc
GLib.alc:	GLIB-sig.alc 
PANGO-sig.alc:	GLib.alc     
Pango.alc:	PANGO-sig.alc 
ATK-sig.alc:	Pango.alc
Atk.alc:	ATK-sig.alc
GDK-sig.alc:	Pango.alc Atk.alc 
Gdk.alc:	GDK-sig.alc
GTK-sig.alc:	Gdk.alc 
Gtk.alc:	GTK-sig.alc
CANVAS-sig.alc:	Gtk.alc 
Canvas.alc:	CANVAS-sig.alc
GLADE-sig.alc:	Canvas.alc
Glade.alc:	GLADE-sig.alc

-include Makefile.depend
