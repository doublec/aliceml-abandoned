###
### Authors:
###   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
### 
### Copyright:
###   Thorsten Brunklaus, 2000
###   Leif Kornstaedt, 2001
### 
### Last change:
###   $Date$ by $Author$
###   $Revision$
### 

# Configuration Section

SRCTOP = ../..
SRCDIR = $(SRCTOP)/lib/gtk
BUILDDIR := $(shell pwd)
BUILDTOP = $(BUILDDIR)/../..

PREFIX=/opt/stockhausen-devel

SMLPLATFORM = x86-linux

# End of Configuration Section

OZPLATFORM := $(shell oztool platform)
VPATH = $(SRCDIR)

SML = sml
CPP = cpp -undef -D__GNUC__
CPPFLAGS := $(shell gtk-config --cflags gtk) -I$(SRCDIR)/gtk-canvas

OZC = ozc
OZCOPTS = -z9
OZTOOL = oztool
OZTOOLOPTS =

STOC = stoc
STOCOPTS = --no-dump-phases

OZSOURCES = GtkCoreOz.oz Gtk.oz Gdk.oz Canvas.oz
COMPONENTS = $(OZSOURCES:%.oz=%.ozf)
UNTYPEDCOMPONENTS = $(OZSOURCES:%.oz=Untyped%.ozf)

ALICETYPESOURCES = GtkCoreTypes.aml
ALICETYPECOMPONENTS = $(ALICETYPESOURCES:%.aml=%.ozf)

ALICESOURCES = GtkCore.aml
ALICECOMPONENTS = $(ALICESOURCES:%.aml=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/gtk
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/gtk/%) \
               $(ALICETYPECOMPONENTS:%=$(PREFIX)/lib/gtk/%) \
               $(ALICECOMPONENTS:%=$(PREFIX)/lib/gtk/%) \
               $(NATIVECOMPONENTS:%=$(PREFIX)/lib/gtk/%)

CXXFLAGS = $(CPPFLAGS) \
           -I/home/thor/devel/mozart/platform/emulator \
           -I/home/thor/devel/build/platform/emulator

NATIVESOURCES = GtkSignal.c GtkCanvas.c
NATIVEOBJS = $(NATIVESOURCES:%.c=%.o)
NATIVECOMPONENTS = $(NATIVESOURCES:%.c=%.so-$(OZPLATFORM))

NATIVEIMLIB = $(SRCDIR)/build/lib/libgdk_imlib.a
NATIVECANVAS = $(SRCDIR)/build/lib/libgtk-canvas.a

NATIVEFLAGS = $(SRCDIR)/build/lib/libgtk-canvas.a \
              $(SRCDIR)/build/lib/libgdk_imlib.a \
              $(SRCDIR)/build/lib/libart_lgpl.a \
              -lg -lgdk -lgtk

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(ALICETYPECOMPONENTS) $(COMPONENTS) $(ALICECOMPONENTS) \
     $(NATIVEIMLIB) $(NATIVECANVAS) $(NATIVECOMPONENTS) 

depend:
	touch Makefile.depend

header/gtkraw.c: header/gtk.c
	$(CPP) $(CPPFLAGS) $< $@

header/gtkheader.c: header/gtkraw.c header/filter.$(SMLPLATFORM)
	sml @SMLload=header/filter $< $@

header/filter.$(SMLPLATFORM): header/filter.sml header/make-filter.sml
	-rm -f $@
	(cd header && echo 'use "make-filter.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

collect.$(SMLPLATFORM): collect.sml make-collect.sml
	-rm -f $@
	echo 'use "make-collect.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

GtkWrapper.oz: collect.$(SMLPLATFORM) header/gtkheader.c
	sml @SMLload=collect header/gtkheader.c

GtkExport.oz: GtkWrapper.oz ;
Gtk.asig: GtkExport.oz ;
GdkWrapper.oz: Gtk.asig ;
GdkExport.oz: GdkWrapper.oz ;
Gdk.asig: GdkExport.oz ;
CanvasWrapper.oz: Gdk.asig ;
CanvasExport.oz: CanvasWrapper.oz ;
Canvas.asig: CanvasExport.oz ;

Untyped%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

%.ozf: Untyped%.ozf %.asig
	$(STOC) $(STOCOPTS) --replacesign $< $*.asig $@

$(ALICETYPECOMPONENTS): %.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

$(ALICECOMPONENTS): %.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

$(NATIVEOBJS):%.o: %.c
	$(OZTOOL) c++ $(CXXFLAGS) -c $< -o $@

%.so-$(OZPLATFORM): %.o
	oztool ld -o $@ $< $(NATIVEFLAGS)

UntypedGtk.ozf: GtkWrapper.oz GtkExport.oz
Gtk.ozf: GtkCore.ozf
UntypedGdk.ozf: GdkWrapper.oz GdkExport.oz
Gdk.ozf: GtkCore.ozf
UntypedCanvas.ozf: CanvasWrapper.oz CanvasExport.oz
Canvas.ozf: GtkCore.ozf

$(NATIVEIMLIB): gtk-canvas/imlib/config.log
	(cd gtk-canvas/imlib && $(MAKE) all install) || exit 1

$(NATIVECANVAS): gtk-canvas/config.log
	(cd gtk-canvas && $(MAKE) all install) || exit 1

gtk-canvas/imlib/config.log:
	(cd gtk-canvas/imlib && ./configure --prefix=$(BUILDDIR)/build) || exit 1

gtk-canvas/config.log:
	(cd gtk-canvas && ./configure --prefix=$(BUILDDIR)/build) || exit 1

-include Makefile.depend

##
## Installing
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/gtk/%.so-$(OZPLATFORM): %.so-$(OZPLATFORM)
	install -c -m 555 $< $@

$(PREFIX)/lib/gtk/%: %
	install -c -m 444 $< $@


##
## Cleaning Up
##

clean:
	-rm -f header/gtkraw.c header/gtkheader.c
	-rm -f GtkWrapper.oz GtkExport.oz Gtk.asig
	-rm -f GdkWrapper.oz GdkExport.oz Gdk.asig
	-rm -f CanvasWrapper.oz CanvasExport.oz Canvas.asig
	-rm -f $(UNTYPEDCOMPONENTS) $(ALICETYPECOMPONENTS) $(ALICECOMPONENTS) $(COMPONENTS)
	-rm -f GtkCanvas.c $(NATIVEOBJS) $(NATIVECOMPONENTS)
	-rm -rf build

veryclean: clean
	-rm -f header/filter.$(SMLPLATFORM) collect.$(SMLPLATFORM)

distclean: veryclean
	(cd gtk-canvas && $(MAKE) distclean) || exit 1
	-rm -f Makefile.depend
