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
STOC = stoc
STOCOPTS =

OZSOURCES = GTKCore.oz GTK.oz GDK.oz Canvas.oz
COMPONENTS = $(OZSOURCES:%.oz=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/gtk
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/gtk/%)

CXXFLAGS = $(CPPFLAGS)

NATIVESOURCES = GtkCanvas.c
NATIVEOBJS = GtkCanvas.o
NATIVEFILES = gtkcanvas.so-$(OZPLATFORM)
NATIVEFLAGS = $(SRCDIR)/build/lib/libgtk-canvas.a \
              $(SRCDIR)/build/lib/libart_lgpl.a \
              $(SRCDIR)/gtk-canvas/build/lib/libgdk_imlib.a \
              -lg -lgdk -lgtk

.PHONY: all depend install clean veryclean distclean native

##
## Building
##

all: $(COMPONENTS) native

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

GTKWrapper.oz: collect.$(SMLPLATFORM) header/gtkheader.c
	sml @SMLload=collect header/gtkheader.c

GTKExport.oz: GTKWrapper.oz ;
GTK.asig: GTKExport.oz ;
GDKWrapper.oz: GTK.asig ;
GDKExport.oz: GDKWrapper.oz ;
GDK.asig: GDKExport.oz ;
CanvasWrapper.oz: GDK.asig ;
CanvasExport.oz: CanvasWrapper.oz ;
Canvas.asig: CanvasExport.oz ;

Untyped%.ozf: %.oz
	$(OZC) $(OZCOPTS) -c $< -o $@

%.ozf: Untyped%.ozf %.asig
	$(STOC) $(STOCOPTS) --replacesign $< $*.asig $@

UntypedGTK.ozf: GTKWrapper.oz GTKExport.oz
GTK.ozf: GTKCore.ozf
UntypedGDK.ozf: GDKWrapper.oz GDKExport.oz
GDK.ozf: GTKCore.ozf
UntypedCanvas.ozf: CanvasWrapper.oz CanvasExport.oz
Canvas.ozf: GTKCore.ozf

native:
	(cd gtk-canvas/imlib && ./configure --prefix=$(BUILDDIR)/build) || exit 1
	(cd gtk-canvas/imlib && $(MAKE) all install) || exit 1
	(cd gtk-canvas && ./configure --prefix=$(BUILDDIR)/build) || exit 1
	(cd gtk-canvas && $(MAKE) all install) || exit 1
	oztool c++ $(CXXFLAGS) -c GtkCanvas.c -o GtkCanvas.o || exit 1
	oztool ld -o gtkcanvas.so-$(OZPLATFORM) GtkCanvas.o $(NATIVEFLAGS)

natshort:
	oztool c++ $(CXXFLAGS) -c GtkCanvas.c -o GtkCanvas.o || exit 1
	oztool ld -o gtkcanvas.so-$(OZPLATFORM) GtkCanvas.o $(NATIVEFLAGS)
	install -c -m 555 $(NATIVEFILES) $(PREFIX)/lib/gtk

mytest:
	g++ $(CPPFLAGS) -c mytest.c -o mytest.o
	g++ $(CPPFLAGS) -o mytest mytest.o -lg -lgdk -lgtk -lgtk-canvas -lart_lgpl

-include Makefile.depend

##
## Installing
##

install: $(INSTALLDIRS) $(INSTALLFILES)
	install -c -m 555 $(NATIVEFILES) $(PREFIX)/lib/gtk

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/gtk/%: %
	install -c -m 444 $< $@

##
## Cleaning Up
##

clean:
	-rm -f header/gtkraw.c header/gtkheader.c
	-rm -f GTKWrapper.oz GTKExport.oz GTK.asig
	-rm -f GDKWrapper.oz GDKExport.oz GDK.asig
	-rm -f CanvasWrapper.oz CanvasExport.oz Canvas.asig
	-rm -f $(COMPONENTS)
	-rm -f $(NATIVESOURCES) $(NATIVEOBJS) $(NATIVEFILES)
	-rm -rf build

veryclean: clean
	-rm -f header/filter.$(SMLPLATFORM) collect.$(SMLPLATFORM)

distclean: veryclean
	(cd gtk-canvas/imlib && $(MAKE) distclean) || exit 1
	(cd gtk-canvas && $(MAKE) distclean) || exit 1
	-rm -f Makefile.depend
