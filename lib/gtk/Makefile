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

OZHOME=/opt/mozart-1.1.1
PREFIX=/opt/stockhausen-devel

PLATFORM = x86-linux

# End of Configuration Section

SML = sml
CPP = cpp -undef -D__GNUC__
CPPFLAGS := $(shell gtk-config --cflags gtk) -I/home/asimon/bin/include

OZC = ozc
OZCOPTS =
STOC = stoc
STOCOPTS =

OZSOURCES = GTKCore.oz GTK.oz GDK.oz Canvas.oz
COMPONENTS = $(OZSOURCES:%.oz=%.ozf)

INSTALLDIRS = $(PREFIX)/lib/gtk
INSTALLFILES = $(COMPONENTS:%=$(PREFIX)/lib/gtk/%)

.PHONY: all depend install clean veryclean distclean

##
## Building
##

all: $(COMPONENTS)

depend:
	touch Makefile.depend

header/gtkraw.c: header/gtk.c
	$(CPP) $(CPPFLAGS) $< $@

#	gcc $(CPPFLAGS) -E $< -o $@

header/gtkheader.c: header/gtkraw.c header/filter.$(PLATFORM)
	sml @SMLload=header/filter $< $@

header/filter.$(PLATFORM): header/filter.sml header/make-filter.sml
	-rm -f $@
	(cd header && echo 'use "make-filter.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

collect.$(PLATFORM): collect.sml make-collect.sml
	-rm -f $@
	echo 'use "make-collect.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

GTKWrapper.oz: collect.$(PLATFORM) header/gtkheader.c
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

GTK.ozf: GTKCore.ozf
GDK.ozf: GTKCore.ozf
Canvas.ozf: GTKCore.ozf

-include Makefile.depend

##
## Installing
##

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/gtk/%: %
	install -c -m 444 $< $@

#	ln -s $(OZHOME)/share/GTK.so-linux-i486 $(PREFIX)/lib/gtk
#	ln -s $(OZHOME)/share/GDK.so-linux-i486 $(PREFIX)/lib/gtk

##
## Cleaning Up
##

clean:
	-rm -f header/gtkraw.c header/gtkheader.c
	-rm -f GTKWrapper.oz GTKExport.oz GTK.asig
	-rm -f GDKWrapper.oz GDKExport.oz GDK.asig
	-rm -f CanvasWrapper.oz CanvasExport.oz Canvas.asig
	-rm -f $(COMPONENTS)

veryclean: clean
	-rm -f header/filter.$(PLATFORM) collect.$(PLATFORM)

distclean: veryclean
	-rm -f Makefile.depend
