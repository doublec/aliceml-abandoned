## -*- Makefile -*-
##
## Author:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2000
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

SRCS = GTKCore.oz \
       GTK.oz     \
       GDK.oz     \
       Canvas.oz

OBJS = GTKCore.ozf \
       GTK.ozf     \
       GDK.ozf     \
       Canvas.ozf

UOBJS = UntypedGTKCore.ozf \
        UntypedGTK.ozf     \
        UntypedGDK.ozf     \
        UntypedCanvas.ozf

OZOBJS = GTKWrapper.oz    \
         GTKExport.oz     \
         GDKWrapper.oz    \
         GDKExport.oz     \
         CanvasWrapper.oz \
         CanvasExport.oz

OZSIGS = GTK.ozf.sig \
         GDK.ozf.sig \
         Canvas.ozf.sig

SMLOBJS = header/filter.x86-linux \
          collect.x86-linux

SOURCES = GTKCore.ozf.sig \
          GTK.ozf.sig     \
          GDK.ozf.sig     \
          Canvas.ozf.sig

OZHOME=/opt/mozart-1.1.1
PREFIX=/opt/stockhausen-devel

.PHONY: header interface

all: header interface $(OBJS)
	rm -f $(UOBJS)

%.ozf : %.oz
	ozc -c $< -o $@
#	ozc -o Untyped$@ -c $<
#	stoc --replacesign Untyped$@ $<.sig $@

header:
	cd header && (echo "CM.make();" | sml) && (sml @SMLload=filter.x86-linux)

interface:
	(echo "CM.make();" | sml) && (sml @SMLload=collect.x86-linux)

install:
	(cd $(PREFIX)/lib && rm -rf gtk) || exit 1
	mkdir $(PREFIX)/lib/gtk
	cp $(OBJS) $(PREFIX)/lib/gtk
	cp $(SOURCES) $(PREFIX)/lib/gtk
	ln -f -s $(OZHOME)/share/GTK.so-linux-i486 $(PREFIX)/lib/gtk/GTK.so-linux-i486
	ln -f -s $(OZHOME)/share/GDK.so-linux-i486 $(PREFIX)/lib/gtk/GDK.so-linux-i486

clean:
	rm -f $(OBJS)
	rm -f $(UOBJS)
	rm -f $(OZOBJS)
	rm -f $(OZSIGS)

distclean: clean
	rm -f header/gtkheader.c $(SMLOBJS)
