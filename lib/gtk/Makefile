SRCS = GTKCore.oz \
       GTK.oz \
       GDK.oz

OBJS = GTKCore.ozf \
       GTK.ozf \
       GDK.ozf

UOBJS = UntypedGTKCore.ozf \
        UntypedGTK.ozf \
        UntypedGDK.ozf

OZOBJS = GTKWrapper.oz \
         GTKExport.oz \
         GDKWrapper.oz \
         GDKExport.oz

OZSIGS = GTK.oz.sig \
         GDK.oz.sig

SMLOBJS = header/filter.x86-linux \
          collect.x86-linux

.PHONY: header interface

all: header interface $(OBJS)
	rm -f $(UOBJS)

%.ozf : %.oz
	ozc -o Untyped$@ -c $<
	stoc --replacesign Untyped$@ $<.sig $@

header:
	cd header && (echo "CM.make();" | sml) && (sml @SMLload=filter.x86-linux)

interface:
	(echo "CM.make();" | sml) && (sml @SMLload=collect.x86-linux)

install:
	if (test -e $(PREFIX)/lib/gtk); then echo; else mkdir $(PREFIX)/lib/gtk; fi;
	mv -f $(OBJS) $(PREFIX)/lib/gtk
	ln -f -s $(OZHOME)/share/GTK.so-linux-i486 $(PREFIX)/lib/gtk/GTK.so-linux-i486
	ln -f -s $(OZHOME)/share/GDK.so-linux-i486 $(PREFIX)/lib/gtk/GDK.so-linux-i486

clean:
	rm -f $(OBJS)
	rm -f $(UOBJS)
	rm -f $(OZOBJS)
	rm -f $(OZSIGS)

distclean: clean
	rm -f header/gtkheader.c $(SMLOBJS)
