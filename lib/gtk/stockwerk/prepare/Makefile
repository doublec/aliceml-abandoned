###
# GTK+ 2.0 Bindings for Alice
#
# Preparation steps: 
# 1. generate a big header file with all GTK+ declarations
# 2. clean file from things that could confuse the CKit parser

INSTALL = ../myinstall -v -m 644
#COMPS = GTK GDK GDK_PIXBUF G
#OPTS = $(COMPS:%=-D%_DISABLE_DEPRECATED)
OPTS =
GCC = gcc -E -Wall -undef -U__GNUC__ -DGLIB_HAVE_ALLOCA_H \
          `pkg-config --cflags gtk+-2.0 libgnomecanvas-2.0`
ALICEC = alicec --dump-phases


all: gtkraw.c gtkclean.c
	$(INSTALL) gtkclean.c ..
	rm -f gtkclean.c

gtkraw.c: gtkdefs.c
	$(GCC) $(OPTS) gtkdefs.c > gtkraw.c

gtkclean.c: Prepare.stc
	alicerun Prepare gtkraw.c gtkclean.c

%.stc: %.aml
	$(ALICEC) $< -o $@

clean:
	rm -f Prepare.stc gtkraw.c gtkclean.c
