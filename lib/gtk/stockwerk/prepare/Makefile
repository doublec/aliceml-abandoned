###
# GTK+ 2.0 Interface for Alice
#
# Preparation steps: 
# 1. generate a big header file with all GTK+ declarations
# 2. clean file from things that confuses the CKit parser

# uncomment the following lines to disable generation for deprecated functions
#COMPS = GTK GDK GDK_PIXBUF G
#OPTS = $(COMPS:%=-D%_DISABLE_DEPRECATED)
OPTS =

ifdef WINDOWS
EXTRA_CC_OPTS = -DGLIB_HAVE_ALLOCA_H -D__IEEE_BIG_ENDIAN
else
EXTRA_CC_OPTS =
endif

ifndef PC_OPTS
PC_OPTS = gtk+-2.0 libgnomecanvas-2.0
endif

GCC = gcc -E -Wall -undef -U__GNUC__  $(EXTRA_CC_OPTS) \
          `pkg-config --cflags $(PC_OPTS)`
ALICEC = alicec --no-warn-conventions
INSTALL = ../myinstall -v -m 644


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

distclean: clean
