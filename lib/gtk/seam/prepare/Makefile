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

PLATFORM := $(shell ../platform.sh smlnj)
ifeq ("$(PLATFORM)", "")
  $(error Unknown platform)
endif
ifeq ("$(PLATFORM)", "x86-cygwin")
EXTRA_CC_OPTS = -DGLIB_HAVE_ALLOCA_H -D__IEEE_LITTLE_ENDIAN
SML = bash -c sml
else
ifeq ("$(PLATFORM)", "ppc-darwin")
EXTRA_CC_OPTS = -D__ppc__
SML = sml
else
EXTRA_CC_OPTS =
SML = sml
endif
endif

ifndef PC_OPTS
PC_OPTS = gtk+-2.0 libgnomecanvas-2.0
endif

GCC = gcc -E -Wall -undef -U__GNUC__  $(EXTRA_CC_OPTS) \
          `pkg-config --cflags $(PC_OPTS)`
ALICEC = alicec --no-warn-conventions
INSTALL = ../myinstall -v -m 644


all: gtkraw.c gtkclean.c PositiveList.sml
	$(INSTALL) gtkclean.c ..
	$(INSTALL) PositiveList.sml ..
	rm -f gtkclean.c
	rm -f PositiveList.sml

gtkraw.c: gtkdefs.c
	$(GCC) $(OPTS) gtkdefs.c > gtkraw.c

gtkclean.c: Prepare.alc
	alicerun Prepare gtkraw.c gtkclean.c


PositiveList.sml: DefParser.alc
	alicerun DefParser

%.alc: %.aml
	$(ALICEC) $< -o $@

clean:
	rm -f Prepare.alc DefParser.alc gtkraw.c gtkclean.c

distclean: clean
