# Generated automatically from Makefile.in by configure.
PREFIX	    = //c/Programs/alice
SRCDIR      = .

OZC         = ozc
OZL         = ozl
COMPILE     = $(OZC) -c
LINK        = $(OZL)

CPP         = /usr/bin/cpp
CPPFLAGS    = -undef -D__GNUC__ -std=c99
GTKCONFIG   = /home/bruni/.wrapper/gtk-config
HAVE_CANVAS = yes
CANVASDIR   = /home/bruni/devel/wingtk/build

ALICEC      = alicec
ALICECOPTS  = --trace-component-access --dump-phases
ALICEL      = alicelink
ALICELOPTS  =

#---------------------------------------------------------------------
# Extra Variables
#---------------------------------------------------------------------

BUILDDIR        = $(SRCDIR)
GTKFLAGS        = $(shell $(GTKCONFIG) --cflags)
GTKLIBS         = $(shell $(GTKCONFIG) --libs)

ifeq ($(HAVE_CANVAS), yes)
GTKCANVASFLAGS = -I$(CANVASDIR)/include
GTKCANVASLIBS  = -L$(CANVASDIR)/lib -lgtk-canvas -lart_lgpl -lgdk_imlib
else
GTKCANVASFLAGS =
GTKCANVASLIBS  =
endif

CPPEXTRAFLAGS = $(shell $(GTKCONFIG) --cflags gtk) $(GTKCANVASFLAGS)

#---------------------------------------------------------------------
# Alice Binding
#---------------------------------------------------------------------

OZSOURCES = AliceWrapper.oz
OZOBJS    = $(OZSOURCES:%.oz=%.ozf)

ALICESOURCES = GtkTreeTypes.aml \
               UTIL-sig.aml \
               Util.aml \
               TYPE_MANAGER-sig.aml \
               MkTypeManager.aml \
               WRAPPER-sig.aml \
               MkWrapper.aml \
               SIGNATURE-sig.aml \
               MkSignature.aml \
               GTK_BINDING-sig.aml \
               GtkBinding.aml \
               GtkTypes.aml
ALICEOBJS    = $(ALICESOURCES:%.aml=%.ozf)

#---------------------------------------------------------------------
# Alice Wrapper Files
#---------------------------------------------------------------------

OZWRPSRCS = UnsafeGDK.oz \
            UnsafeGTK.oz \
            UnsafeGTKCANVAS.oz
OZWRPOBJS = $(OZWRPSRCS:%.oz=%.ozf)
OZWRPSIGS = $(OZWRPSRCS:%.oz=%.asig)

WRPSRCS    = GDK.aml \
             GTK.aml \
             GTKCANVAS.aml
WRPOBJS    = $(WRPSRCS:%.aml=%.ozf)
COMPONENTS = $(WRPOBJS:%.ozf=Linked%.ozf)

#---------------------------------------------------------------------
# Target Variables
#---------------------------------------------------------------------

TARGETS         = gtkraw.c $(ALICEOBJS) $(OZOBJS)
WRAPPER_TARGETS = $(OZWRPOBJS)

INSTALLDIRS  = $(PREFIX)/lib/gtk
INSTALLFILES = $(COMPONENTS:Linked%=$(PREFIX)/lib/gtk/%)

#---------------------------------------------------------------------
# Rules
#---------------------------------------------------------------------

%.ozf: %.oz
	$(COMPILE) $< -o $@

%.ozf: %.aml
	$(ALICEC) $(ALICECOPTS) -c $(BUILDDIR)/$< -o $(BUILDDIR)/$@

Linked%.ozf: %.ozf
	$(ALICEL) $(ALICELOPTS) $< -o $@

#---------------------------------------------------------------------
# Building
#---------------------------------------------------------------------

.PHONY: all clean veryclean distclean binding

all: $(TARGETS) binding

#---------------------------------------------------------------------
# Read GTK C Sources
#---------------------------------------------------------------------

gtkraw.c: gtkdefs.c
	$(CPP) $(CPPFLAGS) $(CPPEXTRAFLAGS) $< $@

#---------------------------------------------------------------------
# Create Binding Files
#---------------------------------------------------------------------

binding:
	alicerun "x-oz://system/gtk/Generator.ozf" \
          --no-native AliceWrapper.ozf
	@$(MAKE) wrapper_files

wrapper_files: $(WRAPPER_TARGETS)
wrapper: $(WRPOBJS) $(COMPONENTS)

#---------------------------------------------------------------------
# Installing
#---------------------------------------------------------------------

install: $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(PREFIX)/lib/gtk/%: Linked%
	install -c -m 444 $< $@

#---------------------------------------------------------------------
# Cleaning up
#---------------------------------------------------------------------

clean veryclean:
	-$(RM) $(ALICEOBJS) $(OZWRPSRCS) $(OZWRPSIGS) $(OZWRPOBJS) $(WRPOBJS)

wrapper_clean:
	-$(RM) $(WRPOBJS)

distclean: clean
	-$(RM) gtkraw.c config.* Makefile
