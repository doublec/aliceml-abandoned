###
# Makefile for Sample-Binding

BINDINGS = C gtk gdk

ASIG_FILES =  $(BINDINGS:%=%.asig)
SIG_FILES =  $(BINDINGS:%=%-sig.aml)
CC_FILES = $(BINDINGS:%=%-binding.cc)
OBJ_FILES = $(BINDINGS:%=%-binding.o)
DLL_FILES = $(BINDINGS:%=%.dll)


# executable names
ifdef WINDOWS
GENERATOR = ../Binding.x86-win32.exe 
else
GENERATOR = ../Binding.x86-linux 
endif

SML = sml
ALICEC = alicec
ALICETOOL = alicetool 
CLIBS = gtk+-2.0 libgnomecanvas-2.0

all: stc obj dll hello.stc scramble.stc

################### SOURCE CODE GENERATION #####################

generate:
	sml @SMLload=$(GENERATOR) --create_basic
	sml @SMLload=$(GENERATOR) -c gtk-config.xml -h gtk.h -i glib.h -i GtkAuthoring.hh 

######################### COMPILE ##############################

stc: types.stc $(BINDINGS:%=%-sig.stc) CORE-sig.stc Core.stc Gtk.stc
obj: $(BINDINGS:%=%-binding.o)
dll: $(BINDINGS:%=%.dll) NativeCore.dll

%.stc: %.aml
	alicec $<

%.o: %.cc
	$(ALICETOOL) -v cc -c $< -o $@

%.dll: %-binding.o 
	$(ALICETOOL) -v ld -Wl,-S $< -o $@ `pkg-config --libs $(CLIBS)`


######################### CLEAN ##############################

clean:
	rm -f $(OBJ_FILES) $(DLL_FILES)

