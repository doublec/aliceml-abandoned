###
# Makefile for Sample-Binding

BINDINGS = gtk

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

all: alc obj dll hello.alc scramble.alc

################### SOURCE CODE GENERATION #####################

generate:
	sml @SMLload=$(GENERATOR) --create-types
	sml @SMLload=$(GENERATOR) -c c-config.xml -h c.h  
	sml @SMLload=$(GENERATOR) -c gtk-config.xml -h gtk.h -i glib.h -i GtkAuthoring.hh 


######################### COMPILE ##############################

alc: types.alc $(BINDINGS:%=%-sig.alc) c-sig.alc CORE-sig.alc Core.alc Gtk.alc
obj: $(BINDINGS:%=%-binding.o)
dll: $(BINDINGS:%=%.dll) c.dll NativeCore.dll

%.alc: %.aml
	alicec $<

%.o: %.cc
	$(ALICETOOL) -v cc -c $< -o $@

c.dll: c-binding.o c.o
	$(ALICETOOL) -v ld -Wl,-S c-binding.o c.o -o $@ `pkg-config --libs $(CLIBS)`

%.dll: %-binding.o 
	$(ALICETOOL) -v ld -Wl,-S $< -o $@ `pkg-config --libs $(CLIBS)`


######################### CLEAN ##############################

clean:
	rm -f $(OBJ_FILES) $(DLL_FILES)

