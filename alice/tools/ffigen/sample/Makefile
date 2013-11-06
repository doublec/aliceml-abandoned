###
# Makefile for Sample-Binding


# executable names
ifdef WINDOWS
GENERATOR = ../Binding.x86-win32.exe
else
GENERATOR = ../Binding.x86-linux
endif

SML = sml
ALICETOOL = alicetool
NATIVES = util util_binding
CLIBS = gtk+-2.0 libgnomecanvas-2.0

code: types.alc c-sig.alc util-sig.alc c.dll util.dll demo.alc

all: generate types.alc c-sig.alc util-sig.alc c.dll util.dll demo.alc


################### SOURCE CODE GENERTION #####################

generate: 
	sml @SMLload=$(GENERATOR) --create-types
	sml @SMLload=$(GENERATOR) -c c-config.xml -h c.h
	sml @SMLload=$(GENERATOR) -c util-config.xml -h util.h -i glib.h -i GtkAuthoring.hh 

################### SOURCE CODE GENERTION #####################

types.alc: types.aml
	alicec types.aml

c-sig.alc: c-sig.aml
	alicec c-sig.aml

util-sig.alc: util-sig.aml
	alicec util-sig.aml

demo.alc: demo.aml
	alicec demo.aml

%.o: %.cc
	alicetool -v cc -c $< -o $@

c.dll: c-binding.o c.o
	alicetool -v ld -Wl,-S c-binding.o c.o -o c.dll `pkg-config --libs $(CLIBS)`

util.dll: util.o util-binding.o
	alicetool -v ld -Wl,-S util.o util-binding.o -o util.dll `pkg-config --libs $(CLIBS)`

clean:
	rm -f *.alc *.o *.dll
