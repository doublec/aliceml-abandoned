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


all: generate types.stc C-sig.stc util-sig.stc util.dll C.dll demo.stc


################### SOURCE CODE GENERTION #####################

generate: 
	sml @SMLload=$(GENERATOR) --create_basic
	sml @SMLload=$(GENERATOR) -c config.xml -h util.h -i glib.h -i GtkAuthoring.hh 


################### SOURCE CODE GENERTION #####################

types.stc: types.aml
	alicec types.aml

C-sig.stc: C-sig.aml
	alicec C-sig.aml

util-sig.stc: util-sig.aml
	alicec util-sig.aml

demo.stc: demo.aml
	alicec demo.aml

%.o: %.cc
	alicetool -v cc -c $< -o $@

C.dll: C-binding.o
	alicetool -v ld -Wl,-S C-binding.o -o C.dll `pkg-config --libs $(CLIBS)`

util.dll: util.o util-binding.o
	alicetool -v ld -Wl,-S util.o util-binding.o -o util.dll `pkg-config --libs $(CLIBS)`

clean:
	rm -f *.stc *.o *.dll
