# Makefile for the Alice easter egg

all: ToplevelIt.alc

ToplevelIt.alc: egg.alc egg.txt
	alicerun egg

egg.txt: egg
	./egg

egg: egg.h egg.c
	gcc `pkg-config --cflags gtk+-2.0` egg.c -o egg

egg.h: Alice_small.png
	gdk-pixbuf-csource $< > $@

egg.alc: egg.aml
	alicec -c egg.aml

clean:
	rm -f egg egg.txt egg.h egg.alc ToplevelIt.alc

install: all
	cp ToplevelIt.alc ../
