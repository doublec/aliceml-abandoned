# Makefile for the Alice easter egg

PC_OPTS = --cflags gtk+-2.0

ifeq ("$(GTK_PREFIX)", "")
else
  # path to pkgconfig's package descriptions (*.pc)
  export PKG_CONFIG_PATH := $(GTK_PREFIX)/lib/pkgconfig:$(PKG_CONFIG_PATH)

  # override the "prefix" setting in the package descriptions
  PC_OPTS := $(PC_OPTS) --define-variable=prefix=$(GTK_PREFIX)

endif

all: ToplevelIt.alc

ToplevelIt.alc: egg.alc egg.txt
	alicerun egg

egg.txt: egg
	./egg

egg: egg.h egg.c
	gcc `pkg-config $(PC_OPTS)` egg.c -o egg

egg.h: Alice_small.png
	gdk-pixbuf-csource $< > $@

egg.alc: egg.aml
	alicec -c egg.aml

clean:
	rm -f egg egg.txt egg.h egg.alc ToplevelIt.alc

install: all
	cp ToplevelIt.alc ../
