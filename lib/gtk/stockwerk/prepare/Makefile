INSTALL = install -C -v -m 666

COMPS = GTK GDK GDK_PIXBUF G
DEFINES = $(COMPS:%=-D%_DISABLE_DEPRECATED)
GCC = gcc -E -Wall -std=99 -undef -D__GNUC__ $(DEFINES)
ALICEC = alicec --dump-phases


all: gtkraw.c gtkclean.c
	$(INSTALL) gtkclean.c ..
	rm -f gtkclean.c

gtkraw.c:
	$(GCC) `pkg-config --cflags gtk+-2.0` gtkdefs.c > gtkraw.c

gtkclean.c: Prepare.stc
	alicerun Prepare gtkraw.c gtkclean.c

%.stc: %.aml
	$(ALICEC) $< -o $@

clean:
	rm -f Prepare.stc gtkraw.c gtkclean.c
