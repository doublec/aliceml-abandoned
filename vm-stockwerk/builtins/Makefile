TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = \
	Array.o Char.o CommandLine.o Future.o General.o Hole.o Int.o Math.o \
	OS.o Real.o String.o TextIO.o Thread.o Unsafe.o Vector.o Word.o \
	Builtins.o #Prebound.o

all: libbuiltins.a

libbuiltins.a: $(OBJS)
	ar rcs $@ $(OBJS)

clean:
	rm -rf $(OBJS) libbuiltins.a

distclean: clean
