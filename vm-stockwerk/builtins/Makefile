TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = \
	GlobalPrimitives.o Primitive.o Internal.o Unqualified.o \
	Array.o Char.o Future.o General.o GlobalStamp.o Hole.o Int.o List.o \
	Math.o Option.o Real.o String.o Thread.o Unsafe.o Vector.o Word.o

#--** CommandLine, OS, TextIO do not belong here

all: libbuiltins.a

libbuiltins.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libbuiltins.a

distclean: veryclean
