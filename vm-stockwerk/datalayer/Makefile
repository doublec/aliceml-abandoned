TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

OBJS = Alice.o

all: libdatalayer.a

libdatalayer.a: $(OBJS)
	ar rc $@ $(OBJS) && ranlib $@ 

#--** dependencies should be auto-generated
Alice.o: Alice.cc Alice.hh

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libdatalayer.a

distclean: veryclean
