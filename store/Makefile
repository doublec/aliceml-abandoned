CC     = g++
CFLAGS = -DDEBUG_CHECK -Wall -ggdb #-DINTERFACE -finline-functions -fno-implement-inlines -O6

SRCS = store.cc

OBJS = store.o

LIBPATH = /home/thor/devel/stockhausen/vm-stockwerk/store

.PHONY: header

HEADERWIDTH      = 31
TAGWIDTH         = 16
SIZEWIDTH        = 11
GENERATIONSWIDTH = 2

all: lib

%.o : %.cc base.hh types.hh headerdef.hh
	$(CC) $(CFLAGS) -c -o $@ $<

lib: $(OBJS)
	ld -shared -o libstore.so $(OBJS)

test: test.cc
	$(CC) $(CFLAGS) -o test test.cc $(LIBPATH)/libstore.so

store.cc: memchunk.hh store.hh headerop.hh pointerop.hh dataset.hh gchelper.hh

header:
	$(CC) $(CFLAGS) -o headerdef headerdef.cc
	./headerdef $(HEADERWIDTH) $(TAGWIDTH) $(SIZEWIDTH) $(GENERATIONSWIDTH)

clean:
	rm -rf test $(OBJS) libstore.so

distclean: clean
	rm -rf *.s *~
