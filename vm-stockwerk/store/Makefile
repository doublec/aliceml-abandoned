CC     = g++
CFLAGS = -DDEBUG_CHECK -ggdb #-DINTERFACE -finline-functions -fno-implement-inlines -O6

SRCS = headerdef.cc store.cc

OBJS = headerdef.o store.o

LIBPATH = /home/thor/devel/stockhausen/vm-stockwerk/store

all: lib test.cc
	$(CC) $(CFLAGS) -o test test.cc $(LIBPATH)/libstore.so

%.o : %.cc base.hh types.hh headerdef.hh
	$(CC) $(CFLAGS) -c -o $@ $<

lib: $(OBJS)
	ld -shared -o libstore.so $(OBJS)

headerdef.cc: headerdef.hh
store.cc: memchunk.hh store.hh headerop.hh pointerop.hh dataset.hh gchelper.hh

clean:
	rm -rf test $(OBJS) libstore.so

distclean: clean
	rm -rf *.s *~
