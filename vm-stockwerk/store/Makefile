CC     = g++
CFLAGS = -ggdb -DDEBUG_CHECK

SRCS = store.cc

OBJS = store.o

all: test.cc $(OBJS)
	$(CC) $(CFLAGS) -o test test.cc $(OBJS)

%.o : %.cc base.hh types.hh headerdef.hh
	$(CC) $(CFLAGS) -c -o $@ $<

lib: $(OBJS)
	ar -r libstore.a $(OBJS)
	ranlib libstore.a

store.cc: memchunk.hh store.hh headerop.hh pointerop.hh dataset.hh gchelper.hh

clean:
	rm -rf test $(OBJS) libstore.a

distclean: clean
	rm -rf *.s *~
