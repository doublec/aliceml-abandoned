CC     = g++
CFLAGS = -ggdb -DDEBUG_CHECK

SRCS = memmanager.cc \
       store.cc

OBJS = memmanager.o \
       store.o

all: test.cc $(OBJS)
	$(CC) $(CFLAGS) -o test test.cc $(OBJS)

%.o : %.cc base.hh types.hh headerdef.hh
	$(CC) $(CFLAGS) -c -o $@ $<

memmanager.cc: memmanager.hh memchunk.hh
store.cc: store.hh helper.hh tuple.hh

clean:
	rm -rf test $(OBJS)

distclean: clean
	rm -rf *.s *~
