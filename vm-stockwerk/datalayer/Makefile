CC     = g++
CFLAGS = -I ../store -DDEBUG_CHECK -ggdb #-DINTERFACE -finline-functions -fno-implement-inlines -O6

SRCS = alicedata.cc

OBJS = alicedata.o

all: $(OBJS)
	ld -shared -o libdatalayer.so $(OBJS)

%.o : %.cc
	$(CC) $(CFLAGS) -c -o $@ $<

alicedata.cc: alicedata.hh

clean:
	rm -rf $(OBJS) libdatalayer.so

distclean: clean
	rm -rf *.s *~
