TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SRCS = Store.cc
OBJS = $(SRCS:%.cc=%.o)

.PHONY: all clean distclean

all: libstore.a

libstore.a: StoreConfig.hh $(OBJS)
	ar rc libstore.a $(OBJS) || exit 1
	ranlib libstore.a

StoreConfig.hh: StoreConfig
	./StoreConfig

StoreConfig: StoreConfig.cc
	$(CXX) $(CXXFLAGS) -o StoreConfig StoreConfig.cc

$(OBJS): StoreConfig.hh

clean:
	rm -f StoreConfig.o StoreConfig StoreConfig.exe
	rm -f $(OBJS)

veryclean: clean
	rm -f test StoreConfig.hh libstore.a

distclean: veryclean
