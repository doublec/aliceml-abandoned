# Makefile for alice xml binding

# Change this to reflect your installation directory
# INSTALLDIR = ../../install
INSTALLDIR = ../../vm-seam/build3/install


## No changes needed below this

INSTALLTO = $(INSTALLDIR)/lib/xml

NATIVES = NativeAliceXML

SOURCES = \
	NativeAliceXML.cc

TARGETS = AliceXML.stc

EXTRA_LIBS = -lxml2

EXTRA_INCLUDES = -I/usr/include/libxml2

CXX_WARNING_FLAGS = \
	-Wall -W -Wundef -Wpointer-arith -Wcast-qual -Wcast-align \
	-Wwrite-strings -Wconversion -Wstrict-prototypes \
	-Wmissing-prototypes -Winline -Wsynth -Woverloaded-virtual \
	-Wsign-promo

CXX_DEBUG_FLAGS = \
	-DINTERFACE -DDEBUG_CHECK -DSTORE_DEBUG -DJAVA_INTERPRETER_DEBUG \
	-fno-inline-functions -fimplement-inlines -ggdb

CXX_OPTIMIZE_FLAGS = \
	-pipe -march=i686 -O3 -fomit-frame-pointer \
	-fno-keep-static-consts

CXX_MACHINE_FLAGS = -mcpu=pentium

LD_OPTIMIZE_FLAGS = -Wl,-S

ALLFLAGS =  $(CXX_MACHINE_FLAGS) $(CXX_WARNING_FLAGS) $(CXX_OPTIMIZE_FLAGS)
#ALLFLAGS =  $(CXX_MACHINE_FLAGS) $(CXX_WARNING_FLAGS) $(CXX_DEBUG_FLAGS)

LDFLAGS = $(LD_OPTIMIZE_FLAGS)
#LDFLAGS =

MAKEDEPEND = alicetool -v cc -M $(DEFINES) $(INCLUDES)


all: $(NATIVES:%=%.dll) $(TARGETS)

AliceXML.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool -v c++ -DPROFILE=0 $(ALLFLAGS) $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool -v ld $(LDFLAGS) $(EXTRA_LIBS) $(SOURCES:%.cc=%.o) -o $@

%.stc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(NATIVES:%=%.o)
	rm -f $(TARGETS)

veryclean: clean
	rm -f $(NATIVES:%=%.dll.def)

distclean: veryclean
	rm -f $(NATIVES:%=%.dll)

depend:
	alicedep *.aml > Makefile.depend
#	$(MAKEDEPEND) $(SOURCES) >> Makefile.depend

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(NATIVES:%=%.asig) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
