# Makefile for alice xml binding

INSTALLDIR = `alice-config --alicelibdir`
INSTALLTO = $(INSTALLDIR)/xml

NATIVES = NativeAliceXML

SOURCES = \
	NativeAliceXML.cc

TARGETS = AliceXML.alc

EXTRA_LIBS = -lxml2

EXTRA_INCLUDES = -I/usr/include/libxml2

all: $(NATIVES:%=%.dll) $(TARGETS)

AliceXML.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool compile $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool link $(EXTRA_LIBS) $(SOURCES:%.cc=%.o) -o $@

%.alc: %.aml
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

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(NATIVES:%=%.asig) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
