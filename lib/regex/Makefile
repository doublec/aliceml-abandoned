# Makefile for alice xml binding

INSTALLTO = `alice-config --alicelibdir`/regex

ALICETOOL = alicetool


NATIVES = NativeRegex

SOURCES = \
	NativeRegex.cc

TARGETS = Regex.alc

EXTRA_LIBS = 
EXTRA_INCLUDES = 

MAKEDEPEND = $(ALICETOOL) -v cc -M $(DEFINES) $(INCLUDES)


all: $(NATIVES:%=%.dll) $(TARGETS)

%.o: %.cc
	$(ALICETOOL) compile -DPROFILE=0 $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	$(ALICETOOL) link $(EXTRA_LIBS) $(SOURCES:%.cc=%.o) -o $@

%.alc: %.aml
	alicec -z $< -o $@

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
