###
### Author:
###   Simon Pinkel <pinkel@ps.uni-sb.de>
###
### Copyright:
###   Simon Pinkel, 2004
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

# Makefile for alice mysql binding

INSTALLTO = `alice-config --alicelibdir`/mysql

NATIVES = UnsafeMySQL

SOURCES = UnsafeMySQL.cc

TARGETS = MySQL.alc

EXTRA_LIBS = `mysql_config --libs`
EXTRA_INCLUDES = `mysql_config --cflags`

MAKEDEPEND = alicetool cc -M $(DEFINES) $(INCLUDES)

all: $(NATIVES:%=%.dll) $(TARGETS)

MySQL.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool compile $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool link $(EXTRA_LIBS) $(SOURCES:%.cc=%.o) -o $@

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(NATIVES:%=%.o)
	rm -f *.alc
	rm -f $(NATIVES:%=%.dll.def)
	rm -f $(NATIVES:%=%.dll)

veryclean: clean
	rm -f Makefile.depend
	rm -f *~

depend:
	alicedep *.aml > Makefile.depend

install: all
	install -d $(INSTALLTO)
	install $(NATIVES:%=%.dll) $(TARGETS) $(INSTALLTO)

-include Makefile.depend
