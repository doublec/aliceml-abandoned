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

NATIVES = UnsafeMySQL

SOURCES = UnsafeMySQL.cc

TARGETS = MySQL.stc

MYSQLCLIB = -L/usr/lib/mysql -lmysqlclient
EXTRA_LIBS = $(MYSQLCLIB)

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

LD_OPTIMIZE_FLAGS = # -Wl,-S

ALLFLAGS =  $(CXX_MACHINE_FLAGS) # $(CXX_WARNING_FLAGS) $(CXX_OPTIMIZE_FLAGS)

LDFLAGS = $(LD_OPTIMIZE_FLAGS)

MAKEDEPEND = alicetool -v cc -M $(DEFINES) $(INCLUDES)

all: $(NATIVES:%=%.dll) $(TARGETS)

MySQL.dll: $(SOURCES:%.cc=%.o)

%.o: %.cc
	alicetool -v compile $(ALLFLAGS) $(EXTRA_INCLUDES) \
	-c  $< -o $@

%.dll: %.o
	alicetool -v link $(LDFLAGS) $(EXTRA_LIBS) $(SOURCES:%.cc=%.o) -o $@

%.stc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(NATIVES:%=%.o)
	rm -f *.stc
	rm -f $(NATIVES:%=%.dll.def)
	rm -f $(NATIVES:%=%.dll)

veryclean: clean
	rm -f Makefile.depend
	rm -f *~

depend:
	alicedep *.aml > Makefile.depend

-include Makefile.depend
