##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2002
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SOURCES = \
	Outline.cc Data.cc ClassLoader.cc ClassInfo.cc ClassFile.cc \
	ByteCodeInterpreter.cc JavaLanguageLayer.cc

OBJS = $(SOURCES:%.cc=%.o)

all: libjava.a

libjava.a: $(OBJS)
	-rm -f $@
	ar rc $@ $(OBJS) || exit 1
	ranlib $@

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libjava.a

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > $@

include Makefile.depend
