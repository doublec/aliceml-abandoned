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

JAVA_LANG_SOURCES = Object.cc Float.cc Double.cc
SOURCES = \
	Outline.cc Data.cc ThrowWorker.cc ClassLoader.cc JavaByteCode.cc \
	ClassInfo.cc NativeMethodTable.cc ClassFile.cc ByteCodeInterpreter.cc \
	Startup.cc JavaLanguageLayer.cc \
	$(JAVA_LANG_SOURCES:%=lang/%)

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
