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

JAVA_LANG_SOURCES = \
	Class.cc Object.cc Throwable.cc System.cc Float.cc Double.cc \
	StrictMath.cc
JAVA_IO_SOURCES = FileDescriptor.cc FileOutputStream.cc
JAVA_SECURITY_SOURCES = AccessController.cc
SOURCES = \
	Outline.cc Data.cc ThrowWorker.cc ClassLoader.cc JavaByteCode.cc \
	ClassInfo.cc NativeMethodTable.cc ClassFile.cc ByteCodeInterpreter.cc \
	Startup.cc JavaLanguageLayer.cc \
	$(JAVA_LANG_SOURCES:%=lang/%) \
	$(JAVA_IO_SOURCES:%=io/%) \
	$(JAVA_SECURITY_SOURCES:%=security/%)

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
