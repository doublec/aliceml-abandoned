##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2002-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

JAVA_LANG_SOURCES = \
	Class.cc Object.cc String.cc Throwable.cc System.cc ClassLoader.cc \
	Float.cc Double.cc StrictMath.cc Thread.cc
JAVA_IO_SOURCES = \
	FileDescriptor.cc FileInputStream.cc FileOutputStream.cc \
	ObjectStreamClass.cc
JAVA_SECURITY_SOURCES = AccessController.cc
SUN_MISC_SOURCES = Unsafe.cc AtomicLong.cc
SUN_REFLECT_SOURCES = Reflection.cc NativeConstructorAccessorImpl.cc
SOURCES = \
	Data.cc ThrowWorker.cc ClassLoader.cc JavaByteCode.cc \
	ClassInfo.cc NativeMethodTable.cc ClassFile.cc ByteCodeInterpreter.cc \
	Startup.cc JavaLanguageLayer.cc \
	Dump.cc \
	$(JAVA_LANG_SOURCES:%=java/lang/%) \
	$(JAVA_IO_SOURCES:%=java/io/%) \
	$(JAVA_SECURITY_SOURCES:%=java/security/%) \
	$(SUN_MISC_SOURCES:%=sun/misc/%) \
	$(SUN_REFLECT_SOURCES:%=sun/reflect/%)

OBJS = $(SOURCES:%.cc=%.o)

all: $(OBJS)

clean:
	rm -f $(OBJS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > $@

include Makefile.depend
