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

BASE_SOURCES = \
	Data.cc ThrowWorker.cc ClassLoader.cc JavaByteCode.cc \
	ClassInfo.cc NativeMethodTable.cc ClassFile.cc ByteCodeInterpreter.cc \
	Startup.cc JavaLanguageLayer.cc \
	Dump.cc

JAVA_LANG_CLASSES = \
	Class Object String Throwable System ClassLoader \
	Float Double StrictMath Thread
JAVA_LANG_SOURCES = $(JAVA_LANG_CLASSES:%=java/lang/%.cc)
JAVA_IO_CLASSES = \
	FileDescriptor FileInputStream FileOutputStream ObjectStreamClass
JAVA_IO_SOURCES = $(JAVA_IO_CLASSES:%=java/io/%.cc)
JAVA_SECURITY_CLASSES = AccessController
JAVA_SECURITY_SOURCES = $(JAVA_SECURITY_CLASSES:%=java/security/%.cc)
SUN_MISC_CLASSES = Unsafe AtomicLong
SUN_MISC_SOURCES = $(SUN_MISC_CLASSES:%=sun/misc/%.cc)
SUN_REFLECT_CLASSES = Reflection NativeConstructorAccessorImpl
SUN_REFLECT_SOURCES = $(SUN_REFLECT_CLASSES:%=sun/reflect/%.cc)

SOURCES = \
	$(BASE_SOURCES) \
	$(JAVA_LANG_SOURCES) \
	$(JAVA_IO_SOURCES) \
	$(JAVA_SECURITY_SOURCES) \
	$(SUN_MISC_SOURCES) \
	$(SUN_REFLECT_SOURCES)

OBJS = $(SOURCES:%.cc=%.o)

all: $(OBJS)

clean:
	rm -f $(OBJS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	($(MAKEDEPEND) $(BASE_SOURCES) && \
	 for i in $(JAVA_LANG_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT java/lang/`basename $$i .cc`.o; \
	 done && \
	 for i in $(JAVA_IO_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT java/io/`basename $$i .cc`.o; \
	 done && \
	 for i in $(JAVA_SECURITY_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT java/security/`basename $$i .cc`.o; \
	 done && \
	 for i in $(SUN_MISC_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT sun/misc/`basename $$i .cc`.o; \
	 done && \
	 for i in $(SUN_REFLECT_SOURCES); do \
	     $(MAKEDEPEND) $$i -MT sun/reflect/`basename $$i .cc`.o; \
	 done) > $@

include Makefile.depend
