##
## Author:
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Leif Kornstaedt, 2000-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = java alice generic adt store

SOURCES = Base.cc InitSeam.cc AliceMain.cc JavaMain.cc
OBJS = $(SOURCES:%.cc=%.o)

##
## Enumerate seam.dll files
##
ifdef LIGHTNING
STORE_LIGHTNING_SOURCES = JITStore.cc
else
STORE_LIGHTNING_SOURCES =
endif
STORE_SOURCES = Map.cc WeakMap.cc Heap.cc Store.cc $(STORE_LIGHTNING_SOURCES)
STORE_OBJS = $(STORE_SOURCES:%.cc=store/%.o)
ADT_SOURCES = ChunkMap.cc Outline.cc
ADT_OBJS = $(ADT_SOURCES:%.cc=adt/%.o)
GENERIC_SOURCES = \
	Outline.cc Debug.cc RootSet.cc UniqueString.cc \
	StackFrame.cc TaskStack.cc IOHandler.cc IODesc.cc SignalHandler.cc \
	Scheduler.cc Transients.cc Worker.cc Interpreter.cc \
	Primitive.cc PushCallWorker.cc BindFutureWorker.cc \
	Unpickler.cc Pickler.cc Profiler.cc
GENERIC_OBJS = $(GENERIC_SOURCES:%.cc=generic/%.o)
SEAM_OBJS = $(STORE_OBJS) $(ADT_OBJS) $(GENERIC_OBJS)

##
## Enumerate alice.dll files
##
ifdef LIGHTNING
ALICE_LIGHTNING_SOURCES = \
	NativeConcreteCode.cc NativeCodeInterpreter.cc NativeCodeJitter.cc
else
ALICE_LIGHTNING_SOURCES =
endif
ALICE_PRIMITIVE_MODULES = \
	Unqualified Array Byte Char Future General GlobalStamp Hole Int \
	List Math Option Real Ref Remote String Thread UniqueString Unsafe \
	Vector Word8 Word8Array Word8Vector Word31
ALICE_LIB_SYSTEM_MODULES = \
	Config IODesc OS CommandLine Component Debug Socket Rand Value \
	Reflect Foreign
ALICE_LIB_UTILITY_MODULES = ImpMap Cell Addr
ALICE_LIB_DISTRIBUTION_MODULES = Remote
ALICE_SOURCES = \
	Data.cc Guid.cc AbstractCode.cc PrimitiveTable.cc \
	LazySelInterpreter.cc AbstractCodeInterpreter.cc AliceConcreteCode.cc \
	AliceLanguageLayer.cc BootLinker.cc \
	$(ALICE_LIGHTNING_SOURCES) \
	$(ALICE_PRIMITIVE_MODULES:%=primitives/%.cc) \
	$(ALICE_LIB_SYSTEM_MODULES:%=lib/system/Unsafe%.cc) \
	$(ALICE_LIB_UTILITY_MODULES:%=lib/utility/Unsafe%.cc) \
	$(ALICE_LIB_DISTRIBUTION_MODULES:%=lib/distribution/Unsafe%.cc)
ALICE_OBJS = $(ALICE_SOURCES:%.cc=alice/%.o)

##
## Enumerate java.dll files
##
JAVA_JAVA_LANG_SOURCES = \
	Class.cc Object.cc String.cc Throwable.cc System.cc ClassLoader.cc \
	Float.cc Double.cc StrictMath.cc Thread.cc
JAVA_JAVA_IO_SOURCES = \
	FileDescriptor.cc FileInputStream.cc FileOutputStream.cc \
	ObjectStreamClass.cc
JAVA_JAVA_SECURITY_SOURCES = AccessController.cc
JAVA_SUN_MISC_SOURCES = Unsafe.cc AtomicLong.cc
JAVA_SUN_REFLECT_SOURCES = Reflection.cc NativeConstructorAccessorImpl.cc
JAVA_SOURCES = \
	Data.cc ThrowWorker.cc ClassLoader.cc JavaByteCode.cc \
	ClassInfo.cc NativeMethodTable.cc ClassFile.cc ByteCodeInterpreter.cc \
	Startup.cc JavaLanguageLayer.cc \
	Dump.cc \
	$(JAVA_JAVA_LANG_SOURCES:%=java/lang/%) \
	$(JAVA_JAVA_IO_SOURCES:%=java/io/%) \
	$(JAVA_JAVA_SECURITY_SOURCES:%=java/security/%) \
	$(JAVA_SUN_MISC_SOURCES:%=sun/misc/%) \
	$(JAVA_SUN_REFLECT_SOURCES:%=sun/reflect/%)
JAVA_OBJS = $(JAVA_SOURCES:%.cc=java/%.o)
##
## Done
##

LIBS = -L$(SUPPORTDIR)/lib $(EXTRA_LIBS) -lz

.PHONY: clean-local veryclean-local distclean-local $(SUBDIRS)

all: alice.exe java.exe

$(SUBDIRS): %:
	(cd $@ && $(MAKE) all)

seam.dll: Base.o InitSeam.o store adt generic
	$(LD) $(LDFLAGS) -shared -o $@ Base.o InitSeam.o $(SEAM_OBJS) $(LIBS)

alice.dll: seam.dll alice
	$(LD) $(LDFLAGS) $(ALICE_DLL_EXTRA_LDFLAGS) \
	-shared -o $@ $(ALICE_OBJS) seam.dll $(LIBS)

alice.exe: AliceMain.o alice.dll
	$(LD) $(LDFLAGS) -o $@ $< alice.dll seam.dll

java.dll: seam.dll java
	$(LD) $(LDFLAGS) $(JAVA_DLL_EXTRA_LDFLAGS) \
	-shared -o $@ $(JAVA_OBJS) seam.dll $(LIBS)

java.exe: JavaMain.o java.dll
	$(LD) $(LDFLAGS) -o $@ $< java.dll seam.dll

clean: clean-local
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done
veryclean: veryclean-local
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean) || exit 1; done
distclean: distclean-local
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean) || exit 1; done

clean-local:
	rm -f $(OBJS)
veryclean-local: clean-local
	rm -f seam.dll alice.dll alice.exe java.dll java.exe
distclean-local: veryclean-local
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES) store/StoreConfig.hh
	$(MAKEDEPEND) $(SOURCES) > Makefile.depend

store/StoreConfig.hh:
	cd store && make StoreConfig.hh

include Makefile.depend
