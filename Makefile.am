## 
## Author:
##   Marco Kuhlmann <kuhlmann@ps.uni-sb.de>
## 
## Copyright:
##   Marco Kuhlmann, 2003
## 

## no interfaces changed, only implementation (good):
##    increment REVISION
## 
## interfaces added, none removed (good):
##    increment CURRENT, increment AGE, set REVISION to 0
## 
## interfaces removed (bad, breaks upwards compatibility):
##    increment CURRENT, set AGE and REVISION to 0

LIBSEAM_LT_CURRENT	= 0
LIBSEAM_LT_REVISION = 0
LIBSEAM_LT_AGE = 1

## 
## The installation procedure is pretty straightforward, except for two
## complications:
## 
## (1) Libtool requires the -no-undefined option when used on Windows.
## This option is added to EXTRA_LDFLAGS by the configure script and is
## passed to libseam.la.
## 
## (2) store/StoreConfig.hh is generated at compile-time and should be
## installed, but not distributed.  This requires that the store
## subdirectory gets its own Makefile.am, and the use of nodist.
## 

EXTRA_LDFLAGS = @EXTRA_LDFLAGS@

SUBDIRS = libltdl store
INCLUDES = $(LTDLINCL)

bin_PROGRAMS = seam

seam_SOURCES = SeamMain.cc
seam_LDADD = libseam.la

bin_SCRIPTS = seamtool

nobase_dist_pkginclude_HEADERS = \
	Base.hh \
	Seam.hh \
	store/Base.hh \
	store/Types.hh \
	store/HeaderOp.hh \
	store/PointerOp.hh \
	store/StatusWord.hh \
	store/Value.hh \
	store/Map.hh \
	store/WeakMap.hh \
	store/Store.hh \
	store/BaseMap.hh \
	store/BaseMap.cc \
	store/MapNode.hh \
	store/Heap.hh \
	store/JITStore.hh \
	adt/IntMap.hh \
	adt/ChunkMap.hh \
	adt/Queue.hh \
	adt/Stack.hh \
	generic/FinalizationSet.hh \
	generic/Transform.hh \
	generic/ConcreteRepresentationHandler.hh \
	generic/ConcreteRepresentation.hh \
	generic/ConcreteCode.hh \
	generic/Closure.hh \
	generic/Thread.hh \
	generic/ThreadQueue.hh \
	generic/Tuple.hh \
	generic/String.hh \
	generic/StackFrame.hh \
	generic/Backtrace.hh \
	generic/Pickle.hh \
	generic/Float.hh \
	generic/Double.hh \
	generic/Debug.hh \
	generic/RootSet.hh \
	generic/UniqueString.hh \
	generic/TaskStack.hh \
	generic/IOHandler.hh \
	generic/IODesc.hh \
	generic/SignalHandler.hh \
	generic/Scheduler.hh \
	generic/Transients.hh \
	generic/Worker.hh \
	generic/Interpreter.hh \
	generic/Primitive.hh \
	generic/PushCallWorker.hh \
	generic/BindFutureWorker.hh \
	generic/Unpickler.hh \
	generic/Pickler.hh \
	generic/Profiler.hh \
	generic/Broker.hh \
	generic/DllLoader.hh \
	generic/Authoring.hh \
	generic/JitterGenericData.hh \
	generic/DebugWorker.hh \
	generic/Debugger.hh \
	generic/DebuggerEvent.hh \
	generic/GenericDebuggerEvent.hh \
	generic/Stream.hh \
	libltdl/ltdl.h
nobase_nodist_pkginclude_HEADERS = \
	store/StoreConfig.hh

lib_LTLIBRARIES = libseam.la

libseam_la_SOURCES = \
	Base.cc \
	InitSeam.cc \
	Outline.cc \
	store/Map.cc \
	store/WeakMap.cc \
	store/Heap.cc \
	store/Store.cc \
	store/JITStore.cc \
	adt/ChunkMap.cc \
	generic/Debug.cc \
	generic/RootSet.cc \
	generic/UniqueString.cc \
	generic/StackFrame.cc \
	generic/TaskStack.cc \
	generic/IOHandler.cc \
	generic/IODesc.cc \
	generic/SignalHandler.cc \
	generic/Scheduler.cc \
	generic/Transients.cc \
	generic/Worker.cc \
	generic/Interpreter.cc \
	generic/Primitive.cc \
	generic/PushCallWorker.cc \
	generic/BindFutureWorker.cc \
	generic/Unpickler.cc \
	generic/Pickler.cc \
	generic/Profiler.cc \
	generic/Broker.cc \
	generic/DllLoader.cc \
	generic/DebugWorker.cc \
	generic/Debugger.cc \
	generic/GenericDebuggerEvent.cc \
	generic/Stream.cc
nodist_libseam_la_SOURCES = \
	store/StoreConfig.hh
libseam_la_LDFLAGS = $(EXTRA_LDFLAGS)
libseam_la_LIBADD = $(LIBLTDL) "-dlopen" self
libseam_la_DEPENDENCIES = $(LIBLTDL)

aclocaldir = $(datadir)/aclocal
dist_aclocal_DATA = seam.m4
