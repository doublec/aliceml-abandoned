INCLUDES = -I$(top_srcdir) -I.
BUILT_SOURCES = StoreConfig.hh

storeincludedir = '${pkgincludedir}/store'

# StoreConfig

noinst_PROGRAMS = \
	StoreConfig

StoreConfig_SOURCES = \
	StoreConfig.cc

StoreConfig.hh: StoreConfig$(EXEEXT)
	./$<

# Convenience library

noinst_LTLIBRARIES = \
	libstore.la

libstore_la_SOURCES = \
	BaseMap.cc \
	Heap.cc \
	JITStore.cc \
	Map.cc \
	Store.cc \
	WeakMap.cc

nodist_libstore_la_SOURCES = \
	StoreConfig.hh

# Header files

dist_storeinclude_HEADERS = \
	Base.hh \
	BaseMap.hh \
	HeaderOp.hh \
	Heap.hh \
	JITStore.hh \
	Map.hh \
	MapNode.hh \
	PointerOp.hh \
	StatusWord.hh \
	Store.hh \
	Types.hh \
	Value.hh \
	WeakMap.hh

nodist_storeinclude_HEADERS = \
	StoreConfig.hh

dist_noinst_HEADERS = \
	GCHelper.hh \
	Parameter.hh \
	Set.hh
