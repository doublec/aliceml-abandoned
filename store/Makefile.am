INCLUDES = -I$(top_srcdir) -I.
BUILT_SOURCES = StoreConfig.hh

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

dist_pkginclude_HEADERS = \
	store/Base.hh \
	store/BaseMap.hh \
	store/HeaderOp.hh \
	store/Heap.hh \
	store/JITStore.hh \
	store/Map.hh \
	store/MapNode.hh \
	store/PointerOp.hh \
	store/StatusWord.hh \
	store/Store.hh \
	store/Types.hh \
	store/Value.hh \
	store/WeakMap.hh

nodist_pkginclude_HEADERS = \
	store/StoreConfig.hh

dist_noinst_HEADERS = \
	GCHelper.hh \
	Parameter.hh \
	Set.hh
