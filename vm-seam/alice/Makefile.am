SUBDIRS = lib primitives

INCLUDES = -I@top_srcdir@

noinst_LIBRARIES = libalicebase.a

libalicebase_a_SOURCES = \
	Data.cc \
	DebugEnvironment.cc \
	Guid.cc \
	AbstractCode.cc \
	PrimitiveTable.cc \
	LazySelInterpreter.cc \
	AbstractCodeFrame.cc \
	AbstractCodeInterpreter.cc \
	AliceConcreteCode.cc \
	AliceDebuggerEvent.cc \
	AliceLanguageLayer.cc \
	BootLinker.cc \
	NativeConcreteCode.cc \
	NativeCodeInterpreter.cc \
	NativeCodeJitter.cc

pkginclude_HEADERS = \
	AbstractCode.hh \
	AbstractCodeFrame.hh \
	AbstractCodeInterpreter.hh \
	AliceConcreteCode.hh \
	AliceDebuggerEvent.hh \
	AliceLanguageLayer.hh \
	Authoring.hh \
	Base.hh \
	BootLinker.hh \
	Data.hh \
	DebugEnvironment.hh \
	Guid.hh \
	NativeCodeInterpreter.hh \
	NativeCodeJitter.hh \
	NativeConcreteCode.hh \
	PrimitiveTable.hh \
	Types.hh

noinst_HEADERS = \
	JitterAliceData.hh \
	JitterImmediateEnv.hh \
	LazySelInterpreter.hh
