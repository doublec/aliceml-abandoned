## -*- Makefile -*-
##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2002
##   Leif Kornstaedt, 2002
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

GENERICSRCS = \
	RootSet.cc \
	Outline.cc \
	TaskStack.cc \
	IOHandler.cc \
	Scheduler.cc \
	Transients.cc \
	Interpreter.cc \
	Guid.cc

SERVICESSRCS = \
	PushCallInterpreter.cc \
	ByneedInterpreter.cc \
	Unpickler.cc \
	Pickler.cc \
	BootLinker.cc

ALICESRCS = \
	Alice.cc \
	LazySelectionInterpreter.cc \
	AbstractCodeInterpreter.cc

PRIMITIVES = \
	Unqualified Array Char Future General GlobalStamp Hole Int LargeWord \
	List Math Option Real Ref String Thread Unsafe Vector Word
PRIMITIVESSRCS = Primitive.cc PrimitiveTable.cc $(PRIMITIVES:%=%.cc)

UNSAFE_SYSTEM = Config IO OS CommandLine Component Debug Rand Reflect Socket
UNSAFE_UTILITY = MkRefMap
UNSAFE_DISTRIBUTION = Remote
UNSAFESSRCS = \
	$(UNSAFE_SYSTEM:%=Unsafe%.cc) \
	$(UNSAFE_UTILITY:%=Unsafe%.cc) \
	$(UNSAFE_DISTRIBUTION:%=Unsafe%.cc)

SRCS = \
	$(GENERICSRCS) \
	$(SERVICESSRCS) \
	$(ALICESRCS) \
	$(PRIMITIVESSRCS) \
	$(UNSAFESSRCS) \
	Debug.cc \
	Base.cc \
	Properties.cc \
	Main.cc

OBJS = $(SRCS:%.cc=%.o)

all: $(OBJS)
	$(LD) $(CXXFLAGS) -o stow.exe $(OBJS) \
	-L$(TOPDIR)/adt -L$(TOPDIR)/store -ladt -lstore $(EXTRA_LIBS)

store:
	cd $(TOPDIR)/store && $(MAKE) distclean && $(MAKE)

adt:
	cd $(TOPDIR)/adt && $(MAKE) distclean && $(MAKE)

clean:
	rm -rf stow.exe $(OBJS)

distclean: clean
	rm -rf *~

Makefile.depend: Makefile $(SRCS)
	$(MAKEDEPEND) $(SRCS) > Makefile.depend

include Makefile.depend
