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

GENERICSRCS = RootSet.cc \
              TaskStack.cc \
              Scheduler.cc \
              Transients.cc \
              Alice.cc

INTERPRETERSRCS = Interpreter.cc \
                  PushCallInterpreter.cc \
                  LazySelectionInterpreter.cc \
                  ByneedInterpreter.cc \
                  AbstractCodeInterpreter.cc \
                  Unpickler.cc \
                  Pickler.cc \
                  BootLinker.cc

PRIMITIVES = \
	Unqualified Array Char Future General GlobalStamp Hole Int LargeWord \
	List Math Option Real Ref String Thread Unsafe Vector Word
PRIMITIVESSRCS = Primitive.cc PrimitiveTable.cc $(PRIMITIVES:%=%.cc)

UNSAFE_SYSTEM = Config IO OS CommandLine Component Debug Rand Reflect
UNSAFE_UTILITY = MkRefMap
UNSAFESSRCS = $(UNSAFE_SYSTEM:%=Unsafe%.cc) $(UNSAFE_UTILITY:%=Unsafe%.cc)

SRCS = $(GENERICSRCS) \
       $(PRIMITIVESSRCS) \
       $(UNSAFESSRCS) \
       $(INTERPRETERSRCS) \
       Debug.cc \
       Base.cc \
       Outline.cc \
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
