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
                  BootLinker.cc

PRIMITIVESSRCS = Primitive.cc \
                 PrimitiveTable.cc \
                 Array.cc \
                 Char.cc \
                 Future.cc \
                 General.cc \
                 GlobalStamp.cc \
                 Hole.cc \
                 Int.cc \
                 LargeWord.cc \
                 List.cc \
                 Math.cc \
                 Option.cc \
                 Real.cc \
                 Ref.cc \
                 String.cc \
                 Thread.cc \
                 Unqualified.cc \
                 Unsafe.cc \
                 Vector.cc \
                 Word.cc

UNSAFESSRCS = UnsafeMkRefMap.cc \
              UnsafeReflect.cc \
              UnsafeIO.cc \
              UnsafeComponent.cc \
              UnsafeOS.cc \
              UnsafeConfig.cc \
              UnsafeDebug.cc \
              UnsafeCommandLine.cc

SRCS = $(GENERICSRCS) \
       $(PRIMITIVESSRCS) \
       $(UNSAFESSRCS) \
       $(INTERPRETERSRCS) \
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
