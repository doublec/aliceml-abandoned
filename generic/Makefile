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

SOURCES = \
	Outline.cc Debug.cc RootSet.cc UniqueString.cc Properties.cc \
	StackFrame.cc TaskStack.cc IOHandler.cc IODesc.cc SignalHandler.cc \
	Scheduler.cc Transients.cc Worker.cc Interpreter.cc \
	Primitive.cc PushCallWorker.cc BindFutureWorker.cc \
	Unpickler.cc Pickler.cc BootLinker.cc \
	Profiler.cc

OBJS = $(SOURCES:%.cc=%.o)

all: libgeneric.a

libgeneric.a: $(OBJS)
	ar rc libgeneric.a $(OBJS) || exit 1
	ranlib libgeneric.a

clean:
	rm -f $(OBJS)

veryclean: clean
	rm -f libgeneric.a

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > $@

include Makefile.depend
