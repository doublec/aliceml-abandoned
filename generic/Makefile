##
## Authors:
##   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
##   Leif Kornstaedt <kornstae@ps.uni-sb.de>
## 
## Copyright:
##   Thorsten Brunklaus, 2002
##   Leif Kornstaedt, 2002-2003
## 
## Last change:
##   $Date$ by $Author$
##   $Revision$
## 

TOPDIR = ..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SOURCES = \
	Outline.cc Debug.cc RootSet.cc UniqueString.cc \
	StackFrame.cc TaskStack.cc IOHandler.cc IODesc.cc SignalHandler.cc \
	Scheduler.cc Transients.cc Worker.cc Interpreter.cc \
	Primitive.cc PushCallWorker.cc BindFutureWorker.cc \
	Unpickler.cc Pickler.cc Profiler.cc Broker.cc
OBJS = $(SOURCES:%.cc=%.o)

all: $(OBJS)

clean:
	rm -f $(OBJS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

Makefile.depend: Makefile $(SOURCES)
	$(MAKEDEPEND) $(SOURCES) > $@

include Makefile.depend
