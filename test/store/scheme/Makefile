TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

FL     = flex
BB     = bison
BFLAGS = --verbose --debug --defines

SRCS = Parser.cc      \
       Environment.cc \
       Decorator.cc   \
       Interpreter.cc \
       Main.cc

OBJS = lex.yy.o           \
       scheme.bison.tab.o \
       Parser.o           \
       Environment.o      \
       Decorator.o        \
       Interpreter.o      \
       Main.o


all: scheme.bison.tab.c lex.yy.c $(OBJS)
	$(CXX) $(CXXFLAGS) -L$(TOPDIR)/store -L$(TOPDIR)/adt -o scheme $(OBJS) -lstore -ladt

%.o : %.c
	$(CXX) $(MACHINE_FLAGS) $(OPTIMIZE_FLAGS) -I. -I$(TOPDIR) -c $< -o $@

scheme.bison.tab.c: Nodes.hh scheme.bison
	$(BB) $(BFLAGS) scheme.bison

lex.yy.c: Nodes.hh scheme.flex
	$(FL) -I scheme.flex

Environment.cc: Environment.hh
Parser.cc: Parser.hh
Decorator.cc: Decorator.hh

store:
	cd ../../../store && $(MAKE) distclean && $(MAKE) all
adt:
	cd ../../../adt && $(MAKE) distclean && $(MAKE) all

clean:
	rm -f scheme scheme.bison.tab.h scheme.bison.tab.c scheme.bison.output lex.yy.c $(OBJS)

distclean: clean
	cd ../../../store && $(MAKE) distclean
	cd ../../../adt && $(MAKE) distclean
	rm -rf *~
