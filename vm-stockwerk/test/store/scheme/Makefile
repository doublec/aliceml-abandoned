TOPDIR = ../../..

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

FL     = flex
BB     = bison
BFLAGS = --verbose --debug --defines

SRCS = Parser.cc  \
       Interpreter.cc \
       Main.cc

OBJS = lex.yy.o           \
       scheme.bison.tab.o \
       Parser.o           \
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

Parser.cc: Parser.hh

clean:
	rm -f scheme scheme.bison.tab.h scheme.bison.tab.c scheme.bison.output lex.yy.c $(OBJS)

distclean: clean
	rm -rf *~
