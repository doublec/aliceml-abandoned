TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = store adt scheduler datalayer builtins interpreter

OBJS = Main.o
LIBS = \
	interpreter/bootstrap/libbootstrapinterpreter.a \
	builtins/libbuiltins.a datalayer/libdatalayer.a \
	scheduler/libscheduler.a adt/libadt.a store/libstore.a

.PHONY: all-subdirs

all: all-subdirs stow

stow: $(OBJS)
	$(CXX) -o $@ $(OBJS) $(LIBS)

all-subdirs:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done

veryclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean) || exit 1; done

distclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean) || exit 1; done
