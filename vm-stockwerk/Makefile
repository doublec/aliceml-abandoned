TOPDIR = .

include $(TOPDIR)/Makefile.vars
include $(TOPDIR)/Makefile.rules

SUBDIRS = store adt scheduler datalayer builtins interpreter

OBJS = Base.o Main.o
LIBS = \
	interpreter/libinterpreter.a \
	builtins/libbuiltins.a datalayer/libdatalayer.a \
	scheduler/libscheduler.a adt/libadt.a store/libstore.a \
	-lmsvcrt

.PHONY: all-subdirs

all: all-subdirs stow.exe

stow.exe: $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

all-subdirs:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) all) || exit 1; done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean) || exit 1; done
	rm -f $(OBJS)

veryclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean) || exit 1; done
	rm -f $(OBJS) stow.exe

distclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean) || exit 1; done
	rm -f $(OBJS) stow.exe
