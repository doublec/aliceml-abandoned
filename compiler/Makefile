SUBDIRS = backend-mozart

.PHONY: all clean veryclean distclean

all:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) || exit 1); done

clean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) clean || exit 1); done

veryclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) veryclean || exit 1); done

distclean:
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) distclean || exit 1); done
