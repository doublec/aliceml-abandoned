SML = sml-cm
PLATFORM = x86-linux

SUBDIRS = backend-mozart

all: stoc-mozart.$(PLATFORM)
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) || exit 1); done

stoc-mozart.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "Exec-mozart.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

stoc-com+.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "Exec-com+.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi
