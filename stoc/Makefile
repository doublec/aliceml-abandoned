SML := $(shell pwd)/../bootstrap/smla
PLATFORM = x86-linux

SUBDIRS = backend-mozart

all: stoc-mozart.$(PLATFORM)
	for i in $(SUBDIRS); do (cd $$i && $(MAKE) || exit 1); done

stodep.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "make-stodep.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

stoc-mozart.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "make-stoc-mozart.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

stot-mozart.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "make-stot-mozart.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

stoc-com+.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "make-stoc-com+.sml";' | $(SML))
	if [ ! -f $@ ]; then exit 1; fi

clean:
	rm -f stodep.* stoc-mozart.* stot-mozart.* stoc-com+.*
