SML = sml-cm
PLATFORM = x86-linux

SUBDIRS = backend-mozart

all: stoc-mozart.$(PLATFORM)
	for i in $(SUBDIRS); do cd $$i && $(MAKE) || exit 1; done

stoc-mozart.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "Exec-mozart.sml";' | $(SML))

stoc-com+.$(PLATFORM):
	-rm -f $@
	(cd top && echo 'use "top/Exec-com+.sml";' | $(SML))
