SML = sml-cm
PLATFORM = x86-linux

all: stoc-mozart.$(PLATFORM)

stoc-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "Exec-mozart.sml";' | $(SML)

stoc-com+.$(PLATFORM):
	-rm -f $@
	echo 'use "Exec-com+.sml";' | $(SML)
