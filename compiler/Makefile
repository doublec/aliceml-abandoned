SML = sml-cm
PLATFORM = x86-linux

all: stoc-mozart.$(PLATFORM)

stoc-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "top/Exec-mozart.sml";' | $(SML)

stoc-com+.$(PLATFORM):
	-rm -f $@
	echo 'use "top/Exec-com+.sml";' | $(SML)
