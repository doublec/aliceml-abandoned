SML = sml-cm
PLATFORM = x86-linux

all: stoc-mozart.$(PLATFORM)

stoc-mozart.$(PLATFORM):
	echo 'use "Exec-mozart.sml";' | $(SML)

stoc-com+.$(PLATFORM):
	echo 'use "Exec-com+.sml";' | $(SML)
