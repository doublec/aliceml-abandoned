SML = sml-cm
PLATFORM = x86-linux

all: stoc-jvm.$(PLATFORM)

stoc-jvm.$(PLATFORM):
	-rm -f $@
	echo 'use "Exec-jvm.sml";' | $(SML)
