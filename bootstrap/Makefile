###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 1999-2001
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SML := $(shell pwd)/smla
PLATFORM = x86-linux

all: stodep.$(PLATFORM) stoc-mozart.$(PLATFORM)

stodep.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stodep.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stoc-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stoc-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stot-mozart.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stot-mozart.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

stoc-com+.$(PLATFORM):
	-rm -f $@
	echo 'use "make-stoc-com+.sml";' | $(SML)
	if [ ! -f $@ ]; then exit 1; fi

clean:
	rm -f stodep.* stoc-mozart.* stot-mozart.* stoc-com+.*
