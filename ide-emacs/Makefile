###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 2001
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SRCDIR = .
EMACS = emacs

%.elc: %.el
	$(EMACS) --eval "(setq load-path (cons \"$(SRCDIR)\" load-path))" -q -batch -f batch-byte-compile $< && \
	if [ $<c != $@ ]; then mv $<c $@; fi

all: stockhausen.elc
