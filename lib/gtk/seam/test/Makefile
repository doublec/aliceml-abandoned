MORE = -I ~/devel/alice/install/include
FLAGS = `pkg-config --cflags --libs gtk+-2.0 libgnomecanvas-2.0` $(MORE)
CC = g++

ALICEC = alicec --dump-phases
ALICEDEP = alicedep
ALICERUN = alicerun

.PHONY: run all

all: gtktest.stc
	$(ALICERUN) gtktest

%: %.cc
	$(CC) $< -o $@ $(FLAGS)

%.stc: %.aml
	$(ALICEC) -c $< -o $@

depend:
	$(CC) -M *.cc > Makefile.depend
	$(ALICEDEP) *.aml >> Makefile.depend

-include Makefile.depend

clean:
	rm -f *.o
	rm -f *.stc
	rm -f test valisttest reftest canvastest

distclean:
	rm -f Makefile.depend
