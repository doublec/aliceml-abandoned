MORE = -I ~/devel/alice/install/include
FLAGS = `pkg-config --cflags --libs gtk+-2.0 libgnomecanvas-2.0` $(MORE)
CC = g++

ALICEC = alicec --dump-phases
ALICEDEP = alicedep
ALICERUN = alicerun

.PHONY: run all

all: gtktest.alc
	$(ALICERUN) gtktest

%: %.cc
	$(CC) $< -o $@ $(FLAGS)

%.alc: %.aml
	$(ALICEC) -c $< -o $@

depend:
	$(CC) -M *.cc $(FLAGS) > Makefile.depend
	$(ALICEDEP) *.aml >> Makefile.depend

-include Makefile.depend

clean:
	rm -f *.o
	rm -f *.alc
	rm -f test valisttest reftest canvastest

distclean:
	rm -f Makefile.depend
