BOOTSTRAPDIR= ../../../bootstrap

ALICEC      = alicec
ALICEC_OPTS = --dump-phases
ALICEDEP    = alicedep

SRCS = hello.aml          \
       packbox.aml        \
       canvas.aml         \
       scramble.aml       \
       GtkBuilder-sig.aml \
       GtkBuilder.aml     \
       move.aml           \
       widgets.aml        \
       TextViewer.aml     \
       AliceTour.aml
OBJS = $(SRCS:%.aml=%.ozf)

%.ozf: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

all: $(OBJS)

depend:
	$(ALICEDEP) $(SRCS) > Makefile.depend

clean:
	rm -f *.oz*

distclean: clean
	rm -f Makefile.depend

-include Makefile.depend
