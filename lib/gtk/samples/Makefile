BOOTSTRAPDIR= ../../../bootstrap

STOC     = alicec
STOCOPTS = --dump-phases
STODEP   = sml @SMLload=$(BOOTSTRAPDIR)/alicedep

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
	$(STOC) $(STOCOPTS) -c $< -o $@

all: $(OBJS)

depend:
	$(STODEP) $(SRCS) > Makefile.depend

clean:
	rm -f *.oz*

distclean: clean
	rm -f Makefile.depend

-include Makefile.depend
