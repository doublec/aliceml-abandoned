BOOTSTRAPDIR= ../../../bootstrap

STOC     = stoc
STOCOPTS =
STODEP   = sml @SMLload=$(BOOTSTRAPDIR)/stodep

SRCS = hello.aml          \
       packbox.aml        \
       widgets.aml        \
       GtkBuilder-sig.aml \
       GtkBuilder.aml     \
       TextViewer.aml     \
       canvas.aml

OBJS = hello.ozf          \
       packbox.ozf        \
       widgets.ozf        \
       GtkBuilder-sig.ozf \
       GtkBuilder.ozf     \
       TextViewer.ozf     \
       canvas.ozf

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
