STOC     = stoc
STOCOPTS =

SRCS = hello.aml          \
       packbox.aml        \
       widgets.aml        \
       GtkBuilder-sig.aml \
       GtkBuilder.aml     \
       TextViewer.aml

OBJS = hello.ozf          \
       packbox.ozf        \
       widgets.ozf        \
       GtkBuilder-sig.ozf \
       GtkBuilder.ozf     \
       TextViewer.ozf

%.ozf: %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

all: $(OBJS)

clean:
	rm -f *.oz*
