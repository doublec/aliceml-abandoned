STOC     = stoc
STOCOPTS =

SRCS = values.aml \
       relation.aml

OBJS = values.ozf \
       relation.ozf

%.ozf : %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

all: $(OBJS)

clean:
	rm -f *.oz*
