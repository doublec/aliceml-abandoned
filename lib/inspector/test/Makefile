STOC     = stoc
STOCOPTS =

SRCS = values.aml \
       relation.aml \
       killer.aml

OBJS = values.ozf \
       relation.ozf \
       killer.ozf

%.ozf : %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

all: $(OBJS)

clean:
	rm -f *.oz*
