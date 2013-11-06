STOC     = stoc
STOCOPTS =

SRCS = values.aml \
       relation.aml \
       killer.aml \
       rtt.aml \
       names.aml

OBJS = $(SRCS:%.aml=%.ozf)

%.ozf : %.aml
	$(STOC) $(STOCOPTS) -c $< -o $@

all: $(OBJS)

clean:
	rm -f *.oz*
