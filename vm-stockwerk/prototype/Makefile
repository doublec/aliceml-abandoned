###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 2002
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

SRCS = PrimitiveTable.oz Pickle.oz AbstractCodeInterpreter.oz Scheduler.oz

%.ozf: %.oz
	ozc -c $< -o $@

all: $(SRCS:%.oz=%.ozf)

clean:
	-rm -f $(SRCS:%.oz=%.ozf)
