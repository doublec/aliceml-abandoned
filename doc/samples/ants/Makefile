SIMULATOR_SOURCES = \
	Ant.aml \
	Instructions.aml \
	World.aml \
	Simulator.aml \
	InstructionsParser.aml

SIMULATOR_TARGETS = $(SIMULATOR_SOURCES:%.aml=%.stc)

SOURCES = $(SIMULATOR_SOURCES)
TARGETS = $(SIMULATOR_TARGETS)

.PHONY: all simulator depend clean

all: simulator

simulator: $(SIMULATOR_TARGETS)

%.stc: %.aml
	alicec -c $< -o $@

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend

clean:
	rm -f $(TARGETS)
