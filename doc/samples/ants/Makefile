PATH := /opt/alice-devel/bin:$(PATH)
ALICEC = alicec --no-warn-conventions --no-warn-unused-imports
ALICEDEP =  alicedep
ALICERUN = alicerun

SIMULATOR_COMPONENTS = \
	Ant \
	Instructions \
	World \
	Simulator \
	InstructionsParser \
	WorldParser \
	SimulatorMain

SIMULATOR_SOURCES = $(SIMULATOR_COMPONENTS:%=%.aml)
SIMULATOR_TARGETS = $(SIMULATOR_COMPONENTS:%=%.stc)

COMPILER_COMPONENTS = \
	ANTE-sig Ante

COMPILER_SOURCES = $(COMPILER_COMPONENTS:%=%.aml)
COMPILER_TARGETS = $(COMPILER_COMPONENTS:%=%.stc)

SOURCES = $(SIMULATOR_SOURCES) $(COMPILER_SOURCES)
TARGETS = $(SIMULATOR_TARGETS) $(COMPILER_TARGETS)

.PHONY: all simulator compiler parsec depend clean distclean

all: simulator compiler

simulator: $(SIMULATOR_TARGETS) $(COMPILER_TARGETS)

%.stc: %.aml
	$(ALICEC) $<

%.ant: %.ante
	compile $<

parsec/Makefile.depend:
	(cd parsec && $(MAKE) depend)

parsec: parsec/Makefile.depend
	(cd parsec && $(MAKE))

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.depend

-include Makefile.depend

clean:
	rm -f $(TARGETS)

distclean: clean
	rm -f Makefile.depend
