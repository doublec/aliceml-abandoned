TEAM = anttrax

#PATH := /opt/alice-devel/bin:$(PATH)
ALICEC = alicec --no-warn-conventions --no-warn-unused-imports
ALICEDEP = alicedep
ALICERUN = alicerun

SIMULATOR_COMPONENTS = \
	Ant \
	Instructions \
	World \
	Simulator \
	InstructionsParser \
	WorldParser \
	SimulatorMain \
	Viewer

SIMULATOR_SOURCES = $(SIMULATOR_COMPONENTS:%=%.aml)
SIMULATOR_TARGETS = $(SIMULATOR_COMPONENTS:%=%.stc)

COMPILER_COMPONENTS = \
	ANTE-sig Ante Instructions

COMPILER_SOURCES = $(COMPILER_COMPONENTS:%=%.aml)
COMPILER_TARGETS = $(COMPILER_COMPONENTS:%=%.stc)

SOURCES = $(SIMULATOR_SOURCES) $(COMPILER_SOURCES)
TARGETS = $(SIMULATOR_TARGETS) $(COMPILER_TARGETS)

.PHONY: all simulator compiler parsec depend clean distclean submission

all: simulator compiler

compiler: $(COMPILER_TARGETS)
simulator: $(SIMULATOR_TARGETS)

%.stc: %.aml
	$(ALICEC) $<

%.ant: %.ante
	compile $<

parsec/Makefile.depend:
	(cd parsec && $(MAKE) depend)

parsec: parsec/Makefile.depend
	(cd parsec && $(MAKE))

submission:
	mkdir $(TEAM)
	cp README $(TEAM)
	cp anttrax.ant $(TEAM)/solution-1.ant
	mkdir $(TEAM)/tools
	mkdir $(TEAM)/tools/parsec
	cp $(SOURCES) $(TEAM)/tools
	cp parsec/*.aml $(TEAM)/tools/parsec
	cp Makefile.mozart $(TEAM)/tools/Makefile
	cp compile sim HOWTO $(TEAM)/tools
	cp anttrax.ante $(TEAM)/tools
	tar -czf $(TEAM).tar.gz $(TEAM)
	rm -rf $(TEAM)

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.depend

-include Makefile.depend

clean:
	rm -f $(TARGETS)

distclean: clean
	rm -f Makefile.depend
