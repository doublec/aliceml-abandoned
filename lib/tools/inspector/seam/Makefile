
ALICEC = alicec
ALICEDEP = alicedep
ALICELINK = alicelink
ALICEC_OPTS =

## Components of the gui, in the subdirectory gui/
GUI = MAIN_WINDOW-sig MkMainWindow MkAboutWindow MkPrefsWindow \
      MkConnections \
      CANVAS_DRIVER-sig MkCanvasDriver \
      BOARD-sig MkBoard \
      GrTypes \
      LAYOUT-sig MkLayout LAYOUT_LIST-sig MkLayoutList \
      GR_TREE-sig MkGrTree \
      SELECTION-sig MkSelection \
      UPDATE-sig MkUpdate \
      MkTooltip

## Components in the main directory
MAIN = QUEUE-sig MkQueue \
       Prefs Common Cache \
       AbsTree BasicTypeMappers BuiltinTypeMappers TypeMappers \
       MONITORS-sig MkMonitors \
       HANDLERS-sig MkHandlers \
       Inspector \
       Test

ROOTCOMPONENT = Inspector
LINKEDNAME = InspectorL.alc

SOURCES = $(GUI:%=gui/%.aml) $(MAIN:%=%.aml)

COMPONENTS = $(SOURCES:%.aml=%.alc)

LINKCOMPONENTS = $(SOURCES:%.aml=--include %)

.PHONY: all depend clean veryclean distclean

all: $(COMPONENTS)
	@echo "Schnitzel time"

depend:
	$(ALICEDEP) $(SOURCES) > Makefile.predepend
	sed "/\/alice\//d" Makefile.predepend > Makefile.depend

$(COMPONENTS): %.alc: %.aml
	$(ALICEC) $(ALICEC_OPTS) -c $< -o $@

linked: $(COMPONENTS)
	$(ALICELINK) -v $(LINKCOMPONENTS) $(ROOTCOMPONENT) -o $(LINKEDNAME)


##
## Cleaning Up
##

clean:
	rm -f $(COMPONENTS)

veryclean: clean

distclean: veryclean
	rm -f Makefile.depend

##
## Dependencies
##

-include Makefile.depend
