### -*- Makefile -*-
###
### Author:
###   Guido Tack <tack@ps.uni-sb.de>
###
### Copyright:
###   Guido Tack, 2004
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

INSTALLTO= `alice-config --alicelibdir`/tools

.PHONY: clean all install

GLADE_SOURCES = ExplorerGUI.glade
GLADE_TARGETS = ExplorerGUI.aml ExplorerGUISignals.aml

SOURCES= \
	Explorer.aml \
	ExplorerMain.aml \
	ExplorerSearch.aml \
	TreeNode.aml \
	MainWindow.aml \
	GtkNode.aml \
	Defaults.aml \
	EventHandler.aml \
	CanvasHelper.aml \
	Types.aml \
	VisualNode.aml \
	Search.aml \
	ExplorerNode.aml \
	OptionsWindow.aml \
	ExplorerInspect.aml \
	SaveWindow.aml \
	MenuManager.aml \
	$(GLADE_TARGETS)

TARGETS= $(SOURCES:%.aml=%.alc)

.PHONY: clean all depend install

all: LinkedExplorer.alc

LinkedExplorer.alc: $(TARGETS)
	alicelink --include ./ ./Explorer -o $@

$(GLADE_TARGETS): $(GLADE_SOURCES)
	aliceglade $< -o $(GLADE_SOURCES:%.glade=%)

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS) $(GLADE_TARGETS)

depend: $(GLADE_TARGETS)
	alicedep $(SOURCES) > Makefile.predepend
	sed "/\/opt\//d" Makefile.predepend > Makefile.depend


install: all
	install -d $(INSTALLTO)
	install LinkedExplorer.alc $(INSTALLTO)/Explorer.alc

-include Makefile.depend
