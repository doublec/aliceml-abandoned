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

INSTALLTO= `alice-config --alicetoolsdir`

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

TARGETS= $(SOURCES:%.aml=%.stc)

.PHONY: clean all

all: LinkedExplorer.stc

LinkedExplorer.stc: $(TARGETS)
	alicelink --include ./ ./Explorer -o $@

$(GLADE_TARGETS): $(GLADE_SOURCES)
	aliceglade $< -o $(GLADE_SOURCES:%.glade=%)

%.stc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS) $(GLADE_TARGETS)

depend:
	alicedep $(SOURCES) > Makefile.predepend
	sed "/\/opt\//d" Makefile.predepend > Makefile.depend


install: all
	install -d $(INSTALLTO)
	install LinkedExplorer.stc $(INSTALLTO)/Explorer.stc

-include Makefile.depend
