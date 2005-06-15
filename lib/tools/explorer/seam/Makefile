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
	MkExplorer.aml \
	MkExplorerSearch.aml \
	MkTreeNode.aml \
	MkMainWindow.aml \
	MkGtkNode.aml \
	Defaults.aml \
	MkEventHandler.aml \
	MkCanvasHelper.aml \
	MkTypes.aml \
	MkVisualNode.aml \
	MkSearch.aml \
	MkExplorerNode.aml \
	MkOptionsWindow.aml \
	ExplorerInspect.aml \
	MkSaveWindow.aml \
	MkMenuManager.aml \
	AboutDialog.aml \
	$(GLADE_TARGETS)

TARGETS= $(SOURCES:%.aml=%.alc)

INSTALL_TARGETS = Explorer LinkedMkExplorer EXPLORER_INSPECT-sig ExplorerInspect

.PHONY: clean all depend install

all: LinkedMkExplorer.alc Explorer.alc

LinkedMkExplorer.alc: $(TARGETS)
	alicelink -z --include ./ ./MkExplorer -o $@

$(GLADE_TARGETS): $(GLADE_SOURCES)
	aliceglade $< -o $(GLADE_SOURCES:%.glade=%)

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS)

distclean:
	rm -f $(TARGETS) $(GLADE_TARGETS)

depend: $(GLADE_TARGETS)
	alicedep $(SOURCES) > Makefile.predepend
	sed "/\/opt\//d" Makefile.predepend > Makefile.depend


install: $(INSTALL_TARGETS:%=%.alc)
	install -d $(INSTALLTO)
	install $(INSTALL_TARGETS:%=%.alc) $(INSTALLTO)/

-include Makefile.depend
