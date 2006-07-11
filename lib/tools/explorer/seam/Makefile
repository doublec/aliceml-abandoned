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

SOURCES=					\
	MkExplorer.aml				\
	MkExplorerSearch.aml			\
	MkTreeNode.aml				\
	MkMainWindow.aml			\
	MkGtkNode.aml				\
	Defaults.aml				\
	MkEventHandler.aml			\
	MkCanvasHelper.aml			\
	MkTypes.aml				\
	MkVisualNode.aml			\
	MkSearch.aml				\
	MkExplorerNode.aml			\
	MkOptionsWindow.aml			\
	ExplorerInspect.aml			\
	MkSaveWindow.aml			\
	MkMenuManager.aml			\
	AboutDialog.aml				\
	EXPLORER_INSPECT-sig.aml		\
	CANVAS_HELPER-sig.aml			\
	EVENT_HANDLER-sig.aml			\
	EXPLORER-sig.aml			\
	EXPLORER_NODE-sig.aml			\
	EXPLORER_SEARCH-sig.aml			\
	GTK_NODE-sig.aml			\
	MAIN_WINDOW-sig.aml			\
	MENU_MANAGER-sig.aml			\
	OPTIONS_WINDOW-sig.aml			\
	SAVE_WINDOW-sig.aml			\
	SEARCH-sig.aml				\
	TREE_NODE-sig.aml			\
	TYPES-sig.aml				\
	VISUAL_NODE-sig.aml			\
	$(GLADE_TARGETS)

TARGETS= $(SOURCES:%.aml=%.alc)

INSTALL_TARGETS = Explorer LinkedMkExplorer EXPLORER_INSPECT-sig ExplorerInspect

.PHONY: clean all depend install

all: LinkedMkExplorer.alc Explorer.alc

LinkedMkExplorer.alc: $(TARGETS)
	alicelink -z --include ./ ./MkExplorer -o $@

.PHONY: generate
generate: $(GLADE_SOURCES)
	aliceglade $< -o $(GLADE_SOURCES:%.glade=%)

%.alc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS)
	rm -f Explorer.alc
	rm -f LinkedMkExplorer.alc

distclean: clean

depend: $(GLADE_TARGETS)
	alicedep $(SOURCES) > Makefile.predepend
	sed "/\/opt\//d" Makefile.predepend > Makefile.depend


install: $(INSTALL_TARGETS:%=%.alc)
	install -d $(INSTALLTO)
	install $(INSTALL_TARGETS:%=%.alc) $(INSTALLTO)/

-include Makefile.depend
