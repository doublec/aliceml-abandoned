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
	ExplorerMain.aml \
	ExplorerSearch.aml \
	TreeNode.aml \
	MainWindow.aml \
	GtkNodes.aml \
	Defaults.aml \
	EventHandler.aml \
	CanvasHelper.aml \
	Types.aml \
	VisualNode.aml \
	Search.aml \
	ExplorerNode.aml \
	$(GLADE_TARGETS)

TARGETS= $(SOURCES:%.aml=%.stc)

.PHONY: clean all

all: Explorer.stc

Explorer.stc: $(TARGETS)
	alicelink --include ./ ./ExplorerMain -o $@

$(GLADE_TARGETS): $(GLADE_SOURCES)
	aliceglade $< -o $(GLADE_SOURCES:%.glade=%)

%.stc: %.aml
	alicec -c $< -o $@

clean:
	rm -f $(TARGETS) $(GLADE_TARGETS)

depend: $(SOURCES)
	alicedep $(SOURCES) > Makefile.depend

install: all
	install -d $(INSTALLTO)
	install Explorer.stc $(INSTALLTO)

-include Makefile.depend
