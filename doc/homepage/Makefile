SML_MODE_VERSION = 4.0
SML_MODE_PACKAGE = sml-mode-$(SML_MODE_VERSION)+alice

PHP = php

PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/download \
	$(PREFIX)/papers

INSTALLFILES_0 = \
	style.css bg.gif alice.gif macros.php3 project.html \
	index.html people.html papers.html download.html contact.html \
	contribs.html
INSTALLFILES_DOWNLOAD_0 = \
	COPYING CHANGES
INSTALLFILES_PAPERS_0 = \
	intermediate.ps NEPBericht2001.pdf NEPAntrag2001.pdf NEPReport2004.pdf
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%) \
	$(INSTALLFILES_PAPERS_0:%=$(PREFIX)/papers/%)

DOWNLOADFILES = $(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%)

.PHONY: install install-sml-mode

all: install

install: $(DIRS) $(INSTALLFILES)
	(cd ../manual && make PREFIX=$(PREFIX)/manual-devel)

release: install download
	(cd ../manual && make PREFIX=$(PREFIX)/manual)

download: $(DOWNLOADFILES)

$(DIRS):
	mkdir -p -m 775 $@

$(PREFIX)/%.html: %.html macros.php3 project.html
	$(PHP) -q $< >$<.static
	install -c -m 444 $<.static $@
	rm -f $<.static

$(PREFIX)/%: %
	install -c -m 444 $< $@

$(PREFIX)/alice.gif: \
	../../misc/logo/alice-strikethrough-shadow-blue-254x80.gif
	install -c -m 444 $< $@

install-sml-mode:
	mkdir $(SML_MODE_PACKAGE)
	cp -r ../../misc/sml-mode/* $(SML_MODE_PACKAGE)
	rm -rf $(SML_MODE_PACKAGE)/CVS
	tar -czf $(SML_MODE_PACKAGE).tgz $(SML_MODE_PACKAGE)/*
	zip  $(SML_MODE_PACKAGE).zip $(SML_MODE_PACKAGE)/*
	install -c -m 444 $(SML_MODE_PACKAGE).tgz $(PREFIX)/download/
	install -c -m 444 $(SML_MODE_PACKAGE).zip $(PREFIX)/download/
	rm -rf $(SML_MODE_PACKAGE) $(SML_MODE_PACKAGE).tgz $(SML_MODE_PACKAGE).zip

# Netscape workaround:
$(PREFIX)/bg.gif: ../manual/bg.gif
	install -c -m 444 $< $@

$(PREFIX)/download/COPYING: ../../COPYING
	install -c -m 444 $< $@

$(PREFIX)/download/CHANGES: ../versions/changes-release-seam.txt
	install -c -m 444 $< $@

$(PREFIX)/download/%-devel.tgz:
	(cd /opt && tar -chzf $@ $*-devel)

$(PREFIX)/download/%-mode.tgz:
	(cd /usr/share/emacs/site-lisp && tar -chzf $@ sml-*)
