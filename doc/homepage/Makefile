PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/download \
	$(PREFIX)/papers

INSTALLFILES_0 = \
	style.css bg.gif alice.gif macros.php3 project.html \
	index.html people.html papers.html download.html contact.html
INSTALLFILES_DOWNLOAD_0 = \
	COPYING CHANGES
#	stockhausen-devel.tgz mozart-devel.tgz sml-mode.tgz
INSTALLFILES_PAPERS_0 = \
	intermediate.ps
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%) \
	$(INSTALLFILES_PAPERS_0:%=$(PREFIX)/papers/%)

DOWNLOADFILES = $(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)
	(cd ../manual && make PREFIX=$(PREFIX)/manual-devel)

release: install download
	(cd ../manual && make PREFIX=$(PREFIX)/manual)

download: $(DOWNLOADFILES)

$(DIRS):
	mkdir -p $@

$(PREFIX)/%.html: %.html macros.php3
	php -q $< >$<.static
	install -c -m 444 $<.static $@
	rm -f $<.static

$(PREFIX)/%: %
	install -c -m 444 $< $@

$(PREFIX)/alice.gif: \
	../../misc/logo/alice-strikethrough-shadow-blue-254x80.gif
	install -c -m 444 $< $@

# Netscape workaround:
$(PREFIX)/bg.gif: ../manual/bg.gif
	install -c -m 444 $< $@

$(PREFIX)/download/COPYING: ../../COPYING
	install -c -m 444 $< $@

$(PREFIX)/download/CHANGES: ../versions/changes-devel-mozart.txt
	install -c -m 444 $< $@

$(PREFIX)/download/%-devel.tgz:
	(cd /opt && tar -chzf $@ $*-devel)

$(PREFIX)/download/%-mode.tgz:
	(cd /usr/share/emacs/site-lisp && tar -chzf $@ sml-*)
