PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/download \
	$(PREFIX)/papers \
	$(PREFIX)/manual \
	$(PREFIX)/manual/library \
	$(PREFIX)/manual/examples \
	$(PREFIX)/manual/examples/interpreter

INSTALLFILES_0 = \
	style.css bg.gif alice.gif musicemb.jpg macros.php3 \
	index.php3 people.php3 papers.php3 download.php3
INSTALLFILES_DOWNLOAD_0 = \
	stockhausen-devel.tgz mozart-devel.tgz sml-mode.tgz
INSTALLFILES_PAPERS_0 = \
	intermediate.ps opaque.ps
INSTALLFILES_MANUAL_0 = \
	bg.gif alice.gif style.css \
	index.php3 macros.php3 menu.php3 laziness.php3 \
	futures.php3 threads.php3 components.php3 modules.php3 \
	packages.php3 pickling.php3 constraints.php3 inspector.php3 gtk.php3 \
	extensions.php3 incompatibilities.php3 usage.php3 libraries.php3 \
	explorer.php3 interop.php3 examples.php3 \
	mozart_cartoon.jpg
INSTALLFILES_MANUAL_LIBRARY_0 = \
	style.css macros.php3 menu.php3 \
	alt.php3 bool.php3 general.php3 option.php3 pair.php3 ref.php3
INSTALLFILES_MANUAL_EXAMPLES_0 = \
	streams.aml smolka.aml interpreter.tar
INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0 = \
	Token.aml Syntax.aml LEXER-sig.aml Lexer.aml PARSER-sig.aml \
	Parser.aml ENV-sig.aml Env.aml TYPE-sig.aml Type.aml VALUE-sig.aml \
	Value.aml ELAB-sig.aml Elab.aml EVAL-sig.aml Eval.aml Main.aml README
INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER = \
	$(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/manual/examples/interpreter/%)
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%) \
	$(INSTALLFILES_PAPERS_0:%=$(PREFIX)/papers/%) \
	$(INSTALLFILES_MANUAL_0:%=$(PREFIX)/manual/%) \
	$(INSTALLFILES_MANUAL_LIBRARY_0:%=$(PREFIX)/manual/library/%) \
	$(INSTALLFILES_MANUAL_EXAMPLES_0:%=$(PREFIX)/manual/examples/%) \
	$(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/manual/examples/interpreter/%)

DOWNLOADFILES = $(INSTALLFILES_DOWNLOAD_0:%=$(PREFIX)/download/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)
download: $(DOWNLOADFILES)

manual/examples/interpreter.tar: \
	$(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER)
	(cd manual/examples && tar -cvf interpreter.tar \
	 $(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0:%=interpreter/%))

$(DIRS):
	mkdir -p $@

$(PREFIX)/%: %
	install -c -m 444 $< $@

$(PREFIX)/alice.gif: \
	../../../misc/logo/alice-strikethrough-shadow-blue-254x80.gif
	install -c -m 444 $< $@

$(PREFIX)/manual/alice.gif: \
	../../../misc/logo/alice-strikethrough-shadow-blue-190x60.gif
	install -c -m 444 $< $@

$(PREFIX)/download/%-devel.tgz:
	(cd /opt && tar -chzf $@ $*-devel)

$(PREFIX)/download/%-mode.tgz:
	(cd /usr/share/emacs/site-lisp && tar -chzf $@ sml-*)
