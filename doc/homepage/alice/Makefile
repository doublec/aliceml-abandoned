PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/download \
	$(PREFIX)/papers \
	$(PREFIX)/manual \
	$(PREFIX)/manual/library \
	$(PREFIX)/manual/examples \
	$(PREFIX)/manual/examples/interpreter

INSTALLFILES_0 = \
	style.css bg.gif alice.gif musicemb.jpg macros.php3 project.html \
	index.html people.html papers.html download.html contact.html download-old.html
INSTALLFILES_DOWNLOAD_0 = \
	stockhausen-devel.tgz mozart-devel.tgz sml-mode.tgz
INSTALLFILES_PAPERS_0 = \
	intermediate.ps
INSTALLFILES_MANUAL_0 = \
	style.css macros.php3 menu.html bg.gif logo-small.gif logo-large.gif \
	index.html features.html \
	language.html futures.html types.html modules.html sugar.html \
	components.html distribution.html \
	packages.html pickling.html constraints.html \
	incompatibilities.html limitations.html \
	tools.html interactive.html compiler.html machine.html \
	linker.html depend.html \
	interop.html examples.html tour.html sitemap.html \
	mozart_cartoon.jpg
INSTALLFILES_MANUAL_LIBRARY_0 = \
	macros.php3 menu.html bg.gif \
	index.html \
	alt.html array.html array-slice.html \
	bin-io.html bool.html byneed.html byte.html \
	char.html command-line.html \
	future.html \
	general.html \
	hole.html \
	ieee-real.html imperative-io.html integer.html io.html \
	list.html list-pair.html lock.html \
	math.html mono-array.html mono-array-slice.html mono-vector.html \
	mono-vector-slice.html \
	option.html os.html os-file-sys.html os-io.html os-process.html \
	package.html pair.html pickle.html prim-io.html promise.html \
	real.html ref.html \
	stream-io.html string.html string-cvt.html substring.html \
	text.html text-io.html text-stream-io.html time.html toplevel.html \
	unique-string.html unix.html \
	vector.html vector-pair.html vector-slice.html \
	word.html \
	thread.html url.html config.html socket.html http.html \
	http-client.html http-server.html resolver-handler.html \
	resolver.html component.html component-manager.html remote.html \
	HttpClientExample.aml HttpServerExample.aml \
        fd.html fs.html space.html search.html linear.html \
	gtk.html \
        inspector.html explorer.html
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

# Netscape workaround:
$(PREFIX)/bg.gif: manual/bg.gif
	install -c -m 444 $< $@
$(PREFIX)/manual/library/bg.gif: manual/bg.gif
	install -c -m 444 $< $@

$(PREFIX)/download/%-devel.tgz:
	(cd /opt && tar -chzf $@ $*-devel)

$(PREFIX)/download/%-mode.tgz:
	(cd /usr/share/emacs/site-lisp && tar -chzf $@ sml-*)
