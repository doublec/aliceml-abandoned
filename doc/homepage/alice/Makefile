PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/download \
	$(PREFIX)/papers \
	$(PREFIX)/manual \
	$(PREFIX)/manual/library \
	$(PREFIX)/manual/examples \
	$(PREFIX)/manual/examples/interpreter

INSTALLFILES_0 = \
	style.css bg.gif alice.gif musicemb.jpg macros.php3 project.php3 \
	index.php3 people.php3 papers.php3 download.php3
INSTALLFILES_DOWNLOAD_0 = \
	stockhausen-devel.tgz mozart-devel.tgz sml-mode.tgz
INSTALLFILES_PAPERS_0 = \
	intermediate.ps opaque.ps
INSTALLFILES_MANUAL_0 = \
	style.css macros.php3 menu.php3 bg.gif logo-small.gif logo-large.gif \
	index.php3 features.php3 \
	language.php3 futures.php3 types.php3 modules.php3 sugar.php3 \
	threads.php3 components.php3 \
	packages.php3 pickling.php3 constraints.php3 gtk.php3 \
	incompatibilities.php3 usage.php3 \
	explorer.php3 interop.php3 examples.php3 tour.php3 \
	mozart_cartoon.jpg
INSTALLFILES_MANUAL_LIBRARY_0 = \
	macros.php3 menu.php3 bg.gif \
	index.php3 \
	alt.php3 array.php3 array-slice.php3 \
	bin-io.php3 bool.php3 byneed.php3 byte.php3 \
	char.php3 command-line.php3 \
	future.php3 \
	general.php3 \
	hole.php3 \
	ieee-real.php3 imperative-io.php3 integer.php3 io.php3 \
	list.php3 list-pair.php3 lock.php3 \
	math.php3 mono-array.php3 mono-array-slice.php3 mono-vector.php3 \
	mono-vector-slice.php3 \
	option.php3 os.php3 os-file-sys.php3 os-io.php3 os-process.php3 \
	package.php3 pair.php3 pickle.php3 prim-io.php3 promise.php3 \
	real.php3 ref.php3 \
	stream-io.php3 string.php3 string-cvt.php3 substring.php3 \
	text.php3 text-io.php3 text-stream-io.php3 time.php3 toplevel.php3 \
	unique-string.php3 unix.php3 \
	vector.php3 vector-pair.php3 vector-slice.php3 \
	word.php3 \
	thread.php3 url.php3 config.php3 socket.php3 http.php3 \
	http-client.php3 http-server.php3 resolver-handler.php3 \
	resolver.php3 component.php3 component-manager.php3 remote.php3 \
	HttpClientExample.aml HttpServerExample.aml \
        fd.php3 fs.php3 space.php3 search.php3 linear.php3 \
        inspector.php3 explorer.php3
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
