PHP = php

ifeq ($(SEAM), 1)
INTEROP =
PLATFORM = seam
else
INTEROP = interop.html
PLATFORM = mozart
endif

PREFIX = /opt/alice-devel/doc

DIRS = $(PREFIX) \
	$(PREFIX)/library \
	$(PREFIX)/examples \
	$(PREFIX)/examples/interpreter

INSTALLFILES_0 = \
	style.css macros.php3 menu.html bg.gif logo-small.gif logo-large.gif \
	index.html features.html \
	language.html futures.html types.html modules.html sugar.html \
	components.html distribution.html \
	packages.html pickling.html constraints.html \
	incompatibilities.html limitations.html \
	tools.html interactive.html compiler.html machine.html \
	linker.html depend.html \
	$(INTEROP) migration.html tour.html sitemap.html \
	mozart_cartoon.jpg inspector-1.jpg inspector-2.jpg inspector-3.jpg \
	inspector-conc-1.jpg inspector-conc-2.jpg inspector-conc-3.jpg \
	inspector-conc-4.jpg inspector-conc-5.jpg inspector-conc-6.jpg \
	inspector-fd-1.jpg inspector-fd-2.jpg inspector-fd-3.jpg \
	inspector-fd-4.jpg inspector-fd-5.jpg inspector-fd-6.jpg explorer.jpg
INSTALLFILES_LIBRARY_0 = \
	macros.php3 menu.html bg.gif \
	index.html \
	alt.html array.html array2.html array-slice.html atom.html \
	bin-io.html bool.html byneed.html byte.html \
	char.html command-line.html \
	future.html \
	general.html \
	hashable.html hole.html \
	ieee-real.html imp-map.html imp-set.html imperative-io.html \
	integer.html int-inf.html io.html \
	list.html list-pair.html lock.html \
	map.html math.html mono-array.html mono-array-slice.html \
	mono-vector.html mono-vector-slice.html \
	option.html ordered.html os.html os-file-sys.html os-io.html \
	os-path.html os-process.html \
	package.html pair.html pickle.html prim-io.html print.html \
	promise.html \
	real.html ref.html \
	set.html stack.html stamp.html stream-io.html string.html \
	string-cvt.html substring.html \
	text.html text-io.html text-stream-io.html time.html toplevel.html \
	unix.html \
	vector.html vector-pair.html vector-slice.html \
	word.html \
	thread.html url.html config.html socket.html http.html \
	http-client.html http-server.html resolver-handler.html \
	resolver.html component.html component-manager.html remote.html \
	HttpClientExample.aml HttpServerExample.aml \
        fd.html fs.html space.html search.html linear.html \
	gtk-overview.html gdk.html gtk.html gtk-canvas.html \
        inspector.html explorer.html
INSTALLFILES_EXAMPLES_0 = \
	streams.aml smolka.aml scramble.aml interpreter.tar
INSTALLFILES_EXAMPLES_INTERPRETER_0 = \
	Token.aml Syntax.aml LEXER-sig.aml Lexer.aml PARSER-sig.aml \
	Parser.aml ENV-sig.aml Env.aml TYPE-sig.aml Type.aml VALUE-sig.aml \
	Value.aml ELAB-sig.aml Elab.aml EVAL-sig.aml Eval.aml Main.aml \
	Makefile readme.txt
INSTALLFILES_EXAMPLES_INTERPRETER = \
	$(INSTALLFILES_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/examples/interpreter/%)
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_LIBRARY_0:%=$(PREFIX)/library/%) \
	$(INSTALLFILES_EXAMPLES_0:%=$(PREFIX)/examples/%) \
	$(INSTALLFILES_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/examples/interpreter/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)

examples/interpreter.tar: \
	$(INSTALLFILES_EXAMPLES_INTERPRETER)
	(cd examples && tar -cvf interpreter.tar \
	 $(INSTALLFILES_EXAMPLES_INTERPRETER_0:%=interpreter/%))

$(DIRS):
	mkdir -p -m 775 $@

$(PREFIX)/%.html: %.html macros.php3 menu.html
	(echo '<?php $$platform = "$(PLATFORM)"; ?>'; cat $<) > $<.static.in
	$(PHP) -f $<.static.in > $<.static
	install -c -m 444 $<.static $@
	rm -f $<.static.in $<.static

$(PREFIX)/%: %
	install -c -m 444 $< $@

# Netscape workaround:
$(PREFIX)/library/bg.gif: bg.gif
	install -c -m 444 $< $@
