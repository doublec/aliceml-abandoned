PHP = php

ifeq ($(TARGET), mozart)
INTEROP = interop.html migration.html
CONSTRAINTS = constraints_mozart.html
PLATFORM_LIB = fd.html fs.html space.html search.html \
	linear.html explorer.html \
	gtk.html gtk-canvas.html gtk-overview.html gdk.html
PLATFORM = mozart
else
INTEROP =
CONSTRAINTS = constraints_gecode.html
PLATFORM_LIB = \
	fd.html fs.html space.html search.html \
	modeling.html explorer.html \
	glib.html pango.html atk.html \
	gtk.html gtk-canvas.html gtk-overview.html gdk.html \
        sf-search.html sf-path.html sf-problem.html sf-debug.html sf-distributed.html
PLATFORM = seam
endif

PREFIX = /opt/alice-devel/doc

DIRS = $(PREFIX) \
	$(PREFIX)/library \
	$(PREFIX)/library/constraints \
	$(PREFIX)/examples \
	$(PREFIX)/examples/interpreter \
	$(PREFIX)/cptutorial

INSTALLFILES_0 = \
	style.css macros.php3 menu.html bg.gif logo-small.gif logo-large.gif \
	index.html features.html \
	language.html futures.html types.html modules.html sugar.html \
	components.html distribution.html \
	packages.html pickling.html constraints.html \
	incompatibilities.html limitations.html \
	tools.html interactive.html compiler.html machine.html glade.html \
	linker.html depend.html \
	$(INTEROP) tour.html sitemap.html \
	mozart_cartoon.jpg \
	inspector-intro-1.png inspector-intro-2.png inspector-intro-3.png \
	inspector-intro-4.png \
	inspector-lazy-1.png \
	inspector-conc-1.png inspector-conc-2.png inspector-conc-3.png \
	inspector-conc-4.png \
	inspector-prom-1.png inspector-prom-2.png inspector-prom-3.png \
	inspector-prom-4.png \
	inspector-pack-1.png inspector-pack-2.png \
	inspector-fd-1.jpg inspector-fd-2.jpg inspector-fd-3.jpg \
	inspector-fd-4.jpg inspector-fd-5.jpg inspector-fd-6.jpg \
	inspector-search-1.png \
	Alice.hhc Alice.hhp \
	explorer.png
INSTALLFILES_LIBRARY_0 = \
	macros.php3 menu.html bg.gif \
	index.html \
	alt.html array.html array2.html array-slice.html atom.html regex.html \
	bin-io.html bool.html byte.html \
	channel.html char.html command-line.html \
	date.html \
	exn.html \
	fn.html future.html \
	general.html \
	hashable.html hole.html \
	ieee-real.html imp-map.html imp-set.html imperative-io.html \
	integer.html int-inf.html io.html \
	list.html list-pair.html lock.html \
	map.html math.html mono-array.html \
	mono-array-slice.html \
	mono-vector.html mono-vector-slice.html \
	option.html ordered.html os.html os-file-sys.html os-io.html \
	os-path.html os-process.html \
	package.html pair.html pickle.html prim-io.html print.html \
	promise.html \
	real.html ref.html ref-map.html \
	set.html sited.html sqlite.html stack.html stamp.html store.html \
	stream-io.html \
	string.html string-cvt.html substring.html \
	text.html text-io.html text-stream-io.html time.html timer.html \
	toplevel.html \
	unix.html \
	vector.html vector-pair.html vector-slice.html \
	word.html xml.html \
	thread.html url.html config.html socket.html http.html \
	http-client.html http-server.html resolver-handler.html \
	resolver.html component.html component-manager.html remote.html \
	compiler.html \
	HttpClientExample.aml HttpServerExample.aml \
        $(PLATFORM_LIB) \
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

.PHONY: install clean cptutorial

all: install

install: $(DIRS) $(INSTALLFILES) cptutorial
	rm -f constraints.html
	rm -f $(PLATFORM_LIB:%=library/%)

cptutorial:
	(cd cptutorial && \
	 make all && \
	 install -d $(PREFIX)/cptutorial/samples && \
	 find html/samples -maxdepth 1 -type f -exec \
	    install -c -m 444 "{}" $(PREFIX)/cptutorial/samples ";" && \
	 find html -maxdepth 1 -type f -exec \
	    install -c -m 444 "{}" $(PREFIX)/cptutorial ";")

clean:
	rm -f constraints.html
	rm -f $(PLATFORM_LIB:%=library/%)

constraints.html: $(CONSTRAINTS)
	cp $< $@

examples/interpreter.tar: \
	$(INSTALLFILES_EXAMPLES_INTERPRETER)
	(cd examples && tar -cvf interpreter.tar \
	 $(INSTALLFILES_EXAMPLES_INTERPRETER_0:%=interpreter/%))

$(PLATFORM_LIB:%=library/%): library/% : library/$(PLATFORM)/%
	cp $< $@

$(DIRS):
	mkdir -p -m 775 $@

$(PREFIX)/%.html: %.html macros.php3 menu.html
	(echo '<?php $$platform = "$(PLATFORM)"; ?>'; cat $<) > $<.static.in
	(cd $(dir $<); $(PHP) -f $(notdir $<).static.in > $(notdir $<).static)
	install -c -m 444 $<.static $@
	rm -f $<.static.in $<.static

$(PREFIX)/%: %
	install -c -m 444 $< $@

# Netscape workaround:
$(PREFIX)/library/bg.gif: bg.gif
	install -c -m 444 $< $@
