PREFIX = /services/ps/httpd/html/alice

DIRS = $(PREFIX) \
	$(PREFIX)/manual \
	$(PREFIX)/manual/examples \
	$(PREFIX)/manual/examples/interpreter

INSTALLFILES_0 = \
	style.css bg.gif musicemb.jpg macros.php3 \
	index.php3 people.php3 papers.php3 download.php3
INSTALLFILES_MANUAL_0 = \
	bg.gif style.css index.php3 macros.php3 menu.php3 laziness.php3 \
	futures.php3 threads.php3 components.php3 modules.php3 \
	packages.php3 pickling.php3 constraints.php3 inspector.php3 gtk.php3 \
	extensions.php3 incompatibilities.php3 usage.php3 libraries.php3 \
	explorer.php3 interop.php3 examples.php3 \
	mozart_cartoon.jpg
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
	$(INSTALLFILES_MANUAL_0:%=$(PREFIX)/manual/%) \
	$(INSTALLFILES_MANUAL_EXAMPLES_0:%=$(PREFIX)/manual/examples/%) \
	$(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/manual/examples/interpreter/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)

manual/examples/interpreter.tar: \
	$(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER)
	(cd manual/examples && tar cvf interpreter.tar \
	 $(INSTALLFILES_MANUAL_EXAMPLES_INTERPRETER_0:%=interpreter/%))

$(DIRS):
	mkdir -p $@

$(PREFIX)/%: %
	install -c -m 444 $< $@
