PREFIX = /services/ps/httpd/html/stockhausen

DIRS = $(PREFIX) \
	$(PREFIX)/operette2 $(PREFIX)/operette2/examples \
	$(PREFIX)/operette2/examples/interpreter \
	$(PREFIX)/lightning $(PREFIX)/lightning/examples

INSTALLFILES_0 = \
	bg.gif mozart_cartoon.jpg musicemb.jpg stockhausen.gif \
	stockhausen.jpg stockhausen.ps index.html
INSTALLFILES_OPERETTE2_0 = \
	bg.gif style.css index.php3 macros.php3 menu.php3 laziness.php3 \
	futures.php3 threads.php3 components.php3 modules.php3 \
	extensions.php3 incompatibilities.php3 usage.php3 libraries.php3 \
	interop.php3 examples.php3
INSTALLFILES_OPERETTE2_EXAMPLES_0 = \
	streams.aml smolka.aml interpreter.tar
INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER_0 = \
	Token.aml Syntax.aml LEXER-sig.aml Lexer.aml PARSER-sig.aml \
	Parser.aml ENV-sig.aml Env.aml TYPE-sig.aml Type.aml VALUE-sig.aml \
	Value.aml ELAB-sig.aml Elab.aml EVAL-sig.aml Eval.aml Main.aml README
INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER = \
	$(INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/operette2/examples/interpreter/%)
INSTALLFILES_LIGHTNING_0 = \
	style.css index.php3 macros.php3 menu.php3 laziness.php3 futures.php3 \
	threads.php3 components.php3 modules.php3 extensions.php3 \
	incompatibilities.php3 usage.php3 libraries.php3 interop.php3 \
	examples.php3
INSTALLFILES_LIGHTNING_EXAMPLES_0 = \
	concfib.aml hamming.aml infer.aml inflist.aml nrev.aml \
	streams.aml smolka.aml
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_OPERETTE2_0:%=$(PREFIX)/operette2/%) \
	$(INSTALLFILES_OPERETTE2_EXAMPLES_0:%=$(PREFIX)/operette2/examples/%) \
	$(INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER_0:%=$(PREFIX)/operette2/examples/interpreter/%) \
	$(INSTALLFILES_LIGHTNING_0:%=$(PREFIX)/lightning/%) \
	$(INSTALLFILES_LIGHTNING_EXAMPLES_0:%=$(PREFIX)/lightning/examples/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)

operette2/examples/interpreter.tar: \
	$(INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER)
	(cd operette2/examples && tar cvf interpreter.tar \
	 $(INSTALLFILES_OPERETTE2_EXAMPLES_INTERPRETER_0:%=interpreter/%))

$(DIRS):
	mkdir -p $@

$(PREFIX)/%: %
	install -c -m 444 $< $@
