PREFIX = /home/ps/httpd/html/stockhausen

DIRS = $(PREFIX) $(PREFIX)/operette1 $(PREFIX)/operette1/examples \
	$(PREFIX)/operette1/examples/interpreter

INSTALLFILES_0 = \
	bg.gif mozart_cartoon.jpg musicemb.jpg stockhausen.gif \
	stockhausen.jpg stockhausen.ps
INSTALLFILES_OPERETTE1_0 = \
	style.css index.php3 macros.php3 menu.php3 laziness.php3 futures.php3 \
	threads.php3 components.php3 modules.php3 extensions.php3 \
	incompatibilities.php3 usage.php3 libraries.php3 examples.php3
INSTALLFILES_OPERETTE1_EXAMPLES_0 = \
	streamadd.aml smolka.aml interpreter.tar
INSTALLFILES_OPERETTE1_EXAMPLES_INTERPRETER_0 = \
	Token.aml Syntax.aml LEXER.aml Lexer.aml PARSER.aml Parser.aml \
	ENV.aml Env.aml TYPE.aml Type.aml VALUE.aml Value.aml \
	ELAB.aml Elab.aml EVAL.aml Eval.aml \
	Main.aml README
INSTALLFILES = \
	$(INSTALLFILES_0:%=$(PREFIX)/%) \
	$(INSTALLFILES_OPERETTE1_0:%=$(PREFIX)/operette1/%) \
	$(INSTALLFILES_OPERETTE1_EXAMPLES_0:%=$(PREFIX)/operette1/examples/%) \
	$(INSTALLFILES_OPERETTE1_EXAMPLES_INTERRETER_0:%=$(PREFIX)/operette1/examples/interpreter/%)

.PHONY: install

all: install

install: $(DIRS) $(INSTALLFILES)

$(DIRS):
	mkdir -p $@

$(PREFIX)/%: %
	install -c -m 444 $< $@
