PREFIX = /opt/alice-devel/

BINDIR = $(PREFIX)/bin

MANPAGES = alice.1 alicec.1 alicelink.1 alicerun.1 alicedep.1

INSTALLFILES = $(MANPAGES:%=$(PREFIX)/man/%) \

.PHONY: install all clean veryclean distclean

all: $(MANPAGES)

install: $(PREFIX)/man $(INSTALLFILES)

alicerun.1: alicerun
	help2man -I see_also -N ./$< -o $@

%.1: $(BINDIR)/%
	help2man -I see_also -N $< -o $@

$(PREFIX)/man:
	mkdir -p $@

$(PREFIX)/man/%.1: %.1
	install -c -m 444 $< $@

clean:
	rm -f $(MANPAGES)

veryclean: clean
distclean: veryclean
