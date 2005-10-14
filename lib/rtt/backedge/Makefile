TARGETS = test_it
ALICEC = alicec
ALICEDEP = alicedep
RM = rm -f

.PHONY: all clean wclean

all: $(TARGETS:%=%.alc)

%.alc: %.aml
	$(ALICEC) $< -o $@

clean:
	$(RM) *~
	$(RM) *.aml?*
	$(RM) Makefile.depend

wclean: clean
	$(RM) *.alc

Makefile.depend:
	$(ALICEDEP) *.aml > $@

-include Makefile.depend