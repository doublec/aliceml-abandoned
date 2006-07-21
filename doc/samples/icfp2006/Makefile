ALICEC = alicec

FILES0 = store code decoder disassemble
FILES = ${FILES0:%=%.alc}

.PHONY:	all
all: ${FILES}

.PHONY:	clean
clean:
	rm ${FILES}

%.alc: %.sml
	$(ALICEC) -c $<
