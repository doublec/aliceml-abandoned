SOURCES = GecodeTest.aml TestSuite.aml RunTests.aml

all: $(SOURCES:%.aml=%.alc)

%.alc: %.aml
	alicec -c $< -o $@

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
