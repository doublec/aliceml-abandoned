SOURCES = GecodeTest.aml TestSuite.aml RunTests.aml

all: $(SOURCES:%.aml=%.stc)

%.stc: %.aml
	alicec -c $< -o $@

depend:
	alicedep $(SOURCES) > Makefile.depend

-include Makefile.depend
