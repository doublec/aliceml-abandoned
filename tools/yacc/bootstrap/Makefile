TARGETS=LrTable.alc LrParser.alc


all:	$(TARGETS)


%.alc: %.aml
	alicec -c $< -o $@



clean: 
	rm -f $(TARGETS)


depend:
	alicedep *.aml > Makefile.depend


-include Makefile.depend
