all:
	alicec TESTSUITE-sig.aml -o TESTSUITE-sig.alc
	(cd fundamental; make)

clean:
	rm TESTSUITE-sig.alc
	(cd fundamental; make clean)

depend:
	(cd fundamental; make depend)
