FILESTOREMOVE=$(patsubst %.aml.m4, %.aml, $(shell find . -name '*.aml.m4'))


all:
	(cd src; make)
	(cd lib; make)
	alicec Main.aml -o Main.alc

clean:
	(cd src; make clean)
	(cd lib; make clean)
	rm -f ${FILESTOREMOVE}
	rm Main.alc

depend:
	(cd src; make depend)
	(cd lib; make depend)