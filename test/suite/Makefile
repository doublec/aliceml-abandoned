all:
	(cd src; make)
	(cd lib; make)
	alicec Main.aml -o Main.alc

clean:
	(cd src; make clean)
	(cd lib; make clean)
	rm Main.alc

depend:
	(cd src; make depend)
	(cd lib; make depend)