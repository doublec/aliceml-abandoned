.SUFFIXES: .il .dll

.il.dll:
	ilasm /dll $<
	lm -a $*.dll -c .

all: StockWerk.dll

verify: StockWerk.dll
	peverify StockWerk.dll

clean:
	@-del StockWerk.dll
