.SUFFIXES: .il .dll

.il.dll:
	ilasm /dll $<

all: StockWerk.dll

verify: StockWerk.dll
	peverify StockWerk.dll

clean:
	@-del StockWerk.dll
