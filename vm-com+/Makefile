.SUFFIXES: .il .exe .dll

.il.exe:
	ilasm $<

.il.dll:
	ilasm /dll $<
	lm -a $*.dll -c .

all: StockWerk.dll Test.exe

verify: StockWerk.dll
	peverify StockWerk.dll

clean:
	@-del StockWerk.dll
	@-del Test.exe
