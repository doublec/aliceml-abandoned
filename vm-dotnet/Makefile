.SUFFIXES: .il .exe .dll

.il.exe:
	ilasm $<

.il.dll:
	ilasm /dll $<

all: StockWerk.dll Test.exe

verify: StockWerk.dll
	peverify StockWerk.dll

clean:
	@-del StockWerk.dll
	@-del Test.exe
