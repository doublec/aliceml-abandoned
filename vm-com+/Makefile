.SUFFIXES: .il .exe

.il.exe:
	ilasm $<

all: StockWerk.exe

verify: StockWerk.exe
	peverify StockWerk.exe

clean:
	@-del StockWerk.exe
