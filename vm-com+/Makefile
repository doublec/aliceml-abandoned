.SUFFIXES: .il .exe

.il.exe:
	ilasm $<

all: Alice.dll
	coolc /dll Alice.cs

clean:
	@-del Alice.dll

