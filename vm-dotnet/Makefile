SRCS   = Alice.cs
OBJS   = Alice.dll

ISRCS  = Win32.cs
IOBJS  = Win32.dll

ILOBJS = Alice.il stow.il

all: stow.cs $(OBJS)
	coolc -nologo -import:Alice.dll -assembly+ stow.cs
	ildasm -text -tok -byt -out=stow.il stow.exe

%.dll : %.cs
	coolc -nologo -dll -assembly+ -out:$@ $<

%.dll : %.il
	ilasm -nologo -quit -dll -out=$@ $<

inspector: Canvas.cs $(IOBJS)
	coolc -nologo -dll -assembly+ -import:Win32.dll -import:Alice.dll Canvas.cs

clean:
	rm -rf $(OBJS) $(ILOBJS) stow.exe

distclean: clean
	rm -rf Win32.dll Canvas.dll *~ *.pdb
