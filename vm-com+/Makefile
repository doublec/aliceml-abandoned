SRCS   = Alice.cs
OBJS   = Alice.dll

ISRCS  = Win32.cs
IOBJS  = Win32.dll

ILOBJS = Alice.il stow.il

all: stow.cs $(OBJS)
	cl -nologo -LD io.c -link -export:readline
	csc -nologo -r:Alice.dll -t:exe stow.cs
	ildasm -text -tok -byt -out=stow.il stow.exe

%.dll : %.cs
	csc -nologo -t:library -out:$@ $<

%.dll : %.il
	ilasm -nologo -quit -dll -out=$@ $<

inspector: Canvas.cs Dialog.cs Helper.cs $(IOBJS)
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs
	csc -nologo -t:library -r;Win32.dll -r:Alice.dll Dialog.cs
	csc -nologo -t:library -r:Alice.dll Helper.cs

clean:
	rm -rf $(OBJS) $(ILOBJS) stow.exe

distclean: clean
	rm -rf Win32.dll Canvas.dll *~ *.pdb

