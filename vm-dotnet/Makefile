SRCS   = Alice.cs
OBJS   = Alice.dll
ILOBJS = xxx.dll

ISRCS  = Win32.cs
IOBJS  = Win32.dll

all: Start.cs $(OBJS) $(ILOBJS)
	coolc -nologo -import:Alice.dll -assembly+ Start.cs

%.dll : %.cs
	coolc -nologo -dll -assembly+ -out:$@ $<

%.dll : %.il
	ilasm -nologo -quit -dll -out=$@ $<

inspector: Inspector.cs $(IOBJS)
	coolc -nologo -assembly+ -import:Win32.dll Inspector.cs

clean:
	rm -rf $(OBJS) $(ILOBJS) $(IOBJS) Start.exe Inspector.exe

distclean: clean
	rm -rf *~ *.pdb

