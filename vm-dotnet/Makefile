SRCS   = Alice.cs
OBJS   = Alice.dll
ILOBJS = foo.dll

all: Start.cs $(OBJS) $(ILOBJS)
	coolc -nologo -import:Alice.dll -assembly+ Start.cs

%.dll : %.cs
	coolc -nologo -dll -assembly+ -out:$@ $<

%.dll : %.il
	ilasm -nologo -quit -dll -out=$@ $<

clean:
	rm -rf $(OBJS) $(ILOBJS) Start.exe

distclean: clean
	rm -rf *~ *.pdb

