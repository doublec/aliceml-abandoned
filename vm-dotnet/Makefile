OBJS   = Alice.dll
IOBJS  = Win32.dll Canvas.dll Dialog.dll Helper.dll

.il.dll:
	ilasm -nologo -quiet -dll -out=$@ $<

all: stow.exe $(OBJS) inspector

inspector: $(IOBJS)

Alice.dll: Alice.cs
	csc -nologo -t:library Alice.cs

Win32.dll: Alice.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Helper.dll: Helper.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll Helper.cs

stow.exe: stow.cs Alice.dll
	csc -nologo -t:exe -r:Alice.dll stow.cs

clean:
	@-del /f /q *~ *.pdb

veryclean: clean

distclean: veryclean
	@-del /f /q $(OBJS) $(IOBJS) stow.exe
