OBJS = Alice.dll Base.dll IO.dll OS.dll TextIO.dll CommandLine.dll
SIGS = IO.dll.sig OS.dll.sig TextIO.dll.sig CommandLine.dll.sig
INSPECTOR_OBJS = Win32.dll Canvas.dll Dialog.dll Helper.dll Tools.dll

.il.dll:
	ilasm -nologo -quiet -dll -out=$@ $<

all: stoc-com+.x86-win32 $(OBJS) $(SIGS) stow.exe inspector

Alice.dll: Alice.cs
	csc -nologo -t:library Alice.cs

stoc-com+.x86-win32: ../stoc/stoc-com+.x86-win32
	copy "..\stoc\stoc-com+.x86-win32" "stoc-com+.x86-win32"

Base.aml: ../lib/bootstrap/Bootstrap.aus
	copy ..\lib\bootstrap\Bootstrap.aus Base.aml

Base.dll: Base.aml stoc-com+.x86-win32
	stoc --noimplicitimport -c ../lib/bootstrap/Bootstrap.aus -o Base.il
	ilasm /dll Base.il

IO.dll: IO.cs
	csc -nologo -t:library IO.cs

OS.dll: OS.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS.cs

TextIO.dll: TextIO.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll TextIO.cs

CommandLine.dll: CommandLine.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll CommandLine.cs

IO.dll.sig: ../lib/bootstrap/IO.sig
	copy ..\lib\bootstrap\IO.sig IO.dll.sig

OS.dll.sig: ../lib/bootstrap/OS.sig
	copy ..\lib\bootstrap\OS.sig OS.dll.sig

TextIO.dll.sig: ../lib/bootstrap/TextIO.sig
	copy ..\lib\bootstrap\TextIO.sig TextIO.dll.sig

CommandLine.dll.sig: ../lib/bootstrap/CommandLine.sig
	copy ..\lib\bootstrap\CommandLine.sig CommandLine.dll.sig

stow.exe: stow.cs Alice.dll
	csc -nologo -t:exe -r:Alice.dll stow.cs

inspector: $(INSPECTOR_OBJS)

Win32.dll: Alice.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Helper.dll: Helper.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll Helper.cs

Tools.dll: Tools.aml stoc-com+.x86-win32
	stoc -c Tools.aml -o Tools.il
	ilasm /dll Tools.il

clean:
	@-del /f /q *~ *.pdb

veryclean: clean

distclean: veryclean
	@-del /f /q Base.aml $(OBJS) $(INSPECTOR_OBJS) stow.exe
