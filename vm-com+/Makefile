OBJS = Alice.dll stow.exe Base.dll IO.dll OS.dll TextIO.dll CommandLine.dll
SIGS = IO.dll.sig OS.dll.sig TextIO.dll.sig CommandLine.dll.sig
INSPECTOR_OBJS = Win32.dll Canvas.dll Dialog.dll Tools.dll

all: stoc-com+.x86-win32 $(OBJS) $(SIGS) inspector streams.dll

stoc-com+.x86-win32: ../stoc/stoc-com+.x86-win32
	copy "..\stoc\stoc-com+.x86-win32" "stoc-com+.x86-win32"

Alice.dll: Alice.cs
	csc -nologo -t:library Alice.cs

stow.exe: stow.cs Alice.dll
	csc -nologo -t:exe -r:Alice.dll stow.cs

stow.il: stow.exe
	ildasm /nobar /text /out=stow.il stow.exe

HashCode.exe: HashCode.cs
	csc -nologo -t:exe HashCode.cs

Skeleton.il: HashCode.exe stow.il
	HashCode stow.il Skeleton.il

Base.aml: ../lib/bootstrap/Bootstrap.aus
	copy ..\lib\bootstrap\Bootstrap.aus Base.aml

Base.dll: Base.aml stoc-com+.x86-win32 Skeleton.il
	stoc --noimplicitimport -c Base.aml -o $@

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

inspector: $(INSPECTOR_OBJS)

Win32.dll: Win32.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Tools.dll: Tools.aml stoc-com+.x86-win32 Skeleton.il Base.dll.sig
	stoc -c Tools.aml -o $@

streams.dll: examples/streams.aml stoc-com+.x86-win32 Skeleton.il Base.dll.sig
	stoc -c examples/streams.aml -o $@

clean:
	@-del /f /q *~ *.pdb

veryclean: clean

distclean: veryclean
	@-del /f /q Base.aml $(OBJS) $(INSPECTOR_OBJS) stow.exe
