OBJS = \
	Alice.dll stow.exe Base.dll \
	IO.dll OS_Process.dll TextIO.dll CommandLine.dll
SIGS = IO.asig OS.asig TextIO.asig CommandLine.asig
INSPECTOR_OBJS = Win32.dll Canvas.dll Dialog.dll Tools.dll

all: stoc-com+.x86-win32 $(OBJS) $(SIGS) inspector streams.dll

stoc-com+.x86-win32: ../bootstrap/stoc-com+.x86-win32
	copy "..\bootstrap\stoc-com+.x86-win32" "stoc-com+.x86-win32"

Alice.dll: Alice.cs
	csc -nologo -t:library Alice.cs

stow.exe: stow.cs Alice.dll
	csc -nologo -t:exe -r:Alice.dll stow.cs

stow.il: stow.exe
	ildasm /nobar /out=stow.il stow.exe

HashCode.exe: HashCode.cs
	csc -nologo -t:exe HashCode.cs

Skeleton.il: HashCode.exe stow.il
	HashCode stow.il Skeleton.il

Base.dll: Base.aml stoc-com+.x86-win32 Skeleton.il
	stoc --no-implicit-import --no-dump-elaboration-sig -c Base.aml -o $@

IO.dll: IO.cs
	csc -nologo -t:library IO.cs

OS_Process.dll: OS_Process.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS_Process.cs

TextIO.dll: TextIO.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll TextIO.cs

CommandLine.dll: CommandLine.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll CommandLine.cs

IO.asig: ../lib/system/IO.asig
	copy ..\lib\system\IO.asig IO.asig

OS.asig: ../lib/system/OS.asig
	copy ..\lib\system\OS.asig OS.asig

TextIO.asig: ../lib/system/TextIO.asig
	copy ..\lib\system\TextIO.asig TextIO.asig

CommandLine.asig: ../lib/system/CommandLine.asig
	copy ..\lib\system\CommandLine.asig CommandLine.asig

inspector: $(INSPECTOR_OBJS)

Win32.dll: Win32.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Tools.dll: Tools.aml stoc-com+.x86-win32 Skeleton.il Base.asig
	stoc --no-dump-elaboration-sig -c Tools.aml -o $@

streams.dll: examples/streams.aml stoc-com+.x86-win32 Skeleton.il Base.asig
	stoc --no-dump-elaboration-sig -c examples/streams.aml -o $@

clean:
	@-del /F /Q *.pdb

veryclean: clean

distclean: veryclean
	@-del /F /Q $(OBJS)
	@-del /F /Q $(INSPECTOR_OBJS)
	@-del /F /Q HashCode.exe
	@-del /F /Q Skeleton.il
	@-del /F /Q Base.dll.il Tools.dll.il stow.il streams.dll.il
	@-del /F /Q streams.dll
	@-del /F /Q stow.exe
	@-del /F /Q stoc-com+.x86-win32
	@-del /F /Q concfib.dll.il hamming.dll.il infer.dll.il inflist.dll.il
	@-del /F /Q concfib.dll hamming.dll infer.dll inflist.dll
