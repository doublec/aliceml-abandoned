OBJS = \
	Alice.dll stow.exe Base.dll \
	IO.dll OS_FileSys.dll OS_Process.dll OS.dll TextIO.dll CommandLine.dll
SIGS = IO.asig OS_FileSys.asig OS_Process.asig TextIO.asig CommandLine.asig
SRCS = OS_FILE_SYS-sig.aml OS_PROCESS-sig.aml OS-sig.aml OS.aml TEXT_IO-sig.aml
INSPECTOR_OBJS = Win32.dll Canvas.dll Dialog.dll Tools.dll

all: stoc-com+.x86-win32 $(OBJS) $(SIGS) $(SRCS) $(INSPECTOR_OBJS)

stoc-com+.x86-win32: ../bootstrap/stoc-com+.x86-win32
	copy "..\bootstrap\stoc-com+.x86-win32" "stoc-com+.x86-win32"

Alice.dll: Alice.cs
	csc -nologo -t:library -r:System.dll Alice.cs

stow.exe: stow.cs Alice.dll
	csc -nologo -t:exe -r:Alice.dll -r:System.dll stow.cs

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

OS_FileSys.dll: OS_FileSys.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS_FileSys.cs

OS_Process.dll: OS_Process.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS_Process.cs

OS.dll:
	stoc --no-dump-elaboration-sig -c OS.aml -o $@

TextIO.dll: TextIO.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll TextIO.cs

CommandLine.dll: CommandLine.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll CommandLine.cs

IO.asig: ../lib/system/IO.asig
	copy ..\lib\system\IO.asig IO.asig

OS_FileSys.asig: ../lib/system/OS_FileSys.asig
	copy ..\lib\system\OS_FileSys.asig OS_FileSys.asig

OS_Process.asig: ../lib/system/OS_Process.asig
	copy ..\lib\system\OS_Process.asig OS_Process.asig

TextIO.asig: ../lib/system/TextIO.asig
	copy ..\lib\system\TextIO.asig TextIO.asig

CommandLine.asig: ../lib/system/CommandLine.asig
	copy ..\lib\system\CommandLine.asig CommandLine.asig

OS_FILE_SYS-sig.aml: ../lib/system/OS_FILE_SYS-sig.aml
	copy ..\lib\system\OS_FILE_SYS-sig.aml OS_FILE_SYS-sig.aml

OS_PROCESS-sig.aml: ../lib/system/OS_PROCESS-sig.aml
	copy ..\lib\system\OS_PROCESS-sig.aml OS_PROCESS-sig.aml

OS-sig.aml: ../lib/system/OS-sig.aml
	copy ..\lib\system\OS-sig.aml OS-sig.aml

OS.aml: ../lib/system/OS.aml
	copy ..\lib\system\OS.aml OS.aml

TEXT_IO-sig.aml: ../lib/system/TEXT_IO-sig.aml
	copy ..\lib\system\TEXT_IO-sig.aml TEXT_IO-sig.aml

Win32.dll: Win32.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Tools.dll: Tools.aml stoc-com+.x86-win32 Skeleton.il Base.asig
	stoc --no-dump-elaboration-sig -c Tools.aml -o $@

clean:
	@-del /F /Q *.pdb

veryclean: clean
	@-del /F /Q HashCode.exe stow.il Skeleton.il
	@-del /F /Q Base.dll.il Tools.dll.il
	@-del /F /Q $(OBJS) $(SIGS) $(SRCS) $(INSPECTOR_OBJS)
	@-del /F /Q stow.exe stoc-com+.x86-win32
	@-del /F /Q concfib.dll.il hamming.dll.il infer.dll.il inflist.dll.il
	@-del /F /Q concfib.dll hamming.dll infer.dll inflist.dll

distclean: veryclean
