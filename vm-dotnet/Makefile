BOOTSTRAPDIR = ../bootstrap

ALICEC0 = sml @SMLload=$(BOOTSTRAPDIR)/alicec-stockwerk

OBJS = \
	Alice.dll stow.exe Base.dll \
	IO.dll OS_FileSys.dll OS_Process.dll OS.dll TextIO.dll CommandLine.dll
SIGS = IO.asig OS_FileSys.asig OS_Process.asig TextIO.asig CommandLine.asig
SRCS = OS_FILE_SYS-sig.aml OS_PROCESS-sig.aml OS-sig.aml OS.aml TEXT_IO-sig.aml
INSPECTOR_OBJS = Win32.dll Canvas.dll Dialog.dll Tools.dll

all: $(OBJS) $(SIGS) $(SRCS) $(INSPECTOR_OBJS)

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

Base.dll: Base.aml Skeleton.il
	$(ALICEC0) --implicit-import-file - Base.aml -o $@

IO.dll: IO.cs
	csc -nologo -t:library IO.cs

OS_FileSys.dll: OS_FileSys.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS_FileSys.cs

OS_Process.dll: OS_Process.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll OS_Process.cs

OS.dll: OS.aml OS_Process.asig OS_FileSys.asig \
	OS_PROCESS-sig.aml OS_FILE_SYS-sig.aml OS-sig.aml
	$(ALICEC0) OS.aml -o $@

TextIO.dll: TextIO.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll TextIO.cs

CommandLine.dll: CommandLine.cs Alice.dll
	csc -nologo -t:library -r:Alice.dll CommandLine.cs

IO.asig: ../lib/system/IO.asig
	cp $< $@

OS_FileSys.asig: ../lib/system/OS_FileSys.asig
	cp $< $@

OS_Process.asig: ../lib/system/OS_Process.asig
	cp $< $@

TextIO.asig: ../lib/system/TextIO.asig
	cp $< $@

CommandLine.asig: ../lib/system/CommandLine.asig
	cp $< $@

OS_FILE_SYS-sig.aml: ../lib/system/OS_FILE_SYS-sig.aml
	cp $< $@

OS_PROCESS-sig.aml: ../lib/system/OS_PROCESS-sig.aml
	cp $< $@

OS-sig.aml: ../lib/system/OS-sig.aml
	cp $< $@

OS.aml: ../lib/system/OS.aml
	cp $< $@

TEXT_IO-sig.aml: ../lib/system/TEXT_IO-sig.aml
	cp $< $@

Win32.dll: Win32.cs
	csc -nologo -t:library Win32.cs

Canvas.dll: Canvas.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Canvas.cs

Dialog.dll: Dialog.cs Win32.dll Alice.dll
	csc -nologo -t:library -r:Win32.dll -r:Alice.dll Dialog.cs

Tools.dll: Tools.aml Skeleton.il Base.asig
	$(ALICEC0) Tools.aml -o $@

clean:
	rm -rf *.pdb

veryclean: clean
	rm -rf HashCode.exe stow.il Skeleton.il
	rm -rf Base.dll.il Tools.dll.il
	rm -f $(OBJS) $(SIGS) $(SRCS) $(INSPECTOR_OBJS)
	rm -rf stow.exe alicec-dotnet.x86-win32

distclean: veryclean
