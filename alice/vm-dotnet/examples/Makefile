OBJS = \
	concfib.dll hamming.dll hello.dll infer.dll inflist.dll streams.dll \
	Fib_Alice_Component.dll Fib_Alice.dll Test.exe

all: $(OBJS)

Fib_Alice_Component.dll: Fib_Alice_Component.aml
	..\stoc --no-dump-elaboration-sig -c Fib_Alice_Component.aml -o $@

Fib_Alice.dll: Fib_Alice.cs
	csc -nologo -t:library -r:../Alice.dll -r:System.dll Fib_Alice.cs

Test.exe: Test.cs
	csc -nologo -t:exe -r:Fib_Alice.dll Test.cs

concfib.dll: concfib.aml
	..\stoc --no-dump-elaboration-sig -c concfib.aml -o $@

hamming.dll: hamming.aml
	..\stoc --no-dump-elaboration-sig -c hamming.aml -o $@

hello.dll: hello.aml
	..\stoc --no-dump-elaboration-sig -c hello.aml -o $@

infer.dll: infer.aml
	..\stoc --no-dump-elaboration-sig -c infer.aml -o $@

inflist.dll: inflist.aml
	..\stoc --no-dump-elaboration-sig -c inflist.aml -o $@

streams.dll: streams.aml
	..\stoc --no-dump-elaboration-sig -c streams.aml -o $@

clean:
	@-del /F /Q *.pdb

veryclean: clean
	@-del /F /Q concfib.dll.il concfib.dll
	@-del /F /Q hamming.dll.il hamming.dll
	@-del /F /Q hello.dll.il hello.dll
	@-del /F /Q infer.dll.il infer.dll
	@-del /F /Q inflist.dll.il inflist.dll
	@-del /F /Q streams.dll.il streams.dll

distclean: veryclean
