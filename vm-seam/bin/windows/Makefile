PREFIX=/home/pslab/Projects/Alice-Distribution
BINDIR=$(PREFIX)/bin

CXX = seamtool compile
CXXFLAGS = -O2 -march=pentium3 -mcpu=pentium3

LD = g++ -mno-cygwin
LDFLAGS = -L$(PREFIX)/lib -lseam

EXES = \
	alicerun.exe alicerunwin.exe alice.exe alicec.exe \
	alicelink.exe alicedep.exe
SCRIPTS = $(EXES:%.exe=$(BINDIR)/%)

all: $(EXES)

alicerun.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='--argc>0?(--brokerArgc,*++argv):usage()' \
		-DPRELOAD_GTK=0 \
		-c alicex.cc -o alicerun.o

alice.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Toplevel"' \
		-DPRELOAD_GTK=0 \
		-c alicex.cc -o alice.o

alicec.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/compiler/CompilerMain"' \
		-DPRELOAD_GTK=0 \
		-c alicex.cc -o alicec.o

alicelink.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Linker"' \
		-DPRELOAD_GTK=0 \
		-c alicex.cc -o alicelink.o

alicedep.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Depend"' \
		-DPRELOAD_GTK=0 \
		-c alicex.cc -o alicedep.o

alicerunwin.exe: alicerun.exe
	cp $< $@
	editbin /subsystem:windows $@

%.exe: %.o
	$(LD) $< -o $@ $(LDFLAGS)

$(BINDIR)/%.exe: %.exe
	install -m 555 $< $@

install: $(EXES:%=$(BINDIR)/%)
	rm -f $(SCRIPTS)

clean:
	-rm -f alice.o alicec.o aliceserv.o alicelink.o alicedep.o
	-rm -f alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe
