PREFIX=/home/pslab/Projects/Alice-Distribution
BINDIR=$(PREFIX)/bin

CXX = seamtool compile
CXXFLAGS = -O2 -march=pentium3 -mcpu=pentium3

LD = g++ -mno-cygwin
LDFLAGS = -L$(PREFIX)/lib -lseam

EXES = \
	alicerun.exe alice.exe alicec.exe \
	aliceserv.exe alicelink.exe alicedep.exe \
	aide.exe
SCRIPTS = $(EXES:%.exe=$(BINDIR)/%)

all: $(EXES)

alicerun.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='--argc>0?(--brokerArgc,*++argv):usage()' \
        -c alicex.cc -o alicerun.o

alice.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/ToplevelMain"' \
        -c alicex.cc -o alice.o

alicec.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerMain"' \
        -c alicex.cc -o alicec.o

aliceserv.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerServerMain"' \
        -c alicex.cc -o aliceserv.o

alicelink.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Linker"' \
        -c alicex.cc -o alicelink.o

alicedep.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Depend"' \
        -c alicex.cc -o alicedep.o

aide.o: alicex.cc
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/AIDE"' \
        -c alicex.cc -o aide.o

%.exe: %.o
	$(LD) $< -o $@ $(LDFLAGS)

$(BINDIR)/%.exe: %.exe
	install -m 555 $< $@

install: $(EXES:%=$(BINDIR)/%)
	rm -f $(SCRIPTS)

clean:
	-rm -f alice.o alicec.o aliceserv.o alicelink.o alicedep.o
	-rm -f alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe
