PREFIX=/home/pslab/Projects/Alice-Distribution

CXX = seamtool compile
CXXFLAGS = -O2 -march=pentium3 -mcpu=pentium3

LD = g++ -mno-cygwin
LDFLAGS = -L$(PREFIX)/lib -lseam

all: alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe

alice.o:
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/ToplevelMain"' \
        -c alicex.cc -o alice.o

alicec.o:
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerMain"' \
        -c alicex.cc -o alicec.o

aliceserv.o:
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerServerMain"' \
        -c alicex.cc -o aliceserv.o

alicelink.o:
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Linker"' \
        -c alicex.cc -o alicelink.o

alicedep.o:
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Depend"' \
        -c alicex.cc -o alicedep.o

%.exe: %.o
	$(LD) $< -o $@ $(LDFLAGS)

clean:
	-rm -f alice.o alicec.o aliceserv.o alicelink.o alicedep.o
	-rm -f alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe
