.PHONY: all clean

FILESRC = getenv.cc initenv.cc panic.cc path.cc ppid.cc registry.cc
FILEOBJS = $(FILESRC:%.cc=%.o)

CXX = g++
CXXFLAGS = -mno-cygwin -O3 -march=i686 -mcpu=i686 # -DMOZART_1_2_0

all: $(FILEOBJS) alicerun.exe alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe

alicerun.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) alicerun.cc $(FILEOBJS) -o alicerun.exe

alice.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/ToplevelMain"' \
        alicex.cc $(FILEOBJS) -o alice.exe

alicec.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerMain"' \
        alicex.cc $(FILEOBJS) -o alicec.exe

aliceserv.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerServerMain"' \
        alicex.cc $(FILEOBJS) -o aliceserv.exe

alicelink.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Linker"' \
        alicex.cc $(FILEOBJS) -o alicelink.exe

alicedep.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/tools/Depend"' \
        alicex.cc $(FILEOBJS) -o alicedep.exe

%.o: %.cc
	$(CXX) $(DEFINES) $(INCLUDES) $(CXXFLAGS) -c $< -o $@

clean:
	-rm -f $(FILEOBJS)
	-rm -f alicerun.o alice.o alicec.o alicelink.o alicedep.o
	-rm -f alicerun.exe alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe
