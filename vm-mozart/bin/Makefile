.PHONY: all clean

FILESRC = getenv.cc initenv.cc panic.cc path.cc ppid.cc registry.cc
FILEOBJS = $(FILESRC:%.cc=%.o)

CXX = g++-2
CXXFLAGS = -mno-cygwin -O3 -march=i686 -mcpu=i686

all: $(FILEOBJS) alicerun.exe alice.exe alicec.exe alicelink.exe alicedep.exe

alicerun.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) alicerun.cc $(FILEOBJS) -o alicerun.exe

alice.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/ToplevelMain"' \
        alicex.cc $(FILEOBJS) -o alice.exe

alicec.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/compiler/CompilerMain"' \
        alicex.cc $(FILEOBJS) -o alicec.exe

alicelink.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/linker/Main"' \
        alicex.cc $(FILEOBJS) -o alicelink.exe

alicedep.exe: $(FILEOBJS)
	$(CXX) $(CXXFLAGS) -DROOT='"x-alice:/depend/Main"' \
        alicex.cc $(FILEOBJS) -o alicedep.exe

%.o: %.cc
	$(CXX) $(DEFINES) $(INCLUDES) $(CXXFLAGS) -c $< -o $@

clean:
	-rm -f $(FILEOBJS)
	-rm -f alicerun.o alice.o alicec.o alicelink.o alicedep.o
	-rm -f alicerun.exe alice.exe alicec.exe alicelink.exe alicedep.exe
