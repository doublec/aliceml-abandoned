PREFIX=/home/pslab/Projects/distro
BINDIR=$(PREFIX)/bin

CXX = seamtool -v compile
CXXFLAGS = -O2 -march=pentium3 -mcpu=pentium3

LD = i686-pc-mingw32-g++
LDFLAGS = -L$(PREFIX)/lib -lseam

EXES = \
	alicerun.exe alicewin.exe alice.exe alicec.exe \
	alicelink.exe alicedep.exe aliceglade.exe aliceremote.exe
SCRIPTS = $(EXES:%.exe=$(BINDIR)/%)

all: $(EXES)

alicerun.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{--argc>0?*++argv:usage()}' \
		-DPRELOAD_GTK=1 \
		-c $< -o $@

aliceremote.exe: alicerun.exe
	cp $< $@

alice.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/tools/Toplevel"}' \
		-DPRELOAD_GTK=1 \
		-c $< -o $@

alicewin.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/tools/Toplevel","--gui"}' \
		-DPRELOAD_GTK=1 \
		-c $< -o $@

alicec.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/compiler/CompilerMain"}' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicelink.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/tools/Linker"}' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicedep.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/tools/Depend"}' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

aliceglade.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DARGS='{"x-alice:/tools/Glade"}' \
		-DPRELOAD_GTK=1 \
		-c $< -o $@

alicewin.exe: alicewin.o
	$(LD) $< -o $@ $(LDFLAGS)
	editbin /subsystem:windows $@

%.exe: %.o
	$(LD) $< -o $@ $(LDFLAGS)

$(BINDIR)/%.exe: %.exe
	install -m 555 $< $@

install: $(EXES:%=$(BINDIR)/%)
	rm -f $(SCRIPTS)

clean:
	-rm -f $(EXES)
	-rm -f $(EXES:%.exe=%.o)
