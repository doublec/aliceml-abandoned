PREFIX=/home/pslab/Projects/distro
BINDIR=$(PREFIX)/bin

CXX = seamtool -v compile
CXXFLAGS = -O2 -march=pentium3 -mcpu=pentium3

LD = g++ -mno-cygwin
LDFLAGS = -L$(PREFIX)/lib -lseam

EXES = \
	alicerun.exe alicerunwin.exe alice.exe alicec.exe \
	alicelink.exe alicedep.exe aliceglade.exe aliceremote.exe
SCRIPTS = $(EXES:%.exe=$(BINDIR)/%)

all: $(EXES)

alicerun.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='--argc>0?(--brokerArgc,*++argv):usage()' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicerunwin.exe: alicerun.exe
	cp $< $@
	editbin /subsystem:windows $@

aliceremote.exe: alicerun.exe
	cp $< $@

alice.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Toplevel"' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicec.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/compiler/CompilerMain"' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicelink.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Linker"' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

alicedep.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Depend"' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

aliceglade.o: alicex.cc Makefile
	$(CXX) $(CXXFLAGS) \
		-DROOT='"x-alice:/tools/Glade"' \
		-DPRELOAD_GTK=0 \
		-c $< -o $@

%.exe: %.o
	$(LD) $< -o $@ $(LDFLAGS)

$(BINDIR)/%.exe: %.exe
	install -m 555 $< $@

install: $(EXES:%=$(BINDIR)/%)
	rm -f $(SCRIPTS)

clean:
	-rm -f alice.o alicec.o aliceserv.o alicelink.o alicedep.o
	-rm -f alice.exe alicec.exe aliceserv.exe alicelink.exe alicedep.exe
