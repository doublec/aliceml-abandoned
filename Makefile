GECODE_URL = http://www.gecode.org/download/gecode-1.3.1.tar.gz
CWD := $(shell pwd)

SUPPORTDIR := $(CWD)/build/support
SUPPORTPATH := $(SUPPORTDIR)/bin:$(PATH)
SEAMPATH := $(CWD)/build/install/seam/bin:$(SUPPORTPATH)
ALICEPATH := $(CWD)/build/alice/bin:$(SEAMPATH)
INSTALLDIR := $(CWD)/build/distro
INSTALLPATH := $(CWD)/build/distro/bin:$(PATH)

PKG_CONFIG_PATH := $(CWD)/build/gecode/misc:$(PKG_CONFIG_PATH)

SEAM_OUT := $(INSTALLDIR)/lib/libseam.a \
            $(INSTALLDIR)/lib/libseam.la \
            $(INSTALLDIR)/bin/seam \
            $(INSTALLDIR)/bin/seamtool

ALICE_LL_OUT := $(INSTALLDIR)/bin/alice-config \
                $(INSTALLDIR)/bin/aliceremote \
                $(INSTALLDIR)/bin/alicerun \
                $(INSTALLDIR)/bin/alicetool

all: seam-support gecode seam alice-ll alice-boostrap

.PHONY: seam-support
seam-support: $(SUPPORTDIR)/bin/automake-seam

$(SUPPORTDIR)/bin/automake-seam:
	cd seam-support/automake-seam && \
	tar xzf automake-1.7.6.tar.gz && \
	cd automake-1.7.6 && \
	patch -p1 < ../automake-1.7.6.seam.patch && \
	patch -p1 < ../automake2automake-seam.patch && \
	rm -rf aclocal.m4 configure && \
	find . -name Makefile.in | xargs rm -f && \
	libtoolize --automake && aclocal && automake --add-missing && autoconf && \
	./configure --prefix=$(SUPPORTDIR) && \
	make all install

.PHONY: gecode
gecode: build/install/gecode/lib/libgecodeint.a build/install/gecode/lib/libgecodekernel.a build/install/gecode/lib/libgecodeset.a

gecode/gecode-1.3.1/configure: gecode/gecode-1.3.1.tar.gz
	cd gecode && tar mxvf gecode-1.3.1.tar.gz && cd gecode-1.3.1 && patch -p1 <../../make/patches/gecode1-3-1_gcc4-4.patch

gecode/gecode-1.3.1.tar.gz: 
	mkdir -p gecode
	cd gecode && wget http://www.gecode.org/download/gecode-1.3.1.tar.gz


build/gecode/Makefile: gecode/gecode-1.3.1/configure
	mkdir -p build/gecode
	cd build/gecode && ../../gecode/gecode-1.3.1/configure --enable-static --disable-examples --disable-search --disable-minimodel --prefix=$(PWD)/build/install/gecode

build/install/gecode/lib/libgecodeint.a build/install/gecode/lib/libgecodekernel.a build/install/gecode/lib/libgecodeset.a: build/gecode/Makefile
	cd build/gecode && make install

.PHONY: seam
seam: $(SEAM_OUT)

seam/configure: seam/configure.ac
	cd seam && libtoolize --copy --force --automake --ltdl && aclocal -I . && autoconf && automake --add-missing

build/seam/Makefile: seam/configure
	mkdir -p build/seam
	cd build/seam && ../../seam/configure --prefix=$(INSTALLDIR) --with-warnings=yes --disable-lightning

$(SEAM_OUT): build/seam/Makefile
	cd build/seam && make install

.PHONY: alice-ll
alice-ll: $(ALICE_LL_OUT)

alice/vm-seam/configure: alice/vm-seam/configure.ac
	cd alice/vm-seam && PATH=$(SUPPORTPATH) aclocal-seam -I . && autoconf && PATH=$(SUPPORTPATH) automake-seam --add-missing

build/alice/Makefile build/alice/Makefile.bootstrap: alice/vm-seam/configure
	mkdir -p build/alice
	cd build/alice && PATH=$(INSTALLPATH) ../../alice/vm-seam/configure --prefix=$(INSTALLDIR) --with-warnings=yes 

$(ALICE_LL_OUT): build/alice/Makefile
	cd build/alice && PATH=$(INSTALLPATH) make install

.PHONY: alice-bootstrap
alice-boostrap: alice/bootstrap/alicec-seam.x86-linux build/distro/bin/alice

alice/vm-seam/Makefile.bootstrap: build/alice/Makefile.bootstrap
	cp $(CWD)/build/alice/Makefile.bootstrap $(CWD)/alice/vm-seam

alice/bootstrap/alicec-seam.x86-linux: 
	cd alice && PATH=$(INSTALLPATH) make PREFIX="$(INSTALLDIR)" TARGET=seam bootstrap-smlnj

build/distro/bin/alice:  alice/vm-seam/Makefile.bootstrap
	cd alice && PATH=$(INSTALLPATH) make PREFIX="$(INSTALLDIR)" TARGET=seam reinstall-seam

