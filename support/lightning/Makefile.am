# Automake requirements:
AUTOMAKE_OPTIONS = gnu

PACKAGE=lightning

EXTRA_DIST =  lightning.m4 lightning-dist.h autogen COPYING.DOC COPYING.LESSER

SUBDIRS = doc lightning opcode tests

include_HEADERS = lightning.h

dist-hook:
	-rm -f $(distdir)/$(include_HEADERS)

if NO_ACLOCAL
else
aclocaldir = $(datadir)/aclocal
aclocal_DATA = lightning.m4
endif

prepare:
	@if test -z "$(TARGET)"; then					\
	  echo "Please pass your package's main distribution directory" \
	  echo "in the TARGET variable (\`$(MAKE) prepare TARGET=...')"; \
	  exit 1;							\
	fi
	cp -R lightning $(TARGET)/lightning
	cp lightning-dist.h $(TARGET)/lightning/lightning.h
	cp lightning/Makefile.am > $(TARGET)/lightning/Makefile.am
	@test -f $(TARGET)/config.sub ||				\
	  (echo "cp config.sub $(TARGET)/config.sub";			\
	  cp config.sub $(TARGET)/config.sub)
	@test -f $(TARGET)/config.guess ||				\
	  (echo "cp config.guess $(TARGET)/config.guess";		\
	  cp config.guess $(TARGET)/config.guess)
