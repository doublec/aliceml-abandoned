STOC_MOZART = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/top/stoc-mozart

NATIVES = BootWord IO TextIO OS Unix
NATIVEFILES = $(NATIVES:%=lib/%.ozf)
BODYFILES = $(NATIVES:%=lib/%.ozf.sig) lib/Word.sml

ALLFILES = lib/Base.sml $(NATIVEFILES) $(BODYFILES)

all: $(ALLFILES)

lib/Base.sml: ../../lib/bootstrap/Bootstrap.aus
	cp $< $@

$(NATIVEFILES): lib/%: ../%
	cp $< $@

%.sml: %.body Base.import
	cat Base.import $< > $@

lib/%.ozf.sig: ../%.ozf.sig Base.import
	cat Base.import $< > $@

%.ozf: %.sml
	$(STOC_MOZART) $< $@

clean:
	rm -rf $(ALLFILES)
	rm -rf lib/*.ozf
