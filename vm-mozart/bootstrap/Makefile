STOC_MOZART = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/top/stoc-mozart

NATIVESOURCES = BootWord IO TextIO OS Unix
NATIVEFILES = $(NATIVESOURCES:%=lib/%.ozf)

MLYACCSOURCES = base.sig join.sml lrtable.sml stream.sml parser2.sml
MLYACCFILES = $(MLYACCSOURCES:%=lib/ml-yacc/%)

ERRORSOURCES = SOURCE.sig Source.sml CRASH.sig Crash.sml ERROR.sig Error.sml
ERRORFILES = $(ERRORSOURCES:%=error/%)

BODYFILES = $(NATIVESOURCES:%=lib/%.ozf.sig) lib/Word.sml \
	$(MLYACCFILES) $(ERRORFILES)

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

lib/ml-yacc/%: ../../lib/bootstrap/ml-yacc/% lib/ml-yacc/%.header Base.import
	cat Base.import lib/ml-yacc/$*.header $< > $@

error/%: ../../stoc/error/% error/%.header Base.import
	cat Base.import error/$*.header $< > $@

%.ozf: %.sml
	$(STOC_MOZART) $< $@

%.ozf: %.sig
	$(STOC_MOZART) $< $@

clean:
	rm -rf $(ALLFILES)
	rm -rf lib/*.ozf
