STOC_MOZART = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/top/stoc-mozart

NATIVESOURCES = BootWord IO TextIO OS Unix
NATIVEFILES = $(NATIVESOURCES:%=lib/%.ozf)

MLYACCSOURCES = base.sig join.sml lrtable.sml stream.sml parser2.sml
MLYACCFILES = $(MLYACCSOURCES:%=lib/ml-yacc/%)

ERRORSOURCES = SOURCE.sig Source.sml CRASH.sig Crash.sml ERROR.sig Error.sml
ERRORFILES = $(ERRORSOURCES:%=error/%)

MISCSOURCES = \
	Assert.sml HASH_KEY.sig StringHashKey.sml Wide.sml MISC.sig Misc.sml \
	IMP_SET.sig MakeHashImpSet.sml IMP_MAP.sig MakeHashImpMap.sml \
	SCOPED_IMP_SET.sig MakeScopedImpSet.sml MakeHashScopedImpSet.sml \
	SCOPED_IMP_MAP.sig MakeScopedImpMap.sml MakeHashScopedImpMap.sml \
	STAMP.sig MakeStamp.sml GLOBAL_STAMP.sig GlobalStamp.sml \
	PRETTY_PRINT.sig PrettyPrint.sml PP_MISC.sig PPMisc.sml URL.sig Url.sml
MISCFILES = $(MISCSOURCES:%=misc/%)

TOPSOURCES = \
	PHASE.sig ComposePhases.sml TARGET.sig EmptyContext.sml ENGINE.sig \
	MakeEngine.sml SIGNATURE.sig COMPOSER.sig
TOPFILES = $(TOPSOURCES:%=top/%)

COMMONSOURCES = \
	Stamp.sml StampSet.sml StampMap.sml NAME.sig Name.sml LABEL.sig \
	Label.sml PATH.sig Path.sml PathSet.sml PathMap.sml TYPE.sig Type.sml \
	PP_PATH.sig PPPath.sml PP_TYPE.sig PPType.sml FIXITY.sig Fixity.sml \
	INF.sig Inf.sml PP_INF.sig PPInf.sml INTERMEDIATE_GRAMMAR.sig \
	MakeIntermediateGrammar.sml IntermediateGrammar.sml PREBOUND.sig \
	MakePrebound.sml Prebound.sml PREBOUND_TYPE.sig PreboundType.sml
COMMONFILES = $(COMMONSOURCES:%=common/%)

BODYFILES = $(NATIVESOURCES:%=lib/%.ozf.sig) lib/Word.sml \
	$(MLYACCFILES) $(ERRORFILES) $(MISCFILES) $(TOPFILES) $(COMMONFILES)

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

misc/%: ../../stoc/misc/% misc/%.header Base.import
	cat Base.import misc/$*.header $< > $@

top/%: ../../stoc/top/% top/%.header Base.import
	cat Base.import top/$*.header $< > $@

common/%: ../../stoc/common/% common/%.header Base.import
	cat Base.import common/$*.header $< > $@

%.ozf: %.sml
	$(STOC_MOZART) $< $@

%.ozf: %.sig
	$(STOC_MOZART) $< $@

clean:
	rm -rf $(ALLFILES)
	rm -rf lib/*.ozf
