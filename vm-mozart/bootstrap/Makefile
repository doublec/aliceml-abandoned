PREFIX = /opt/stockhausen-operette1

STOC = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/top/stoc-mozart
OZHOME = /opt/mozart-1.1.1
OZC = ozc
OZL = ozl

NATIVESOURCES = IO TextIO OS Unix CommandLine Tools Pickle Debug
NATIVEFILES = $(NATIVESOURCES:%=lib/%.ozf)

LIBSOURCES = Base.sml $(NATIVESOURCES:%=%.ozf.sig)
LIBFILES = $(LIBSOURCES:%=lib/%)

MLYACCSOURCES = base.sig join.sml lrtable.sml stream.sml parser2.sml
MLYACCFILES = $(MLYACCSOURCES:%=lib/ml-yacc/%)

ERRORSOURCES = SOURCE.sig Source.sml CRASH.sig Crash.sml ERROR.sig Error.sml
ERRORFILES = $(ERRORSOURCES:%=error/%)

MISCSOURCES = \
	Assert.sml HASH_KEY.sig StringHashKey.sml LargeIntHashKey.sml \
	Wide.sml MISC.sig Misc.sml IMP_SET.sig MakeHashImpSet.sml IMP_MAP.sig \
	MakeHashImpMap.sml SCOPED_IMP_SET.sig MakeScopedImpSet.sml \
	MakeHashScopedImpSet.sml SCOPED_IMP_MAP.sig MakeScopedImpMap.sml \
	MakeHashScopedImpMap.sml STAMP.sig MakeStamp.sml GLOBAL_STAMP.sig \
	GlobalStamp.sml PRETTY_PRINT.sig PrettyPrint.sml PP_MISC.sig \
	PPMisc.sml URL.sig Url.sml
MISCFILES = $(MISCSOURCES:%=misc/%)

TOPSOURCES = \
	PHASE.sig ComposePhases.sml TARGET.sig EmptyContext.sml ENGINE.sig \
	MakeEngine.sml SIGNATURE.sig Signature.sml COMPOSER.sig Composer.sml
TOPFILES = $(TOPSOURCES:%=top/%)

COMMONSOURCES = \
	Stamp.sml StampSet.sml StampMap.sml NAME.sig Name.sml LABEL.sig \
	Label.sml PATH.sig Path.sml PathSet.sml PathMap.sml TYPE.sig Type.sml \
	PP_PATH.sig PPPath.sml PP_TYPE.sig PPType.sml FIXITY.sig Fixity.sml \
	INF.sig Inf.sml PP_INF.sig PPInf.sml INTERMEDIATE_GRAMMAR.sig \
	MakeIntermediateGrammar.sml IntermediateGrammar.sml PREBOUND.sig \
	MakePrebound.sml Prebound.sml PREBOUND_TYPE.sig PreboundType.sml
COMMONFILES = $(COMMONSOURCES:%=common/%)

FRONTENDCOMMONSOURCES = \
	StringMap.sml ABSTRACT_GRAMMAR.sig MakeAbstractGrammar.sml \
	AbstractGrammar.sml TypedGrammar.sml ENV.sig Env.sml ENV0.sig \
	Env0.sml ELABORATION_ERROR.sig ElaborationError.sml \
	ELABORATION_PHASE.sig MakeElaborationPhase.sml TRANSLATION_PHASE.sig \
	TranslationPhase.sml MakeFrontendCommon.sml
FRONTENDCOMMONFILES = $(FRONTENDCOMMONSOURCES:%=frontend-common/%)

FRONTENDSMLSOURCES = \
	LAB.sig Lab.sml ID.sig MakeId.sml Ids.sml SCON.sig SCon.sml \
	INPUT_GRAMMAR.sig MakeInputGrammar.sml InputGrammar.sml \
	PARSING_ERROR.sig ParsingError.sml ABSTRACTION_ERROR.sig \
	AbstractionError.sml INFIX.sig Infix.sml BIND_ENV.sig BindEnv.sml \
	BIND_ENV_FROM_SIG.sig BindEnvFromSig.sml SHARING.sig Sharing.sml \
	ABSTRACTION_PHASE.sig MakeAbstractionPhase.sml DERIVED_FORMS.sig \
	PARSER.sig Parser.sml DerivedForms.sml LEXER_ERROR.sig \
	LexerError.sml Lexer.lex.sml CountPosLexer.sml PARSING_PHASE.sig \
	ParsingPhase.sml BIND_ENV0.sig BindEnv0.sml MakeFrontendSML.sml
FRONTENDSMLFILES = $(FRONTENDSMLSOURCES:%=frontend-sml/%)

BACKENDCOMMONSOURCES = \
	LABEL_SORT.sig MakeLabelSort.sml LabelSort.sml FLAT_GRAMMAR.sig \
	FlatGrammar.sml OUTPUT_FLAT_GRAMMAR.sig OutputFlatGrammar.sml \
	INTERMEDIATE_AUX.sig IntermediateAux.sml SIMPLIFY_MATCH.sig \
	SimplifyMatch.sml SIMPLIFY_REC.sig SimplifyRec.sml \
	FLATTENING_PHASE.sig FlatteningPhase.sml LIVENESS_ANALYSIS_PHASE.sig \
	LivenessAnalysisPhase.sml
BACKENDCOMMONFILES = $(BACKENDCOMMONSOURCES:%=backend-common/%)

ALLFILES = \
	$(NATIVEFILES) $(LIBFILES) $(MLYACCFILES) $(ERRORFILES) $(MISCFILES) \
	$(TOPFILES) $(COMMONFILES) $(FRONTENDCOMMONFILES) $(FRONTENDSMLFILES) \
	$(BACKENDCOMMONFILES) backend-mozart/CodeGenPhase.ozf.sig top/Main.sml

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib
INSTALLFILES0 = stow.ozf stoc.ozf Default.import
INSTALLBINFILES0 = stoc stow
INSTALLLIBFILES0 = Base.ozf IO.ozf TextIO.ozf OS.ozf Unix.ozf CommandLine.ozf \
	Tools.ozf
INSTALLFILES = \
	$(INSTALLFILES0:%=$(PREFIX)/%) \
	$(INSTALLBINFILES0:%=$(PREFIX)/bin/%) \
	$(INSTALLLIBFILES0:%=$(PREFIX)/lib/%)
MINIMALINSTALL = $(PREFIX)/bin/stoc $(PREFIX)/bin/stow $(PREFIX)/stoc.ozf

.PHONY: all install clean

all: $(ALLFILES) backend-mozart/CodeGenPhase.ozf top/Main.ozf stoc.ozf stow.ozf

stoc.ozf: top/Main.ozf
	OZ_LOAD=prefix=x-alice\\:/=./:prefix=http\\://www.mozart-oz.org/home-1.1.1/=$(OZHOME)/ \
	$(OZL) -z9 --include=x-alice: -v x-alice:/$< -o $@

stow.ozf: Init.ozf
	OZ_LOAD=prefix=x-alice\\:/=./:prefix=http\\://www.mozart-oz.org/home-1.1.1/=$(OZHOME)/ \
	$(OZL) -z9 --include=x-alice: -v $< -o $@

%.ozf: %.oz
	$(OZC) -c $< -o $@

lib/Base.sml: ../../lib/bootstrap/Bootstrap.aus
	cp $< $@

$(NATIVEFILES): lib/%: ../%
	cp $< $@

%: %.body Base.import
	cat Base.import $< > $@

top/Main.sml: top/Main.sml.body Base.import
	cat Base.import $< > $@

misc/GlobalStamp.sml: misc/GlobalStamp.sml.body Base.import
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

frontend-common/%: ../../stoc/frontend-common/% frontend-common/%.header \
	Base.import
	cat Base.import frontend-common/$*.header $< > $@

frontend-sml/%: ../../stoc/frontend-sml/% frontend-sml/%.header Base.import
	cat Base.import frontend-sml/$*.header $< > $@

frontend-sml/PARSER.sig: ../../stoc/frontend-sml/Parser.grm.sig \
	frontend-sml/PARSER.sig.header Base.import
	cat Base.import $@.header $< > $@

frontend-sml/Parser.sml: ../../stoc/frontend-sml/Parser.grm.sml \
	frontend-sml/Parser.sml.header Base.import
	cat Base.import $@.header $< > $@

backend-common/%: ../../stoc/backend-common/% backend-common/%.header \
	Base.import
	cat Base.import backend-common/$*.header $< > $@

%.ozf: %.sml
	$(STOC) $< $@

%.ozf: %.sig
	$(STOC) $< $@

install: all $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(INSTALLFILES0:%=$(PREFIX)/%): $(PREFIX)/%: %
	install -c -m 444 $< $@

Default.import: Base.import Other.import
	cat Base.import Other.import > $@

$(PREFIX)/bin/%: %
	install -c -m 555 $< $@

$(PREFIX)/lib/Base.ozf: lib/Base.sml $(MINIMALINSTALL)
	$(PREFIX)/bin/stoc --nodefaultimport -c $< -o $@

$(PREFIX)/lib/%: lib/% lib/%.sig $(MINIMALINSTALL)
	$(PREFIX)/bin/stoc --replacesign $< $<.sig $@

clean:
	-rm -f $(ALLFILES)
	-rm -f *.ozf */*.ozf */*/*.ozf
