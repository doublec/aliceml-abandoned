PREFIX = /opt/stockhausen-devel

STOC = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/stoc-mozart
STOC_OPTS = --no-dump-elaboration-sig --no-implicit-import
OZHOME = /opt/mozart-1.1.1
OZC = ozc
OZL = ozl

NATIVESOURCES = IO TextIO OS Unix CommandLine Tools Pickle Debug Socket
NATIVEFILES = $(NATIVESOURCES:%=lib/%.ozf)

LIBSOURCES = Base.sml $(NATIVESOURCES:%=%.ozf.sig)
LIBFILES = $(LIBSOURCES:%=lib/%)

MLYACCSOURCES = base.sig join.sml lrtable.sml stream.sml parser2.sml
MLYACCFILES = $(MLYACCSOURCES:%=lib/ml-yacc/%)

MISCSOURCES = \
	Assert.sml HASH_KEY.sig StringHashKey.sml LargeIntHashKey.sml \
	IMP_SET.sig MakeHashImpSet.sml IMP_MAP.sig MakeHashImpMap.sml \
	SCOPED_IMP_SET.sig MakeScopedImpSet.sml MakeHashScopedImpSet.sml \
	SCOPED_IMP_MAP.sig MakeScopedImpMap.sml MakeHashScopedImpMap.sml \
	STAMP.sig MakeStamp.sml GLOBAL_STAMP.sig GlobalStamp.sml \
	PRETTY_PRINT.sig PrettyPrint.sml PP_MISC.sig PPMisc.sml \
	URL.sig Url.sml SORT.sig MakeSort.sml DEPTH_FIRST_SEARCH.sig \
	MakeDepthFirstSearch.sml
MISCFILES = $(MISCSOURCES:%=misc/%)

INFRASTRUCTURESOURCES = \
	CRASH.sig Crash.sml SOURCE.sig Source.sml ERROR.sig Error.sml \
	SWITCHES.sig MakeSwitches.sml CONTEXT.sig REPRESENTATION.sig \
	PHASE.sig PHASE_ERROR.sig ComposePhases.sml MakeTracingPhase.sml \
	MakeDumpingPhase.sml EmptyContext.sml \
	SIGNATURE.sig COMPOSER.sig TARGET.sig
INFRASTRUCTUREFILES = $(INFRASTRUCTURESOURCES:%=infrastructure/%)

COMMONSOURCES = \
	Stamp.sml StampSet.sml StampMap.sml NAME.sig Name.sml LABEL.sig \
	Label.sml PATH.sig Path.sml PathSet.sml PathMap.sml TYPE.sig Type.sml \
	PP_PATH.sig PPPath.sml PP_TYPE.sig PPType.sml FIXITY.sig Fixity.sml \
	INF.sig Inf.sml PP_INF.sig PPInf.sml INTERMEDIATE_GRAMMAR.sig \
	MakeIntermediateGrammar.sml PP_INTERMEDIATE_GRAMMAR.sig \
	MakePPIntermediateGrammar.sml IntermediateGrammar.sml \
	CHECK_INTERMEDIATE.sig CheckIntermediate.sml \
	PERVASIVE_TYPE.sig MakePervasiveType.sml PervasiveType.sml \
	LABEL_REFLECTION.sig LabelReflection.sml \
	PATH_REFLECTION.sig PathReflection.sml \
	TYPE_REFLECTION.sig TypeReflection.sml
COMMONFILES = $(COMMONSOURCES:%=common/%)

FRONTENDCOMMONSOURCES = \
	StringMap.sml ABSTRACT_GRAMMAR.sig MakeAbstractGrammar.sml \
	PP_ABSTRACT_GRAMMAR.sig MakePPAbstractGrammar.sml \
	AbstractGrammar.sml TypedGrammar.sml ENV.sig Env.sml \
	ELABORATION_ERROR.sig ElaborationError.sml ELABORATION_PHASE.sig \
	MakeElaborationPhase.sml TRANSLATION_PHASE.sig \
	MakeTranslationPhase.sml MakeFrontendCommon.sml
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
	ParsingPhase.sml MakeFrontendSML.sml
FRONTENDSMLFILES = $(FRONTENDSMLSOURCES:%=frontend-sml/%)

BACKENDCOMMONSOURCES = \
	LABEL_SORT.sig MakeLabelSort.sml LabelSort.sml FLAT_GRAMMAR.sig \
	FlatGrammar.sml OUTPUT_FLAT_GRAMMAR.sig OutputFlatGrammar.sml \
	INTERMEDIATE_AUX.sig IntermediateAux.sml SIMPLIFY_MATCH.sig \
	SimplifyMatch.sml SIMPLIFY_REC.sig SimplifyRec.sml \
	FLATTENING_PHASE.sig FlatteningPhase.sml PRIM_OPS.sig \
	PrimOps.sml VALUE_PROPAGATION_PHASE.sig ValuePropagationPhase.sml \
	LIVENESS_ANALYSIS_PHASE.sig LivenessAnalysisPhase.sml CODE.sig \
	ENGINE.sig MakeEngine.sml BackendCommon.sml
BACKENDCOMMONFILES = $(BACKENDCOMMONSOURCES:%=backend-common/%)

BACKENDMOZARTSOURCES = CodeGenPhase.ozf.sig MakeBackendMozart.sml
BACKENDMOZARTFILES = $(BACKENDMOZARTSOURCES:%=backend-mozart/%)

TOPSOURCES = Signature.sml Composer.sml COMPILER.sig MakeCompiler.sml \
	MAIN.sig MakeMain.sml Main.sml SMLToMozartCompiler.sml
TOPFILES = $(TOPSOURCES:%=top/%)

ALLFILES = \
	$(NATIVEFILES) $(LIBFILES) $(MLYACCFILES) $(MISCFILES) \
	$(INFRASTRUCTUREFILES) $(COMMONFILES) $(FRONTENDCOMMONFILES) \
	$(FRONTENDSMLFILES) $(BACKENDCOMMONFILES) $(BACKENDMOZARTFILES) \
	$(TOPFILES)

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib
INSTALLFILES0 = stow.ozf stoc.ozf Default.import
INSTALLBINFILES0 = stoc stow stoc.bat stow.bat
INSTALLLIBFILES0 = Base.ozf $(NATIVESOURCES:%=%.ozf)
INSTALLFILES = \
	$(INSTALLFILES0:%=$(PREFIX)/%) \
	$(INSTALLBINFILES0:%=$(PREFIX)/bin/%) \
	$(INSTALLLIBFILES0:%=$(PREFIX)/lib/%)
MINIMALINSTALL = $(PREFIX)/bin/stoc $(PREFIX)/bin/stow $(PREFIX)/stoc.ozf

.PHONY: all install clean

all: $(ALLFILES) backend-mozart/CodeGenPhase.ozf top/Main.ozf stoc.ozf stow.ozf

bootstrap:
	OZ_LOAD=prefix=x-alice\\:/=./:prefix=http\\://www.mozart-oz.org/home-1.1.1/=$(OZHOME)/ \
	make STOC=$(PREFIX)/bin/stoc lib/Base.ozf all

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

lib/ml-yacc/%: ../../lib/bootstrap/ml-yacc/% lib/ml-yacc/%.header Base.import
	cat Base.import lib/ml-yacc/$*.header $< > $@

misc/%: misc/%.body Base.import
	cat Base.import $< > $@

misc/%: ../../stoc/misc/% misc/%.header Base.import
	cat Base.import misc/$*.header $< > $@

infrastructure/%: ../../stoc/infrastructure/% infrastructure/%.header \
	Base.import
	cat Base.import infrastructure/$*.header $< > $@

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

top/%: top/%.body Base.import
	cat Base.import $< > $@

top/%: ../../stoc/top/% top/%.header Base.import
	cat Base.import top/$*.header $< > $@

%.ozf: %.sml
	$(STOC) $(STOC_OPTS) -c $< -o $@

%.ozf: %.sig
	$(STOC) $(STOC_OPTS) -c $< -o $@

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
	$(PREFIX)/bin/stoc --no-implicit-import -c $< -o $@

$(PREFIX)/lib/%: lib/% lib/%.sig $(MINIMALINSTALL)
	$(PREFIX)/bin/stoc --no-implicit-import --replacesign $< $<.sig $@

clean:
	-rm -f $(ALLFILES)
	-rm -f *.ozf */*.ozf */*/*.ozf
	-rm -f *.ozm */*.ozm */*/*.ozm
