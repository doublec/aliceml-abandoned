OZC = ozc
STOC = ../../stoc/backend-mozart/stoc-mozart.exe --image=../../stoc/top/stoc-mozart
MERGE = ./merge.sh
OZHOME = /opt/mozart-1.1.0
OZL = ozengine ../Init.ozf $(OZHOME)/bin/ozl

LIB_DIR = ../../lib/bootstrap
STOC_DIR = ../../stoc

BASE_FILES = \
	$(LIB_DIR)/Bootstrap.aus

WORD_FILES = \
	Base.import \
	Word.sml

MLYACC_FILES = \
	Base.import \
	TextIO.import \
	/opt/smlnj/src/ml-yacc/lib/base.sig \
	/opt/smlnj/src/ml-yacc/lib/join.sml \
	/opt/smlnj/src/ml-yacc/lib/lrtable.sml \
	/opt/smlnj/src/ml-yacc/lib/stream.sml \
	/opt/smlnj/src/ml-yacc/lib/parser2.sml

MISC_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	$(STOC_DIR)/misc/Assert.sml \
	$(STOC_DIR)/misc/HASH_KEY.sig \
	$(STOC_DIR)/misc/StringHashKey.sml \
	$(STOC_DIR)/misc/Wide.sml \
	$(STOC_DIR)/misc/MISC.sig \
	$(STOC_DIR)/misc/Misc.sml \
	$(STOC_DIR)/misc/IMP_SET.sig \
	$(STOC_DIR)/misc/MakeHashImpSet.sml \
	$(STOC_DIR)/misc/IMP_MAP.sig \
	$(STOC_DIR)/misc/MakeHashImpMap.sml \
	$(STOC_DIR)/misc/SCOPED_IMP_SET.sig \
	$(STOC_DIR)/misc/MakeScopedImpSet.sml \
	$(STOC_DIR)/misc/MakeHashScopedImpSet.sml \
	$(STOC_DIR)/misc/SCOPED_IMP_MAP.sig \
	$(STOC_DIR)/misc/MakeScopedImpMap.sml \
	$(STOC_DIR)/misc/MakeHashScopedImpMap.sml \
	$(STOC_DIR)/misc/STAMP.sig \
	$(STOC_DIR)/misc/MakeStamp.sml \
	$(STOC_DIR)/misc/PRETTY_PRINT.sig \
	$(STOC_DIR)/misc/PrettyPrint.sml \
	$(STOC_DIR)/misc/PP_MISC.sig \
	$(STOC_DIR)/misc/PPMisc.sml \
	$(STOC_DIR)/misc/URL.sig \
	$(STOC_DIR)/misc/Url.sml

ERROR_FILES = \
	Base.import \
	TextIO.import \
	$(STOC_DIR)/error/SOURCE.sig \
	$(STOC_DIR)/error/Source.sml \
	$(STOC_DIR)/error/CRASH.sig \
	$(STOC_DIR)/error/Crash.sml \
	$(STOC_DIR)/error/ERROR.sig \
	$(STOC_DIR)/error/Error.sml

COMMON_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	$(STOC_DIR)/common/Stamp.sml \
	$(STOC_DIR)/common/StampSet.sml \
	$(STOC_DIR)/common/StampMap.sml \
	$(STOC_DIR)/common/NAME.sig \
	$(STOC_DIR)/common/Name.sml \
	$(STOC_DIR)/common/LABEL.sig \
	$(STOC_DIR)/common/Label.sml \
	$(STOC_DIR)/common/PATH.sig \
	$(STOC_DIR)/common/Path.sml \
	$(STOC_DIR)/common/PathSet.sml \
	$(STOC_DIR)/common/PathMap.sml \
	$(STOC_DIR)/common/TYPE.sig \
	$(STOC_DIR)/common/Type.sml \
	$(STOC_DIR)/common/PP_PATH.sig \
	$(STOC_DIR)/common/PPPath.sml \
	$(STOC_DIR)/common/PP_TYPE.sig \
	$(STOC_DIR)/common/PPType.sml \
	$(STOC_DIR)/common/INTERMEDIATE_GRAMMAR.sig \
	$(STOC_DIR)/common/MakeIntermediateGrammar.sml \
	$(STOC_DIR)/common/IntermediateGrammar.sml \
	$(STOC_DIR)/common/PREBOUND.sig \
	$(STOC_DIR)/common/MakePrebound.sml \
	$(STOC_DIR)/common/Prebound.sml \
	$(STOC_DIR)/common/PREBOUND_TYPE.sig \
	$(STOC_DIR)/common/PreboundType.sml

TOP_FILES = \
	Base.import \
	Error.import \
	$(STOC_DIR)/top/PHASE.sig \
	$(STOC_DIR)/top/TARGET.sig \
	$(STOC_DIR)/top/EmptyContext.sml

FRONTEND_COMMON_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	Common.import \
	Top.import \
	$(STOC_DIR)/frontend-common/StringMap.sml \
	$(STOC_DIR)/frontend-common/FIXITY.sig \
	$(STOC_DIR)/frontend-common/Fixity.sml \
	$(STOC_DIR)/frontend-common/INF.sig \
	$(STOC_DIR)/frontend-common/Inf.sml \
	$(STOC_DIR)/frontend-common/ABSTRACT_GRAMMAR.sig \
	$(STOC_DIR)/frontend-common/MakeAbstractGrammar.sml \
	$(STOC_DIR)/frontend-common/AbstractGrammar.sml \
	$(STOC_DIR)/frontend-common/TypedGrammar.sml \
	$(STOC_DIR)/frontend-common/ENV.sig \
	$(STOC_DIR)/frontend-common/Env.sml \
	$(STOC_DIR)/frontend-common/ENV0.sig \
	$(STOC_DIR)/frontend-common/Env0.sml \
	$(STOC_DIR)/frontend-common/PP_INF.sig \
	$(STOC_DIR)/frontend-common/PPInf.sml \
	$(STOC_DIR)/frontend-common/ELABORATION_ERROR.sig \
	$(STOC_DIR)/frontend-common/ElaborationError.sml \
	$(STOC_DIR)/frontend-common/ELABORATION_PHASE.sig \
	$(STOC_DIR)/frontend-common/ElaborationPhase.sml \
	$(STOC_DIR)/frontend-common/TRANSLATION_PHASE.sig \
	$(STOC_DIR)/frontend-common/TranslationPhase.sml

FRONTEND_SML_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	Common.import \
	Top.import \
	FrontendCommon.import \
	$(STOC_DIR)/frontend-sml/LAB.sig \
	$(STOC_DIR)/frontend-sml/Lab.sml \
	$(STOC_DIR)/frontend-sml/ID.sig \
	$(STOC_DIR)/frontend-sml/MakeId.sml \
	$(STOC_DIR)/frontend-sml/Ids.sml \
	$(STOC_DIR)/frontend-sml/SCON.sig \
	$(STOC_DIR)/frontend-sml/SCon.sml \
	$(STOC_DIR)/frontend-sml/INPUT_GRAMMAR.sig \
	$(STOC_DIR)/frontend-sml/MakeInputGrammar.sml \
	$(STOC_DIR)/frontend-sml/InputGrammar.sml \
	$(STOC_DIR)/frontend-sml/PARSING_ERROR.sig \
	$(STOC_DIR)/frontend-sml/ParsingError.sml \
	$(STOC_DIR)/frontend-sml/ABSTRACTION_ERROR.sig \
	$(STOC_DIR)/frontend-sml/AbstractionError.sml \
	$(STOC_DIR)/frontend-sml/INFIX.sig \
	$(STOC_DIR)/frontend-sml/Infix.sml \
	$(STOC_DIR)/frontend-sml/BIND_ENV.sig \
	$(STOC_DIR)/frontend-sml/BindEnv.sml \
	$(STOC_DIR)/frontend-sml/SHARING.sig \
	$(STOC_DIR)/frontend-sml/Sharing.sml \
	$(STOC_DIR)/frontend-sml/ABSTRACTION_PHASE.sig \
	$(STOC_DIR)/frontend-sml/AbstractionPhase.sml \
	$(STOC_DIR)/frontend-sml/DERIVED_FORMS.sig \
	$(STOC_DIR)/frontend-sml/DerivedForms.sml \
	$(STOC_DIR)/frontend-sml/BIND_ENV0.sig \
	$(STOC_DIR)/frontend-sml/BindEnv0.sml

FRONTEND_SML_LEXER_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	MLYacc.import \
	Misc.import \
	Error.import \
	FrontendSML.import \
	$(STOC_DIR)/frontend-sml/Parser.grm.sig \
	$(STOC_DIR)/frontend-sml/LEXER_ERROR.sig \
	$(STOC_DIR)/frontend-sml/LexerError.sml \
	$(STOC_DIR)/frontend-sml/Lexer.lex.sml \
	$(STOC_DIR)/frontend-sml/CountPosLexer.sml

FRONTEND_SML_PARSER_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	MLYacc.import \
	Misc.import \
	Error.import \
	FrontendSMLLexer.import \
	FrontendSML.import \
	$(STOC_DIR)/frontend-sml/Parser.grm.sml \
	$(STOC_DIR)/frontend-sml/PARSING_PHASE.sig \
	$(STOC_DIR)/frontend-sml/ParsingPhase.sml

FRONTEND_SML_MAIN_FILES = \
	Base.import \
	FrontendSMLMain.sml

BACKEND_COMMON_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	Common.import \
	$(STOC_DIR)/backend-common/LABEL_SORT.sig \
	$(STOC_DIR)/backend-common/MakeLabelSort.sml \
	$(STOC_DIR)/backend-common/LabelSort.sml \
	$(STOC_DIR)/backend-common/FLAT_GRAMMAR.sig \
	$(STOC_DIR)/backend-common/FlatGrammar.sml \
	$(STOC_DIR)/backend-common/OUTPUT_FLAT_GRAMMAR.sig \
	$(STOC_DIR)/backend-common/OutputFlatGrammar.sml \
	$(STOC_DIR)/backend-common/INTERMEDIATE_AUX.sig \
	$(STOC_DIR)/backend-common/IntermediateAux.sml \
	$(STOC_DIR)/backend-common/SIMPLIFY_MATCH.sig \
	$(STOC_DIR)/backend-common/SimplifyMatch.sml \
	$(STOC_DIR)/backend-common/SIMPLIFY_REC.sig \
	$(STOC_DIR)/backend-common/SimplifyRec.sml \
	$(STOC_DIR)/backend-common/FLATTENING_PHASE.sig \
	$(STOC_DIR)/backend-common/FlatteningPhase.sml \
	$(STOC_DIR)/backend-common/LIVENESS_ANALYSIS_PHASE.sig \
	$(STOC_DIR)/backend-common/LivenessAnalysisPhase.sml

BACKEND_MOZART_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	Common.import \
	BackendCommon.import \
	$(STOC_DIR)/backend-mozart/OZIFY_IMPERATIVE_GRAMMAR.sig \
	$(STOC_DIR)/backend-mozart/OzifyImperativeGrammar.sml

MAIN_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	Misc.import \
	Error.import \
	Common.import \
	FrontendCommon.import \
	FrontendSMLMain.import \
	BackendCommon.import \
	BackendMozart.import \
	Main.sml

COMPONENTS = \
	Base Word \
	MLYacc \
	Misc Error Common Top \
	FrontendCommon \
	FrontendSML FrontendSMLLexer FrontendSMLParser FrontendSMLMain \
	BackendCommon BackendMozart \
	Main Test

%.ozf: %.aus
	$(STOC) --in=$< --out=$@

%.ozf: %.oz
	$(OZC) -c $< -o $@

all: boot-stoc

bootstrap:
	make STOC='ozengine ../Init.ozf boot-stoc' stoc-mozart

stoc-mozart: $(COMPONENTS:%=%.ozf)
	$(OZL) $(OZLFLAGS) -v --exclude=.. --include=.,../../stoc \
	-x Test.ozf -o $@

boot-stoc: $(COMPONENTS:%=%.ozf)
	$(OZL) $(OZLFLAGS) -v --exclude=.. --include=.,../../stoc \
	-x Test.ozf -o $@

Base.aus: $(BASE_FILES)
	$(MERGE) $(BASE_FILES) > $@
Word.aus: $(WORD_FILES)
	$(MERGE) $(WORD_FILES) > $@
MLYacc.aus: $(MLYACC_FILES)
	$(MERGE) $(MLYACC_FILES) > $@
Misc.aus: $(MISC_FILES)
	$(MERGE) $(MISC_FILES) > $@
Error.aus: $(ERROR_FILES)
	$(MERGE) $(ERROR_FILES) > $@
Common.aus: $(COMMON_FILES)
	$(MERGE) $(COMMON_FILES) > $@
Top.aus: $(TOP_FILES)
	$(MERGE) $(TOP_FILES) > $@
FrontendCommon.aus: $(FRONTEND_COMMON_FILES)
	$(MERGE) $(FRONTEND_COMMON_FILES) > $@
FrontendSML.aus: $(FRONTEND_SML_FILES)
	$(MERGE) $(FRONTEND_SML_FILES) > $@
FrontendSMLLexer.aus: $(FRONTEND_SML_LEXER_FILES)
	$(MERGE) $(FRONTEND_SML_LEXER_FILES) > $@
FrontendSMLParser.aus: $(FRONTEND_SML_PARSER_FILES)
	$(MERGE) $(FRONTEND_SML_PARSER_FILES) > $@
FrontendSMLMain.aus: $(FRONTEND_SML_MAIN_FILES)
	$(MERGE) $(FRONTEND_SML_MAIN_FILES) > $@
BackendCommon.aus: $(BACKEND_COMMON_FILES)
	$(MERGE) $(BACKEND_COMMON_FILES) > $@
BackendMozart.aus: $(BACKEND_MOZART_FILES)
	$(MERGE) $(BACKEND_MOZART_FILES) > $@
Main.aus: $(MAIN_FILES)
	$(MERGE) $(MAIN_FILES) > $@

clean:
	-rm -rf $(COMPONENTS:%=%.aus)
	-rm -rf $(COMPONENTS:%=%.ozf)

veryclean: clean
	-rm -rf boot-stoc stoc-mozart
