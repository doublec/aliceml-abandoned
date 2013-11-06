###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###   Andreas Rossberg <rossberg@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt and Andreas Rossberg, 1999-2007
###
### Last change:
###   $Date$ by $Author$
###   $Revision$
###

##
## Description of Externally Useful Targets
## ========================================
##
## all:
##    Identical to build3-install.
## depend:
##    Creates the file Makefile.depend,
##    which contains the dependencies needed for Pass 3.
## clean:
##    Removes all files created by previous builds.
## veryclean:
##    Identical to clean.
## distclean:
##    Additionally removes the generated Makefile.depend.
##
## Pass 1
## ------
##
## The goal is to build a compiler using the bootstrap compiler.
## This is performed via the following targets.
##
## build1-src:
##    A complete source tree is created in subdirectory build1.
## build1-all:
##    The native components are created using ozc.  The component
##    Build1.ozf (and all its imports, transitively) are built
##    using the bootstrap compiler via recursive invocation.
## build1-install:
##    A minimal but working system (only consisting of a compiler,
##    without libraries) is installed to subdirectory build1/install.
##
## Pass 2
## ------
##
## The outcome of this pass is a working system with libraries.
## The components in the libraries carry export signatures.
##
## build2-src:
##    A source tree for the libraries is created in subdirectory build2.
## build2-all:
##    Libraries implemented in Alice are compiled using the system
##    installed in Pass 1.
##
## Pass 3
## ------
##
## This pass finally builds bootstrapped versions of /everything/
## (including toplevel), with runtime type information and a first-class
## compiler.
##
## build3-all:
##    A complete source tree is created in subdirectory build1.
##    Every source file is compiled independently; no recursive
##    invocation happens.
## build3-install:
##    A complete system with all libraries and tools, carrying
##    runtime type information, is installed to subdirectory
##    build3/install.
##
## Installation
## ------------
##
## Globally install all files as found in subdirectory build3/install.
## This is accomplished by the following target.
##
## install:
##    Install the system found in build3/install to the global
##    installation directory.
##
## Typical Use for a Clean Build
## =============================
##
## make distclean; make depend; make all; make install
##

# Configurable directories

SRCTOP = ..
BOOTSTRAPDIR = $(SRCTOP)/bootstrap
FIXPARSER=$(BOOTSTRAPDIR)/fixparser.awk
PREFIX = /opt/alice-devel

# No configuration needed from here

OZEXE = $(shell which oz)
OZHOME = $(OZEXE:%/bin/oz=%)

export OZHOME

VPATH = $(SRCTOP)

ALICEC0 = $(BOOTSTRAPDIR)/alicec # --profile
ALICEC1 = ALICE_HOME=build1/install build1/install/bin/alicec --mozart
ALICEC2 = ALICE_HOME=build1/install build1/install/bin/alicec --mozart

ALICEC_OPTS = --no-warn-conventions --check-intermediate --implicit-import-file -- $(ALICEC_EXTRA_OPTS)
ALICEC_EXTRA_OPTS = 

ALICELINK1 = ALICE_HOME=build1/install build1/install/bin/alicelink
ALICELINK2 = ALICE_HOME=build1/install build1/install/bin/alicelink

ALICELINK_OPTS = -v

ALICEDEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep

OZC = $(OZHOME)/bin/ozc
OZL = $(OZHOME)/bin/ozl
MLLEX = ml-lex
MLYACC = ml-yacc
M4 = m4 -DMOZART -DFUTURE_AWAIT_EITHER_IS_PRIMITIVE

##
## Enumerate All Files Making Up the Source Tree
##

VMSOURCES = VMMain.oz Prebound.oz

LIBSYSTEMNATIVES0 = \
        Config IODesc OS Unix CommandLine Component Debug Socket Rand Reflect \
	Foreign Signal
LIBUTILITYNATIVES0 = \
	Cell Addr
LIBNATIVES = \
	$(LIBSYSTEMNATIVES0:%=lib/system/Unsafe%.ozf) \
	lib/system/UnsafeValue.ozf \
	$(LIBUTILITYNATIVES0:%=lib/utility/Unsafe%.ozf)

LIBFUNDAMENTALSOURCES0 = \
	Pervasive.aml ToplevelTypes.aml ToplevelValues.aml \
	Core.aml ALT-sig.aml ARRAY2-sig.aml \
	ARRAY-sig.aml ARRAY_SLICE-sig.aml \
	BOOL-sig.aml BYTE-sig.aml \
	CHAR-sig.aml EXN-sig.aml FN-sig.aml FUTURE-sig.aml \
	GENERAL-sig.aml HOLE-sig.aml \
	IEEE_REAL-sig.aml INTEGER-sig.aml INT_INF-sig.aml \
	LIST-sig.aml LIST_PAIR-sig.aml \
	LOCK-sig.aml MATH-sig.aml MONO_ARRAY-sig.aml MONO_VECTOR-sig.aml \
	MONO_ARRAY_SLICE-sig.aml MONO_VECTOR_SLICE-sig.aml \
	OPTION-sig.aml PACK_REAL-sig.aml PAIR-sig.aml PROMISE-sig.aml \
	REAL-sig.aml REF-sig.aml STRING-sig.aml \
	STRING_CVT-sig.aml SUBSTRING-sig.aml \
	TEXT-sig.aml THREAD-sig.aml TIME-sig.aml \
	VECTOR-sig.aml VECTOR_PAIR-sig.aml VECTOR_SLICE-sig.aml \
	WORD-sig.aml PACKAGE-sig.aml \
	Alt.aml Array.aml Array2.aml \
	ArraySlice.aml Bool.aml Byte.aml Char.aml \
	CharArray.aml CharArraySlice.aml CharVector.aml CharVectorSlice.aml \
	Exn.aml Fn.aml Future.aml General.aml Hole.aml IEEEReal.aml \
	Int.aml LargeInt.aml FixedInt.aml IntInf.aml \
	LargeReal.aml LargeWord.aml List.aml ListPair.aml Lock.aml Math.aml \
	MkRow.aml MkVector.aml MkArray.aml MkMonoVector.aml MkMonoArray.aml \
	MkSlice.aml MkVectorSlice.aml MkArraySlice.aml \
	MkMonoVectorSlice.aml MkMonoArraySlice.aml \
	MkWord.aml Option.aml Pair.aml \
	Position.aml Promise.aml Real.aml Ref.aml String.aml \
	StringCvt.aml Substring.aml \
	Text.aml Thread.aml Time.aml Toplevel.aml \
	Vector.aml VectorPair.aml VectorSlice.aml WideChar.aml \
	WideCharArray.aml WideCharArraySlice.aml WideCharVector.aml \
	WideCharVectorSlice.aml WideString.aml WideSubstring.aml WideText.aml \
	Word.aml Word8.aml Word8Array.aml Word8ArraySlice.aml \
	Word8Vector.aml Word8VectorSlice.aml Word31.aml Word32.aml \
	Package.aml
LIBDATASOURCES0 = \
	ATOM-sig.aml Atom.aml WideAtom.aml \
	STAMP-sig.aml MkStamp.aml GLOBAL_STAMP-sig.aml GlobalStamp.aml \
	HASHABLE-sig.aml ORDERED-sig.aml \
	SET-sig.aml MAP-sig.aml MkRedBlackSet.aml MkRedBlackMap.aml \
	IMP_SET-sig.aml IMP_MAP-sig.aml MkHashImpSet.aml MkHashImpMap.aml \
	MkRedBlackImpSet.aml MkRedBlackImpMap.aml \
	SITED-sig.aml Sited.aml \
	STACK-sig.aml Stack.aml STREAM-sig.aml Stream.aml \
	LOCK-sig.aml Lock.aml
LIBSYSTEMSOURCES0 = \
	$(LIBSYSTEMNATIVES0:%=Unsafe%.asig) UnsafeValue.asig \
	CONFIG-sig.aml IO-sig.aml IO.aml PRIM_IO-sig.aml PrimIO.aml \
	BinPrimIO.aml TextPrimIO.aml STREAM_IO-sig.aml StreamIO.aml \
	IMPERATIVE_IO-sig.aml ImperativeIO.aml BIN_IO-sig.aml BinIO.aml \
	TEXT_TRANSLATOR-sig.aml MkTextTranslator.aml TEXT_STREAM_IO-sig.aml \
	TEXT_IO-sig.aml TextIO.aml IO_DESC-sig.aml OS_FILE_SYS-sig.aml \
	OS_IO-sig.aml OS_PATH-sig.aml OS_PROCESS-sig.aml OS-sig.aml UNIX-sig.aml \
	COMMAND_LINE-sig.aml COMPONENT-sig.aml DEBUG-sig.aml SOCKET-sig.aml \
	RAND-sig.aml REFLECT-sig.aml FOREIGN-sig.aml DYN_MATCH-sig.aml \
	DynMatch.aml SIGNAL-sig.aml \
	$(LIBSYSTEMNATIVES0:%=%.aml) \
	PRINT-sig.aml Print.aml PICKLE-sig.aml Pickle.aml URL-sig.aml Url.aml \
	HTTP-sig.aml Http.aml HTTP_SERVER-sig.aml HttpServer.aml \
	HTTP_CLIENT-sig.aml HttpClient.aml \
	RESOLVER_HANDLER-sig.aml RESOLVER-sig.aml Resolver.aml \
	URL_MAP-sig.aml UrlMap.aml COMPONENT_MANAGER-sig.aml \
	ComponentManager.aml Boot.aml PP_VALUE-sig.aml PPValue.aml \
	PP_COMPONENT-sig.aml PPComponent.aml
LIBUTILITYSOURCES0 = \
	$(LIBUTILITYNATIVES0:%=Unsafe%.asig) \
	SCOPED_IMP_SET-sig.aml MkScopedImpSet.aml SCOPED_IMP_MAP-sig.aml \
	MkScopedImpMap.aml \
	CELL-sig.aml \
	DEPTH_FIRST_SEARCH-sig.aml MkDepthFirstSearch.aml \
	DYNARRAY-sig.aml DynArray.aml \
	PRETTY_PRINT-sig.aml PrettyPrint.aml \
	RANDOM-sig.aml Random.aml STAT-sig.aml Stat.aml CTRL-sig.aml Ctrl.aml \
	$(LIBUTILITYNATIVES0:%=%.aml)
LIBRTTSOURCES0 = \
	NAME-sig.aml Name.aml LABEL-sig.aml Label.aml LabelSet.aml \
	PATH-sig.aml Path.aml PathSet.aml PathMap.aml \
	FIXITY-sig.aml Fixity.aml TYPE-sig.aml Type.aml INF-sig.aml Inf.aml \
	PERVASIVE_TYPE-sig.aml MkPervasiveType.aml PervasiveType.aml \
	PP_MISC-sig.aml PPMisc.aml PP_PATH-sig.aml PPPath.aml \
	PP_TYPE-sig.aml PPType.aml PP_INF-sig.aml PPInf.aml \
	PP_MISMATCH-sig.aml PPMismatch.aml
LIBMLYACCSOURCES0 = base-sig.aml join.aml lrtable.aml stream.aml parser2.aml

LIBSOURCES0 = \
	$(LIBDATASOURCES0:%=data/%) \
	$(LIBSYSTEMSOURCES0:%=system/%) \
	$(LIBUTILITYSOURCES0:%=utility/%) \
	$(LIBRTTSOURCES0:%=rtt/%) \
	$(LIBMLYACCSOURCES0:%=ml-yacc/%)
LIBSOURCES = \
	$(LIBFUNDAMENTALSOURCES0:%=lib/fundamental/%) \
	$(LIBSOURCES0:%=lib/%)

COMPILERINFRASTRUCTURESOURCES0 = \
	CRASH-sig.aml Crash.aml SOURCE-sig.aml Source.aml ERROR-sig.aml \
	Error.aml SWITCHES-sig.aml MkSwitches.aml CONTEXT-sig.aml \
	REPRESENTATION-sig.aml PHASE-sig.aml PHASE_ERROR-sig.aml \
	ComposePhases.aml MkTracingPhase.aml MkResultDumpingPhase.aml \
	MkContextDumpingPhase.aml EmptyContext.aml TARGET-sig.aml \
	Target.aml BACKEND_SPECIFIC-sig.aml MkHashScopedImpSet.aml \
	MkHashScopedImpMap.aml IntMap.aml StringMap.aml PP_MISC-sig.aml \
	PPMisc.aml
COMPILERINFRASTRUCTURESOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES0:%=compiler/infrastructure/%)

COMPILERCOMMONSOURCES0 = \
	Stamp.aml StampSet.aml StampMap.aml INTERMEDIATE_GRAMMAR-sig.aml \
	MkIntermediateGrammar.aml PP_INTERMEDIATE_GRAMMAR-sig.aml \
	MkPPIntermediateGrammar.aml IntermediateGrammar.aml \
	CHECK_INTERMEDIATE-sig.aml CheckIntermediate.aml
COMPILERCOMMONSOURCES = $(COMPILERCOMMONSOURCES0:%=compiler/common/%)

COMPILERFRONTENDCOMMONSOURCES0 = \
	FIXITY_LIB-sig.aml FixityLib.aml \
	LABEL_LIB-sig.aml LabelLib.aml \
	PATH_LIB-sig.aml PathLib.aml \
	TYPE_LIB-sig.aml TypeLib.aml \
	PERVASIVE_TYPE_LIB-sig.aml PervasiveTypeLib.aml \
	INF_LIB-sig.aml InfLib.aml \
	DYN_MATCH_LIB-sig.aml DynMatchLib.aml \
	NAME_TRANSLATION-sig.aml NameTranslation.aml \
	LABEL_TRANSLATION-sig.aml LabelTranslation.aml \
	TYPE_TRANSLATION-sig.aml TypeTranslation.aml \
	INTERMEDIATE_CONS-sig.aml IntermediateCons.aml \
	ABSTRACT_GRAMMAR-sig.aml MkAbstractGrammar.aml \
	PP_ABSTRACT_GRAMMAR-sig.aml MkPPAbstractGrammar.aml \
	AbstractGrammar.aml TypedGrammar.aml \
	ENV-sig.aml Env.aml USE_ENV-sig.aml UseEnv.aml \
	REWRITE_IMPORTS-sig.aml MkRewriteImports.aml \
	ELABORATION_ERROR-sig.aml ElaborationError.aml \
	ELABORATION_PHASE-sig.aml MkElaborationPhase.aml \
	TRANSLATION_ENV-sig.aml TranslationEnv.aml \
	TRANSLATION_PHASE-sig.aml MkTranslationPhase.aml MkFrontendCommon.aml
COMPILERFRONTENDCOMMONSOURCES = \
	$(COMPILERFRONTENDCOMMONSOURCES0:%=compiler/frontend-common/%)

COMPILERFRONTENDSMLSOURCES0 = \
	LAB-sig.aml Lab.aml ID-sig.aml MkId.aml Ids.aml SCON-sig.aml SCon.aml \
	INPUT_GRAMMAR-sig.aml MkInputGrammar.aml InputGrammar.aml \
	PARSING_ERROR-sig.aml ParsingError.aml ABSTRACTION_ERROR-sig.aml \
	AbstractionError.aml INFIX-sig.aml Infix.aml BIND_ENV-sig.aml \
	BindEnv.aml BIND_ENV_FROM_SIG-sig.aml BindEnvFromSig.aml \
	SHARING-sig.aml Sharing.aml ABSTRACTION_PHASE-sig.aml \
	MkAbstractionPhase.aml DERIVED_FORMS-sig.aml PARSER-sig.aml \
	Parser.aml MkDerivedForms.aml LEXER_ERROR-sig.aml MkLexerError.aml \
	Lexer.aml MkCountPosLexer.aml PARSING_PHASE-sig.aml \
	MkParsingPhase.aml MkFrontendSML.aml
COMPILERFRONTENDSMLSOURCES = \
	$(COMPILERFRONTENDSMLSOURCES0:%=compiler/frontend-sml/%)

COMPILERBACKENDCOMMONSOURCES0 = \
	LABEL_SORT-sig.aml LabelSort.aml FLAT_GRAMMAR-sig.aml \
	FlatGrammar.aml OUTPUT_FLAT_GRAMMAR-sig.aml OutputFlatGrammar.aml \
	ARITY-sig.aml Arity.aml INTERMEDIATE_AUX-sig.aml IntermediateAux.aml \
	SIMPLIFY_MATCH-sig.aml SimplifyMatch.aml SIMPLIFY_REC-sig.aml \
	SimplifyRec.aml FLATTENING_PHASE-sig.aml MkFlatteningPhase.aml \
	PRIM_OPS-sig.aml PrimOps.aml VALUE_PROPAGATION_PHASE-sig.aml \
	MkValuePropagationPhase.aml LIVENESS_ANALYSIS_PHASE-sig.aml \
	MkLivenessAnalysisPhase.aml MkBackendCommon.aml
COMPILERBACKENDCOMMONSOURCES = \
	$(COMPILERBACKENDCOMMONSOURCES0:%=compiler/backend-common/%)

COMPILERBACKENDMOZARTSOURCES0 = \
	CodeGen.oz CodeGenPhase.oz \
	CodeGenPhase.asig MkBackendMozart.aml
COMPILERBACKENDMOZARTSOURCES = \
	$(COMPILERBACKENDMOZARTSOURCES0:%=compiler/backend-mozart/%)

COMPILERBACKENDSEAMSOURCES0 = \
	PRIM_PICKLE-sig.aml PrimPickle.aml VALUE-sig.aml Value.aml \
	ABSTRACT_CODE_GRAMMAR-sig.aml MkAbstractCodeGrammar.aml \
	AbstractCodeGrammar.aml OUTPUT_PICKLE-sig.aml OutputPickle.aml \
	ENVIRONMENT-sig.aml MkEnvironment.aml LIVENESS-sig.aml \
	MkLiveness.aml MkCodeGenContext.aml CODE_GEN_PHASE-sig.aml \
	MkCodeGenPhase.aml MkBackendSeam.aml
COMPILERBACKENDSEAMSOURCES = \
	$(COMPILERBACKENDSEAMSOURCES0:%=compiler/backend-seam/%)

COMPILERTOPSOURCES0 = \
	COMPILER-sig.aml MkCompiler.aml \
	DEPENDENCY-sig.aml Dependency.aml \
	RECURSIVE_COMPILER-sig.aml MkRecursiveCompiler.aml \
	SMLToMozartRecursiveCompiler.aml \
	SMLToSeamRecursiveCompiler.aml \
	RecursiveCompiler.aml \
	OPTION_PARSER-sig.aml MkOptionParser.aml \
	BATCH_COMPILER-sig.aml MkBatchCompiler.aml \
	SMLToMozartBatchCompiler.aml \
	SMLToSeamBatchCompiler.aml \
	INTERACTIVE_TOPLEVEL-sig.aml MkToplevel.aml \
	SMLToMozartToplevel.aml SMLToSeamToplevel.aml
COMPILERTOPSOURCES = $(COMPILERTOPSOURCES0:%=compiler/top/%)

COMPILERSOURCES0 = CompilerMain.aml CompileServerMain.aml ToplevelMain.aml
COMPILERSOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES) $(COMPILERCOMMONSOURCES) \
	$(COMPILERFRONTENDCOMMONSOURCES) $(COMPILERFRONTENDSMLSOURCES) \
	$(COMPILERBACKENDCOMMONSOURCES) $(COMPILERBACKENDMOZARTSOURCES) \
	$(COMPILERBACKENDSEAMSOURCES) \
	$(COMPILERTOPSOURCES) $(COMPILERSOURCES0:%=compiler/%)

TOOLSLINKERSOURCES0 = \
	MY_COMPONENT-sig.aml MyComponent.aml LINK-sig.aml Link.aml \
	PrintSig.aml Main.aml
TOOLSLINKERSOURCES = $(TOOLSLINKERSOURCES0:%=tools/linker/%)

TOOLSDEPENDSOURCES0 = \
	LEXER_ERROR-sig.aml MkLexerError.aml SML_ERROR-sig.aml SMLError.aml \
	SMLLexer.aml MkCountPosLexer.aml SMLParser.grm.aml SML_PARSER-sig.aml \
	PARSER-sig.aml SMLParser.aml ANALYZER-sig.aml MkAnalyzer.aml \
	MAKE_DEPEND-sig.aml MkMakeDepend.aml MakeDepend.aml Main.aml
TOOLSDEPENDSOURCES = $(TOOLSDEPENDSOURCES0:%=tools/depend/%)

TOOLSSOURCES = \
	$(TOOLSLINKERSOURCES) $(TOOLSDEPENDSOURCES)

ALLSOURCES = \
	$(VMSOURCES) $(LIBSOURCES) $(COMPILERSOURCES) $(TOOLSSOURCES)

SOURCEDIRS = \
	lib lib/fundamental lib/data lib/system lib/utility lib/rtt lib/ml-yacc \
	compiler compiler/infrastructure compiler/common compiler/top \
	compiler/frontend-common compiler/frontend-sml \
	compiler/backend-common compiler/backend-mozart \
	compiler/backend-seam \
	tools/linker tools/depend

##
## Main Target and Generally Applicable Rules
##

.PHONY: all depend install clean veryclean distclean
.PHONY: build1-all build1-install
.PHONY: build2-all
.PHONY: build3-all build3-install

all: build3-install

# Build anything using the compiler generated in pass 1
alicec1-%: build1-install
	$(MAKE) ALICEC0=$(ALICEC1) $*

%.lex.sml: %.lex
	$(MLLEX) $<

%.grm.sml: %.grm
	$(MLYACC) $<

%.grm.sig: %.grm.sml ;

########################################################################
### BUILD 1
########################################################################

BUILD1DIRS = build1 $(SOURCEDIRS:%=build1/%)
BUILD1SOURCES = \
	build1/Build1.aml build1/lib/fundamental/RTT.aml \
	$(ALLSOURCES:%=build1/%) \
	build1/tools/Linker.aml
BUILD1OZSOURCES = $(filter %.oz, $(BUILD1SOURCES))

INSTALL1DIRS = build1/install \
	build1/install/bin build1/install/compiler build1/install/tools
INSTALL1TOPFILES = VMMain.ozf
INSTALL1BINFILES = alicerun alicec aliceserv alicelink
INSTALL1COMPILERFILES = CompilerMain.ozf
INSTALL1TOOLSFILES = Linker.ozf
INSTALL1FILES = \
	$(INSTALL1BINFILES:%=build1/install/bin/%) \
	$(BUILD1OZSOURCES:%.oz=%.ozf) build1/install/VMMain.ozf \
	$(INSTALL1COMPILERFILES:%=build1/install/compiler/%) \
	$(INSTALL1TOOLSFILES:%=build1/install/tools/%)

##
## Creating the Source Tree for Build 1
##

build1-src: $(BUILD1DIRS) $(BUILD1SOURCES)

$(BUILD1DIRS):
	mkdir -p $@

Rel1.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../|g' < $< | tr '\n' ' ' | \
	sed 's|(\*END\*)|\n|g' > $@

Rel2.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../../|g' < $< | tr '\n' ' ' | \
	sed 's|(\*END\*)|\n|g' > $@

build1/Build1.aml: Build1.aml
	cp $< $@

build1/lib/fundamental/RTT.aml: lib/rtt/DummyRTT.aml
	cp $< $@

build1/lib/fundamental/%.aml: lib/fundamental/%.aml
	cp $< $@

build1/lib/fundamental/%.aml: lib/fundamental/%.aml.m4
	$(M4) $< > $@

build1/%.oz: vm-mozart/%.oz
	cp $< $@

build1/lib/system/%.asig: lib/system/%.asig Rel2.import
	cat Rel2.import $< > $@

build1/lib/utility/%.asig: lib/utility/%.asig Rel2.import
	cat Rel2.import $< > $@

build1/lib/%.aml: lib/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/lib/%.aml: lib/%.aml.m4 Rel2.import
	cat Rel2.import $< | $(M4) > $@

build1/compiler/%.aml: compiler/%.aml.m4 Rel2.import
	cat Rel2.import $< | $(M4) > $@

build1/compiler/infrastructure/%.aml: compiler/infrastructure/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/common/%.aml: compiler/common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/frontend-common/%.aml: compiler/frontend-common/%.aml \
	Rel2.import
	cat Rel2.import $< > $@

build1/compiler/frontend-sml/%.aml: compiler/frontend-sml/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/frontend-sml/Lexer.lex: compiler/frontend-sml/Lexer.lex
	cp $< $@

build1/compiler/frontend-sml/Lexer.aml: \
	build1/compiler/frontend-sml/Lexer.lex.sml \
	compiler/frontend-sml/Lexer.aml.header Rel2.import
	sed s/Unsafe.//g $< > $@.temp
	cat Rel2.import $(SRCTOP)/compiler/frontend-sml/Lexer.aml.header $@.temp > $@
	rm $@.temp

build1/compiler/frontend-sml/Parser.grm: compiler/frontend-sml/Parser.grm
	cp $< $@

build1/compiler/frontend-sml/PARSER-sig.aml: \
	build1/compiler/frontend-sml/Parser.grm.sig \
	Rel2.import compiler/frontend-sml/PARSER-sig.aml.header $(FIXPARSER)
	(cat Rel2.import $(SRCTOP)/compiler/frontend-sml/PARSER-sig.aml.header && \
	 awk -f $(FIXPARSER) < $<) > $@ || rm $@

build1/compiler/frontend-sml/Parser.aml: \
	build1/compiler/frontend-sml/Parser.grm.sml \
	Rel2.import compiler/frontend-sml/Parser.aml.header
	cat Rel2.import $(SRCTOP)/compiler/frontend-sml/Parser.aml.header $< > $@

build1/compiler/backend-common/%.aml: compiler/backend-common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/backend-mozart/%.aml: compiler/backend-mozart/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/backend-mozart/%.asig: compiler/backend-mozart/%.asig \
	Rel2.import
	cat Rel2.import $< > $@

build1/compiler/backend-seam/%.aml: compiler/backend-seam/%.aml \
	Rel2.import
	cat Rel2.import $< > $@

build1/compiler/top/%.aml: compiler/top/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/ToplevelMain.aml: compiler/ToplevelMainMozart.aml Rel1.import
	cat Rel1.import $< > $@

build1/compiler/%.aml: compiler/%.aml Rel1.import
	cat Rel1.import $< > $@

build1/tools/Linker.aml: tools/linker/Build1Linker.aml Rel1.import
	cat Rel1.import $< > $@

build1/tools/linker/%.aml: tools/linker/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/tools/depend/%.aml: tools/depend/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/tools/depend/SMLLexer.lex: tools/depend/SMLLexer.lex
	cp $< $@

build1/tools/depend/SMLLexer.aml: build1/tools/depend/SMLLexer.lex.sml \
	tools/depend/SMLLexer.aml.header Rel2.import
	sed s/Unsafe.//g $< > $@.temp
	cat Rel2.import $(SRCTOP)/tools/depend/SMLLexer.aml.header $@.temp > $@
	rm $@.temp

build1/tools/depend/SMLParser.grm: tools/depend/SMLParser.grm
	cp $< $@

build1/tools/depend/SML_PARSER-sig.aml: build1/tools/depend/SMLParser.grm.sig \
	Rel2.import tools/depend/SML_PARSER-sig.aml.header $(FIXPARSER)
	(cat Rel2.import $(SRCTOP)/tools/depend/SML_PARSER-sig.aml.header && \
	 awk -f $(FIXPARSER) < $<) > $@ || rm $@

build1/tools/depend/SMLParser.grm.aml: build1/tools/depend/SMLParser.grm.sml \
	Rel2.import tools/depend/SMLParser.grm.aml.header
	cat Rel2.import $(SRCTOP)/tools/depend/SMLParser.grm.aml.header $< > $@

build1/%.oz: %.oz
	cp $< $@

##
## Performing Build 1
##

build1-all: build1-src build1/LinkedVMMain.ozf build1/Build1.ozf

build1/%.ozf: build1/%.oz
	$(OZC) -c $< -o $@

build1/%.ozf: build1/%.aml
	$(ALICEC0) $(ALICEC_OPTS) --recursive-compilation \
	--trace-component-access --no-rtt -c $< -o $@

build1/LinkedVMMain.ozf: $(VMSOURCES:%.oz=build1/%.ozf)
	$(OZL) build1/VMMain.ozf $(VMSOURCES:%.oz=--include=build1/%.ozf) -o $@

##
## Installing Build 1
##

build1-install: build1-all $(INSTALL1DIRS) $(INSTALL1FILES)

$(INSTALL1DIRS):
	mkdir -p -m 775 $@

build1/install/compiler/CompilerMain.ozf:
	(cd build1 && tar cf - `find . -name '*.ozf'`) | \
	(cd build1/install && tar xvf -)

build1/install/VMMain.ozf: build1/LinkedVMMain.ozf
	install -c -m 444 $< $@

build1/install/bin/%: %
	install -c -m 555 $< $@

build1/install/tools/Linker.ozf:

########################################################################
### BUILD 2
########################################################################

BUILD2DIRS = \
	build2 build2/lib build2/lib/fundamental build2/lib/data \
	build2/lib/system build2/lib/utility build2/lib/rtt build2/lib/ml-yacc \
	build2/compiler/frontend-common
BUILD2SOURCES = \
	build2/lib/fundamental/RTT.aml build2/lib/rtt/RTT.aml \
	$(LIBSOURCES:%=build2/%) \
	$(COMPILERFRONTENDCOMMONSOURCES:%=build2/%)

##
## Creating the Source Tree for Build 2
##

build2-src: build1-install $(BUILD2DIRS) $(BUILD2SOURCES)

$(BUILD2DIRS):
	mkdir -p $@

build2/lib/fundamental/RTT.aml: build1/lib/fundamental/RTT.aml
	cp $< $@

build2/%.aml: build1/%.aml
	cp $< $@

build2/%.asig: build1/%.asig
	cp $< $@

build2/alicec.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

$(LIBNATIVES:%=build2/%): build2/%: build1/%
	cp $< $@

##
## Performing Build 2
##

build2-all: build2-src build2/lib/rtt/LinkedRTT.ozf

build2/%.ozf: build2/%.aml
	$(ALICEC1) $(ALICEC_OPTS) --no-rtt -c $< -o $@

build2/lib/rtt/LinkedRTT.ozf: build2/lib/rtt/RTT.ozf
	$(ALICELINK1) $(ALICELINK_OPTS) build2/lib/rtt/RTT \
	--include build2 --rewrite "build2/lib/?{x}=../?{x}" -o $@

$(LIBNATIVES:%.ozf=build2/%.asig): %.asig: %.ozf

########################################################################
### BUILD 3
########################################################################

BUILD3DIRS = build3 $(SOURCEDIRS:%=build3/%)
BUILD3SOURCES = $(ALLSOURCES:%=build3/%)

INSTALL3DIRS = \
	build3/install \
	build3/install/bin \
	build3/install/lib \
	build3/install/lib/fundamental \
	build3/install/lib/data \
	build3/install/lib/system \
	build3/install/lib/utility \
	build3/install/lib/rtt \
	build3/install/lib/ml-yacc \
	build3/install/compiler \
	build3/install/compiler/top \
	build3/install/tools
INSTALL3TOPFILES = $(INSTALL1TOPFILES) alicec.import COPYING
INSTALL3BINFILES = $(INSTALL1BINFILES) alicedep
INSTALL3LIBFILES = \
	$(filter %.ozf, $(LIBSOURCES0:%.aml=%.ozf)) \
	$(filter %.ozf, $(LIBSOURCES0:%.asig=%.ozf)) system/UnsafeValue.asig \
	fundamental/Pervasive.ozf fundamental/Core.ozf fundamental/RTT.ozf \
	fundamental/PACKAGE-sig.ozf fundamental/Package.ozf
INSTALL3COMPILERFILES = $(INSTALL1COMPILERFILES) ToplevelMain.ozf
INSTALL3COMPILERTOPFILES = \
	RecursiveCompiler.ozf
INSTALL3TOOLSFILES = Linker.ozf Depend.ozf
INSTALL3FILES = \
	$(INSTALL3TOPFILES:%=build3/install/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%) \
	$(INSTALL3LIBFILES:%=build3/install/lib/%) \
	$(INSTALL3COMPILERFILES:%=build3/install/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=build3/install/compiler/top/%) \
	$(INSTALL3TOOLSFILES:%=build3/install/tools/%)

##
## Creating the Source Tree for Build 3
##

$(BUILD3DIRS):
	mkdir -p $@

$(BUILD3SOURCES): build3/%: build1/%
	cp $< $@

##
## Performing Build 3
##

build3-all: build2-all $(BUILD3DIRS) $(BUILD3SOURCES) \
	$(LIBNATIVES:%=build3/%) \
	build3/lib/fundamental/Core.ozf \
	build3/compiler/LinkedCompilerMain.ozf \
	build3/compiler/LinkedCompileServerMain.ozf \
	build3/compiler/LinkedToplevelMain.ozf \
	build3/compiler/top/LinkedRecursiveCompiler.ozf \
	build3/LinkedVMMain.ozf

build3/lib/fundamental/RTT.ozf: build2/lib/rtt/LinkedRTT.ozf
	cp $< $@

build3/%.ozf: build3/%.oz
	$(OZC) -c $< -o $@

$(LIBNATIVES:%=build3/%): build3/lib/%.ozf: lib/%.oz
	$(OZC) -c $< -o $@

build3/alicec.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

build3/lib/fundamental/Pervasive.ozf: build3/lib/fundamental/Pervasive.aml
	$(ALICEC2) $(ALICEC_OPTS) --no-check-intermediate -c $< -o $@

build3/%.ozf: build3/%.aml
	$(ALICEC2) $(ALICEC_OPTS) -c $< -o $@

build3/LinkedVMMain.ozf: $(VMSOURCES:%.oz=build3/%.ozf)
	$(OZL) build3/VMMain.ozf $(VMSOURCES:%.oz=--include=build3/%.ozf) -o $@

build3/lib/fundamental/LinkedPervasive.ozf: \
	build3/lib/fundamental/Pervasive.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../../?{x}" \
	--include build3 \
	build3/lib/fundamental/Pervasive -o $@

build3/compiler/LinkedCompilerMain.ozf: build3/compiler/CompilerMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../?{x}" \
	--exclude build3/compiler/top/RecursiveCompiler \
	--include build3/compiler \
	build3/compiler/CompilerMain -o $@

build3/compiler/LinkedCompileServerMain.ozf: \
	build3/compiler/CompileServerMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../?{x}" \
	--exclude build3/compiler/top/RecursiveCompiler \
	--include build3/compiler \
	build3/compiler/CompileServerMain -o $@

build3/compiler/LinkedToplevelMain.ozf: build3/compiler/ToplevelMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../?{x}" \
	--exclude build3/compiler/top/RecursiveCompiler \
	--include build3/compiler \
	build3/compiler/ToplevelMain -o $@

build3/compiler/top/LinkedRecursiveCompiler.ozf: \
	build3/compiler/top/RecursiveCompiler.ozf \
	build3/compiler/backend-mozart/CodeGenPhase.ozf \
	build3/compiler/backend-mozart/CodeGen.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}"="../../?{x}" \
	--include build3/compiler \
	build3/compiler/top/RecursiveCompiler -o $@

build3/compiler/top/LinkedSMLToSeamRecursiveCompiler.ozf: \
	build3/compiler/top/SMLToSeamRecursiveCompiler.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../../?{x}" \
	--include build3/compiler \
	build3/compiler/top/SMLToSeamRecursiveCompiler -o $@

build3/tools/linker/LinkedMain.ozf: $(TOOLSLINKERSOURCES:%.aml=build3/%.ozf)
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../?{x}" \
	--include build3/tools/linker \
	build3/tools/linker/Main -o $@

build3/tools/depend/LinkedMain.ozf: $(TOOLSDEPENDSOURCES:%.aml=build3/%.ozf)
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite "build3/?{x}=../?{x}" \
	--include build3/tools/depend \
	build3/tools/depend/Main -o $@

##
## Installing Build 3
##

build3-install: build3-all $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALL3DIRS):
	mkdir -p -m 775 $@

build3/install/VMMain.ozf: build3/LinkedVMMain.ozf
	install -c -m 444 $< $@

build3/install/%.import: build3/%.import
	install -c -m 444 $< $@

build3/install/COPYING: ../COPYING
	install -c -m 444 $< $@

build3/install/bin/%: %
	install -c -m 555 $< $@

build3/install/lib/fundamental/Pervasive.ozf: \
	build3/lib/fundamental/LinkedPervasive.ozf
	install -c -m 444 $< $@

build3/install/lib/fundamental/Core.ozf: \
	build3/lib/fundamental/Core.ozf
	install -c -m 444 $< $@

build3/install/lib/fundamental/RTT.ozf: \
	build3/lib/fundamental/RTT.ozf
	install -c -m 444 $< $@

build3/install/lib/%: build3/lib/%
	install -c -m 444 $< $@

build3/install/compiler/%: build3/compiler/Linked%
	install -c -m 444 $< $@

build3/install/compiler/top/%: build3/compiler/top/Linked%
	install -c -m 444 $< $@

build3/install/tools/Linker.ozf: build3/tools/linker/LinkedMain.ozf
	install -c -m 444 $< $@

build3/install/tools/Depend.ozf: build3/tools/depend/LinkedMain.ozf
	install -c -m 444 $< $@

########################################################################
### THINGS OF A GLOBAL NATURE
########################################################################

##
## Automatic Dependency Management
##

BUILD2ALICESOURCES = $(filter %.aml, $(BUILD2SOURCES))
BUILD3ALICESOURCES = $(filter %.aml, $(BUILD3SOURCES))

depend: $(BUILD1DIRS) $(BUILD2DIRS) $(BUILD3DIRS) \
	$(BUILD2SOURCES) $(BUILD3SOURCES)
	$(ALICEDEP) --mozart $(BUILD2ALICESOURCES) > Makefile.depend
	$(ALICEDEP) --mozart $(BUILD3ALICESOURCES) >> Makefile.depend

-include Makefile.depend

##
## Global Installation
##

INSTALLDIRS = \
	$(PREFIX) \
	$(PREFIX)/bin \
	$(PREFIX)/lib \
	$(PREFIX)/lib/fundamental \
	$(PREFIX)/lib/data \
	$(PREFIX)/lib/system \
	$(PREFIX)/lib/utility \
	$(PREFIX)/lib/rtt \
	$(PREFIX)/lib/ml-yacc \
	$(PREFIX)/compiler \
	$(PREFIX)/compiler/top \
	$(PREFIX)/tools
INSTALLFILES = \
	$(INSTALL3TOPFILES:%=$(PREFIX)/%) \
	$(INSTALL3BINFILES:%=$(PREFIX)/bin/%) \
	$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%) \
	$(INSTALL3COMPILERFILES:%=$(PREFIX)/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=$(PREFIX)/compiler/top/%) \
	$(INSTALL3TOOLSFILES:%=$(PREFIX)/tools/%)

install: build3-install $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p -m 775 $@

$(INSTALL3TOPFILES:%=$(PREFIX)/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3BINFILES:%=$(PREFIX)/bin/%): $(PREFIX)/%: build3/install/%
	install -c -m 555 $< $@

$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3COMPILERFILES:%=$(PREFIX)/compiler/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3COMPILERTOPFILES:%=$(PREFIX)/compiler/top/%): $(PREFIX)/%: \
	build3/install/%
	install -c -m 444 $< $@

$(INSTALL3TOOLSFILES:%=$(PREFIX)/tools/%): $(PREFIX)/%: \
	build3/install/%
	install -c -m 444 $< $@

##
## Cleaning up
##

clean:
	-rm -f Rel1.import Rel2.import
	-rm -rf build1 build2 build3

veryclean: clean

distclean: veryclean
	-rm -f Makefile.depend
