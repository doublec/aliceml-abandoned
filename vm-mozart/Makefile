###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 1999-2002
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
##    Creates the file Makefile.depend, which contains the dependencies
##    needed for Pass 3.
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
## The goal is to build a compiler using the hybrid SML/NJ+Mozart compiler.
## This is performed via the following targets.
##
## build1-src:
##    A complete source tree is created in subdirectory build1.
## build1-all:
##    The native components are created using ozc.  The component
##    Build1.ozf (and all its imports, transitively) are built
##    using the hybrid compiler via recursive invocation.
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
## build2-install:
##    A system with libraries (as far as they are needed to build
##    a system with runtime type information) is installed to
##    subdirectory build2/install.
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
BOOTSTRAPDIR = ../bootstrap
OZHOME = /opt/mozart-devel
PREFIX = /opt/stockhausen-devel

# No configuration needed from here

VPATH = $(SRCTOP)

ALICEC0 = $(BOOTSTRAPDIR)/alicec # --profile
ALICEC1 = STOCKHOME=build1/install build1/install/bin/alicec
ALICEC2 = STOCKHOME=build2/install build2/install/bin/alicec

ALICEC_OPTS = \
	--check-intermediate \
	--no-dump-elaboration-sig --implicit-import-file - $(ALICEC_EXTRA_OPTS)
ALICEC_EXTRA_OPTS = --dump-target

ALICELINK0 = OZ_LOAD=pattern='?{x}'='?{x}'.ozf:cache=$(OZHOME)/cache/ ozl
ALICELINK1 = STOCKHOME=build1/install build1/install/bin/alicelink
ALICELINK2 = STOCKHOME=build2/install build2/install/bin/alicelink

ALICELINK_OPTS = -z9 --sequential

ALICEDEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep

OZC = ozc
MLLEX = ml-lex
MLYACC = ml-yacc

##
## Enumerate All Files Making Up the Source Tree
##

VMSOURCES = VMMain.oz

LIBSYSTEMNATIVES0 = \
	Config IO OS Unix CommandLine Component Debug Socket Rand Reflect
LIBUTILITYNATIVES0 = \
	MkRefMap Addr
LIBNATIVES = \
	$(LIBSYSTEMNATIVES0:%=lib/system/Unsafe%.ozf) \
	$(LIBUTILITYNATIVES0:%=lib/utility/Unsafe%.ozf)

LIBFUNDAMENTALSOURCES0 = \
	Pervasive.aml ToplevelTypes.aml ToplevelValues.aml \
	Fundamental.aml ALT-sig.aml ARRAY-sig.aml BOOL-sig.aml \
	CHAR-sig.aml FUTURE-sig.aml GENERAL-sig.aml HOLE-sig.aml \
	IEEE_REAL-sig.aml INTEGER-sig.aml LIST-sig.aml LIST_PAIR-sig.aml \
	LOCK-sig.aml MATH-sig.aml MONO_VECTOR-sig.aml OPTION-sig.aml \
	PACK_REAL-sig.aml PAIR-sig.aml PROMISE-sig.aml REAL-sig.aml \
	REF-sig.aml STREAM-sig.aml STRING-sig.aml STRING_CVT-sig.aml \
	SUBSTRING-sig.aml THREAD-sig.aml TIME-sig.aml VECTOR-sig.aml \
	VECTOR_PAIR-sig.aml WORD-sig.aml \
	Alt.aml Array.aml Bool.aml Char.aml CharVector.aml Future.aml \
	General.aml Hole.aml IEEEReal.aml Int.aml LargeInt.aml LargeReal.aml \
	LargeWord.aml List.aml ListPair.aml Lock.aml Math.aml MkWord.aml \
	Option.aml Pair.aml Position.aml Promise.aml Real.aml Ref.aml \
	Stream.aml String.aml StringCvt.aml Substring.aml Thread.aml Time.aml \
	Toplevel.aml Vector.aml VectorPair.aml WideChar.aml WideString.aml \
	Word.aml Word8.aml Word8Vector.aml PACKAGE-sig.aml Package.aml \
	ByNeed.aml
LIBSYSTEMSOURCES0 = \
	$(LIBSYSTEMNATIVES0:%=Unsafe%.asig) \
	CONFIG-sig.aml IO-sig.aml BIN_IO-sig.aml BinIO.aml \
	TEXT_IO-sig.aml TextIO.aml OS_FILE_SYS-sig.aml OS_PROCESS-sig.aml \
	OS-sig.aml UNIX-sig.aml COMMAND_LINE-sig.aml COMPONENT-sig.aml \
	DEBUG-sig.aml SOCKET-sig.aml RAND-sig.aml REFLECT-sig.aml \
	$(LIBSYSTEMNATIVES0:%=%.aml) \
	PICKLE-sig.aml Pickle.aml SIGNATURE-sig.aml Signature.aml \
	HTTP-sig.aml Http.aml HTTP_SERVER-sig.aml HttpServer.aml \
	HTTP_CLIENT-sig.aml HttpClient.aml \
	RESOLVER-sig.aml Resolver.aml URL_MAP-sig.aml UrlMap.aml \
	COMPONENT_MANAGER-sig.aml ComponentManager.aml Boot.aml
LIBUTILITYSOURCES0 = \
	$(LIBUTILITYNATIVES0:%=Unsafe%.asig) \
	ASSERT-sig.aml Assert.aml STAMP-sig.aml MkStamp.aml \
	GLOBAL_STAMP-sig.aml GlobalStamp.aml URL-sig.aml Url.aml \
	HASH_KEY-sig.aml IntHashKey.aml LargeIntHashKey.aml StringHashKey.aml \
	IMP_SET-sig.aml MkHashImpSet.aml IMP_MAP-sig.aml MkHashImpMap.aml \
	SIMPLE_IMP_MAP-sig.aml \
	SCOPED_IMP_SET-sig.aml MkScopedImpSet.aml SCOPED_IMP_MAP-sig.aml \
	MkScopedImpMap.aml LIST_SORT-sig.aml MkListSort.aml \
	CELL-sig.aml Cell.aml \
	DEPTH_FIRST_SEARCH-sig.aml MkDepthFirstSearch.aml \
	PRETTY_PRINT-sig.aml PrettyPrint.aml \
	RANDOM-sig.aml Random.aml STAT-sig.aml Stat.aml \
	$(LIBUTILITYNATIVES0:%=%.aml)
LIBRTTSOURCES0 = \
	NAME-sig.aml Name.aml LABEL-sig.aml Label.aml PATH-sig.aml Path.aml \
	PathSet.aml PathMap.aml FIXITY-sig.aml Fixity.aml TYPE-sig.aml \
	Type.aml INF-sig.aml Inf.aml PERVASIVE_TYPE-sig.aml \
	MkPervasiveType.aml PervasiveType.aml PP_MISC-sig.aml PPMisc.aml \
	PP_PATH-sig.aml PPPath.aml PP_TYPE-sig.aml PPType.aml PP_INF-sig.aml \
	PPInf.aml RTT.aml
LIBMLYACCSOURCES0 = base-sig.aml join.aml lrtable.aml stream.aml parser2.aml

LIBSOURCES0 = \
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
	Target.aml MkHashScopedImpSet.aml MkHashScopedImpMap.aml \
	StringMap.aml PP_MISC-sig.aml PPMisc.aml
COMPILERINFRASTRUCTURESOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES0:%=compiler/infrastructure/%)

COMPILERCOMMONSOURCES0 = \
	Stamp.aml StampSet.aml StampMap.aml INTERMEDIATE_GRAMMAR-sig.aml \
	MkIntermediateGrammar.aml PP_INTERMEDIATE_GRAMMAR-sig.aml \
	MkPPIntermediateGrammar.aml IntermediateGrammar.aml \
	CHECK_INTERMEDIATE-sig.aml CheckIntermediate.aml
COMPILERCOMMONSOURCES = $(COMPILERCOMMONSOURCES0:%=compiler/common/%)

COMPILERFRONTENDCOMMONSOURCES0 = \
	NAME_TRANSLATION-sig.aml NameTranslation.aml \
	LABEL_TRANSLATION-sig.aml LabelTranslation.aml \
	PATH_TRANSLATION-sig.aml PathTranslation.aml \
	TYPE_TRANSLATION-sig.aml TypeTranslation.aml \
	PERVASIVE_TYPE_TRANSLATION-sig.aml PervasiveTypeTranslation.aml \
	FIXITY_TRANSLATION-sig.aml FixityTranslation.aml \
	INF_TRANSLATION-sig.aml InfTranslation.aml \
	ABSTRACT_GRAMMAR-sig.aml MkAbstractGrammar.aml \
	PP_ABSTRACT_GRAMMAR-sig.aml MkPPAbstractGrammar.aml \
	AbstractGrammar.aml TypedGrammar.aml \
	ENV-sig.aml Env.aml USE_ENV-sig.aml UseEnv.aml \
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
	Parser.aml DerivedForms.aml LEXER_ERROR-sig.aml MkLexerError.aml \
	Lexer.aml MkCountPosLexer.aml PARSING_PHASE-sig.aml \
	MkParsingPhase.aml MkFrontendSML.aml
COMPILERFRONTENDSMLSOURCES = \
	$(COMPILERFRONTENDSMLSOURCES0:%=compiler/frontend-sml/%)

COMPILERBACKENDCOMMONSOURCES0 = \
	LABEL_SORT-sig.aml MkLabelSort.aml LabelSort.aml FLAT_GRAMMAR-sig.aml \
	FlatGrammar.aml OUTPUT_FLAT_GRAMMAR-sig.aml OutputFlatGrammar.aml \
	ARITY-sig.aml Arity.aml INTERMEDIATE_AUX-sig.aml IntermediateAux.aml \
	SIMPLIFY_MATCH-sig.aml SimplifyMatch.aml SIMPLIFY_REC-sig.aml \
	SimplifyRec.aml FLATTENING_PHASE-sig.aml MkFlatteningPhase.aml \
	PRIM_OPS-sig.aml PrimOps.aml VALUE_PROPAGATION_PHASE-sig.aml \
	ValuePropagationPhase.aml LIVENESS_ANALYSIS_PHASE-sig.aml \
	MkLivenessAnalysisPhase.aml MkBackendCommon.aml
COMPILERBACKENDCOMMONSOURCES = \
	$(COMPILERBACKENDCOMMONSOURCES0:%=compiler/backend-common/%)

COMPILERBACKENDMOZARTSOURCES0 = \
	Prebound.oz CodeGen.oz CodeGenPhase.oz \
	CodeGenPhase.asig MkBackendMozart.aml
COMPILERBACKENDMOZARTSOURCES = \
	$(COMPILERBACKENDMOZARTSOURCES0:%=compiler/backend-mozart/%)

COMPILERBACKENDDOTNETSOURCES0 = \
	SKELETON-sig.aml Skeleton.aml IL-sig.aml IL.aml System.aml \
	Alice.aml BUILTINS-sig.aml Builtins.aml CODE_STORE-sig.aml \
	CodeStore.aml CODE_GEN_PHASE-sig.aml CodeGenPhase.aml \
	MkBackendDotNet.aml
COMPILERBACKENDDOTNETSOURCES = \
	$(COMPILERBACKENDDOTNETSOURCES0:%=compiler/backend-dotnet/%)

COMPILERBACKENDSTOCKWERKSOURCES0 = \
	PRIM_PICKLE-sig.aml PrimPickle.aml VALUE-sig.aml Value.aml \
	ABSTRACT_CODE_GRAMMAR-sig.aml MkAbstractCodeGrammar.aml \
	AbstractCodeGrammar.aml OUTPUT_PICKLE-sig.aml OutputPickle.aml \
	ENVIRONMENT-sig.aml MkEnvironment.aml CODE_GEN_PHASE-sig.aml \
	MkCodeGenContext.aml MkCodeGenPhase.aml MkBackendStockwerk.aml
COMPILERBACKENDSTOCKWERKSOURCES = \
	$(COMPILERBACKENDSTOCKWERKSOURCES0:%=compiler/backend-stockwerk/%)

COMPILERTOPSOURCES0 = \
	COMPILER-sig.aml MkCompiler.aml SignTable.aml \
	RECURSIVE_COMPILER-sig.aml MkRecursiveCompiler.aml \
	SMLToMozartRecursiveCompiler.aml \
	SMLToDotNetRecursiveCompiler.aml \
	SMLToStockwerkRecursiveCompiler.aml \
	OPTION_PARSER-sig.aml MkOptionParser.aml \
	BATCH_COMPILER-sig.aml MkBatchCompiler.aml \
	SMLToMozartBatchCompiler.aml \
	SMLToDotNetBatchCompiler.aml \
	SMLToStockwerkBatchCompiler.aml \
	INTERACTIVE_TOPLEVEL-sig.aml MkToplevel.aml \
	SMLToMozartToplevel.aml SMLToStockwerkToplevel.aml
COMPILERTOPSOURCES = $(COMPILERTOPSOURCES0:%=compiler/top/%)

COMPILERSOURCES0 = CompilerMain.aml CompileServerMain.aml ToplevelMain.aml
COMPILERSOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES) $(COMPILERCOMMONSOURCES) \
	$(COMPILERFRONTENDCOMMONSOURCES) $(COMPILERFRONTENDSMLSOURCES) \
	$(COMPILERBACKENDCOMMONSOURCES) $(COMPILERBACKENDMOZARTSOURCES) \
	$(COMPILERBACKENDDOTNETSOURCES) $(COMPILERBACKENDSTOCKWERKSOURCES) \
	$(COMPILERTOPSOURCES) $(COMPILERSOURCES0:%=compiler/%)

LINKERSOURCES0 = \
	MY_COMPONENT-sig.aml MyComponent.aml LINK-sig.aml Link.aml Main.aml
LINKERSOURCES = $(LINKERSOURCES0:%=linker/%)

ALLSOURCES = $(VMSOURCES) $(LIBSOURCES) $(COMPILERSOURCES) $(LINKERSOURCES)

SOURCEDIRS = \
	lib lib/fundamental lib/system lib/utility lib/rtt lib/ml-yacc \
	compiler compiler/infrastructure compiler/common compiler/top \
	compiler/frontend-common compiler/frontend-sml \
	compiler/backend-common compiler/backend-mozart \
	compiler/backend-dotnet compiler/backend-stockwerk linker

##
## Main Target and Generally Applicable Rules
##

.PHONY: all depend install clean veryclean distclean
.PHONY: build1-all build1-install
.PHONY: build2-all build2-install
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
	$(ALLSOURCES:%=build1/%)

INSTALL1DIRS = build1/install build1/install/bin \
	build1/install/compiler build1/install/linker
INSTALL1TOPFILES = VMMain.ozf
INSTALL1BINFILES = alicerun alicec aliceserv alicelink
INSTALL1COMPILERFILES = CompilerMain.ozf CompileServerMain.ozf
INSTALL1FILES = \
	$(INSTALL1TOPFILES:%=build1/install/%) \
	$(INSTALL1BINFILES:%=build1/install/bin/%) \
	$(INSTALL1COMPILERFILES:%=build1/install/compiler/%)

##
## Creating the Source Tree for Build 1
##

build1-src: $(BUILD1DIRS) $(BUILD1SOURCES)

$(BUILD1DIRS):
	mkdir -p $@

Rel1.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../|g' < $< > $@

Rel2.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../../|g' < $< > $@

build1/Build1.aml: Build1.aml
	cp $< $@

build1/lib/fundamental/RTT.aml: lib/fundamental/DummyRTT.aml
	cp $< $@

build1/lib/fundamental/%.aml: lib/fundamental/%.aml
	cp $< $@

build1/%.oz: vm-mozart/%.oz
	cp $< $@

build1/lib/system/%.asig: lib/system/%.asig Rel2.import
	cat Rel2.import $< > $@

build1/lib/utility/%.asig: lib/utility/%.asig Rel2.import
	cat Rel2.import $< > $@

build1/lib/%.aml: lib/%.aml Rel2.import
	cat Rel2.import $< > $@

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
	cat Rel2.import $(SRCTOP)/compiler/frontend-sml/Lexer.aml.header $< > $@

build1/compiler/frontend-sml/Parser.grm: compiler/frontend-sml/Parser.grm
	cp $< $@

build1/compiler/frontend-sml/PARSER-sig.aml: \
	build1/compiler/frontend-sml/Parser.grm.sig \
	Rel2.import compiler/frontend-sml/PARSER-sig.aml.header fixparser.awk
	(cat Rel2.import $(SRCTOP)/compiler/frontend-sml/PARSER-sig.aml.header && \
	 awk -f $(SRCTOP)/fixparser.awk < $<) > $@ || rm $@

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

build1/compiler/backend-dotnet/%.aml: compiler/backend-dotnet/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/backend-stockwerk/%.aml: compiler/backend-stockwerk/%.aml \
	Rel2.import
	cat Rel2.import $< > $@

build1/compiler/top/%.aml: compiler/top/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/%.aml: compiler/%.aml Rel1.import
	cat Rel1.import $< > $@

build1/linker/%.aml: linker/%.aml Rel1.import
	cat Rel1.import $< > $@

build1/%.oz: %.oz
	cp $< $@

##
## Performing Build 1
##

build1-all: build1-src \
	build1/LinkedVMMain.ozf build1/compiler/LinkedCompilerMain.ozf \
	build1/compiler/LinkedCompileServerMain.ozf

build1/%.ozf: build1/%.oz
	$(OZC) -c $< -o $@

build1/%.ozf: build1/%.aml
	$(ALICEC0) $(ALICEC_OPTS) --recursive-compilation \
	--trace-component-access --no-rtt -c $< -o $@

BUILD1OZSOURCES = $(filter %.oz, $(BUILD1SOURCES))
BUILD1OZLINPUTS = \
	$(LIBNATIVES:%=build1/%) \
	$(BUILD1OZSOURCES:%.oz=%.ozf) \
	build1/Build1.ozf

build1/LinkedVMMain.ozf: build1/VMMain.ozf $(BUILD1OZLINPUTS)
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. $< -o $@

build1/compiler/LinkedCompilerMain.ozf: $(BUILD1OZLINPUTS)
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. \
	build1/compiler/CompilerMain.ozf -o $@

build1/compiler/LinkedCompileServerMain.ozf: $(BUILD1OZLINPUTS)
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. \
	build1/compiler/CompileServerMain.ozf -o $@

##
## Installing Build 1
##

build1-install: build1-all $(INSTALL1DIRS) $(INSTALL1FILES)

$(INSTALL1DIRS):
	mkdir -p $@

build1/install/VMMain.ozf: build1/LinkedVMMain.ozf
	install -c -m 444 $< $@

build1/install/compiler/CompilerMain.ozf: \
	build1/compiler/LinkedCompilerMain.ozf
	install -c -m 444 $< $@

build1/install/compiler/CompileServerMain.ozf: \
	build1/compiler/LinkedCompileServerMain.ozf
	install -c -m 444 $< $@

build1/install/bin/%: %
	install -c -m 555 $< $@

########################################################################
### BUILD 2
########################################################################

BUILD2DIRS = \
	build2 build2/lib build2/lib/fundamental build2/lib/system \
	build2/lib/utility build2/lib/rtt build2/lib/ml-yacc
BUILD2SOURCES = build2/lib/fundamental/RTT.aml $(LIBSOURCES:%=build2/%)

INSTALL2DIRS = build2/install build2/install/bin \
	build2/install/lib build2/install/lib/fundamental \
	build2/install/lib/system build2/install/lib/utility \
	build2/install/lib/rtt build2/install/lib/ml-yacc \
	build2/install/compiler
INSTALL2TOPFILES = VMMain.ozf alicec.import
INSTALL2BINFILES = alicerun alicec aliceserv alicelink
INSTALL2LIBFILES = rtt/RTT.ozf
INSTALL2COMPILERFILES = CompilerMain.ozf CompileServerMain.ozf
INSTALL2FILES = \
	$(INSTALL2TOPFILES:%=build2/install/%) \
	$(INSTALL2BINFILES:%=build2/install/bin/%) \
	$(INSTALL2LIBFILES:%=build2/install/lib/%) \
	$(INSTALL2COMPILERFILES:%=build2/install/compiler/%)

##
## Creating the Source Tree for Build 2
##

build2-src: build1-install $(BUILD2DIRS) $(BUILD2SOURCES) \

$(BUILD2DIRS):
	mkdir -p $@

build2/lib/fundamental/RTT.aml: build1/lib/fundamental/RTT.aml
	cp $< $@

build2/%.aml: build1/%.aml
	cp $< $@

build2/%.asig: build1/%.asig
	cp $< $@

build2/LinkedVMMain.ozf build2/compiler/LinkedCompilerMain.ozf \
	build2/compiler/LinkedCompileServerMain.ozf: build2/%: build1/%
	cp $< $@

build2/alicec.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

$(LIBNATIVES:%=build2/%): build2/%: build1/%
	cp $< $@

$(LIBNATIVES:%.ozf=build2/%.asig): build2/%: build1/%
	cp $< $@

##
## Performing Build 2
##

build2-all: build2-src build2/lib/rtt/LinkedRTT.ozf

build2/%.ozf: build2/%.aml
	$(ALICEC1) $(ALICEC_OPTS) --no-rtt -c $< -o $@

build2/lib/rtt/LinkedRTT.ozf: build2/lib/rtt/RTT.ozf
	$(ALICELINK1) $(ALICELINK_OPTS) --include=.. $< -o $@

$(LIBNATIVES:%.ozf=build2/%.asig): %.asig: %.ozf

##
## Installing Build 2
##

build2-install: build2-all $(INSTALL2DIRS) $(INSTALL2FILES)

$(INSTALL2DIRS):
	mkdir -p $@

build2/install/VMMain.ozf: build1/install/VMMain.ozf
	install -c -m 444 $< $@

build2/install/%.import: build2/%.import
	install -c -m 444 $< $@

$(INSTALL2BINFILES:%=build2/install/bin/%): build2/%: build1/%
	install -c -m 555 $< $@

$(INSTALL2LIBFILES:%=build2/install/lib/%): build2/install/%: build2/%
	install -c -m 444 $< $@

build2/install/compiler/CompilerMain.ozf: \
	build1/install/compiler/CompilerMain.ozf
	install -c -m 444 $< $@

build2/install/compiler/CompileServerMain.ozf: \
	build1/install/compiler/CompileServerMain.ozf
	install -c -m 444 $< $@

########################################################################
### BUILD 3
########################################################################

BUILD3DIRS = build3 $(SOURCEDIRS:%=build3/%)
BUILD3SOURCES = $(ALLSOURCES:%=build3/%)

INSTALL3DIRS = build3/install build3/install/bin \
	build3/install/lib build3/install/lib/fundamental \
	build3/install/lib/system build3/install/lib/utility \
	build3/install/lib/rtt build3/install/lib/ml-yacc \
	build3/install/compiler build3/install/compiler/top \
	build3/install/linker
INSTALL3TOPFILES = $(INSTALL2TOPFILES) alice.import
INSTALL3BINFILES = $(INSTALL2BINFILES) alice
INSTALL3LIBFILES = \
	$(filter %.ozf, $(LIBSOURCES0:%.aml=%.ozf)) \
	$(filter %.ozf, $(LIBSOURCES0:%.asig=%.ozf)) \
	fundamental/Fundamental.ozf
INSTALL3COMPILERFILES = $(INSTALL2COMPILERFILES) ToplevelMain.ozf
INSTALL3COMPILERTOPFILES = \
	SMLToMozartRecursiveCompiler.ozf \
	SMLToDotNetRecursiveCompiler.ozf
INSTALL3LINKERFILES = Main.ozf
INSTALL3FILES = \
	$(INSTALL3TOPFILES:%=build3/install/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%.bat) \
	$(INSTALL3LIBFILES:%=build3/install/lib/%) \
	$(INSTALL3COMPILERFILES:%=build3/install/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=build3/install/compiler/top/%) \
	$(INSTALL3LINKERFILES:%=build3/install/linker/%)

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

build3-all: build2-install $(BUILD3DIRS) $(BUILD3SOURCES) \
	$(LIBNATIVES:%=build3/%) \
	build3/lib/fundamental/LinkedFundamental.ozf \
	build3/compiler/LinkedCompilerMain.ozf \
	build3/compiler/LinkedCompileServerMain.ozf \
	build3/compiler/LinkedToplevelMain.ozf \
	build3/compiler/top/LinkedSMLToMozartRecursiveCompiler.ozf \
	build3/compiler/top/LinkedSMLToDotNetRecursiveCompiler.ozf \
	build3/LinkedVMMain.ozf

build3/lib/fundamental/RTT.ozf: build2/lib/rtt/LinkedRTT.ozf
	cp $< $@

build3/%.ozf: build3/%.oz
	$(OZC) -c $< -o $@

$(LIBNATIVES:%=build3/%): \
	build3/lib/%.ozf: lib/%.oz
	$(OZC) -c $< -o $@

build3/alicec.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

build3/alice.import: lib/fundamental/Fundamental.import lib/Other.import \
	compiler/top/Toplevel.import
	cat $^ > $@

build3/%.ozf: build3/%.aml
	$(ALICEC2) $(ALICEC_OPTS) -c $< -o $@

BUILD3 := $(shell pwd)/build3
TOP3 = $(BUILD3)/compiler/top

build3/lib/fundamental/LinkedFundamental.ozf: \
	build3/lib/fundamental/Fundamental.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) build3/lib/fundamental/Fundamental \
	-o $@

build3/LinkedVMMain.ozf: build3/VMMain.ozf build3/lib/utility/Url.ozf \
	build3/lib/system/Signature.ozf build3/lib/system/Boot.ozf
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. $< -o $@

build3/compiler/LinkedCompilerMain.ozf: build3/compiler/CompilerMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler \
	build3/compiler/CompilerMain -o $@

build3/compiler/LinkedCompileServerMain.ozf: \
	build3/compiler/CompileServerMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler \
	build3/compiler/CompileServerMain -o $@

build3/compiler/LinkedToplevelMain.ozf: build3/compiler/ToplevelMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler \
	build3/compiler/ToplevelMain -o $@

build3/compiler/top/LinkedSMLToMozartRecursiveCompiler.ozf: \
	build3/compiler/top/SMLToMozartRecursiveCompiler.ozf \
	$(COMPILERBACKENDMOZARTSOURCES:%.oz=build3/%.ozf)
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../../ \
	--include=$(BUILD3)/compiler \
	build3/compiler/top/SMLToMozartRecursiveCompiler -o $@

build3/compiler/top/LinkedSMLToDotNetRecursiveCompiler.ozf: \
	build3/compiler/top/SMLToDotNetRecursiveCompiler.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../../ \
	--include=$(BUILD3)/compiler \
	build3/compiler/top/SMLToDotNetRecursiveCompiler -o $@

build3/linker/LinkedMain.ozf: build3/linker/Main.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--include=$(BUILD3)/linker build3/linker/Main -o $@

##
## Installing Build 3
##

build3-install: build3-all $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALL3DIRS):
	mkdir -p $@

build3/install/VMMain.ozf: build3/LinkedVMMain.ozf
	install -c -m 444 $< $@

build3/install/%.import: build3/%.import
	install -c -m 444 $< $@

build3/install/bin/%.bat: %.bat
	install -c -m 444 $< $@

build3/install/bin/%: %
	install -c -m 555 $< $@

build3/install/lib/fundamental/Fundamental.ozf: \
	build3/lib/fundamental/LinkedFundamental.ozf
	install -c -m 444 $< $@

build3/install/lib/%: build3/lib/%
	install -c -m 444 $< $@

build3/install/compiler/%: build3/compiler/Linked%
	install -c -m 444 $< $@

build3/install/compiler/top/%: build3/compiler/top/Linked%
	install -c -m 444 $< $@

build3/install/linker/%: build3/linker/Linked%
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
	$(ALICEDEP) $(BUILD2ALICESOURCES) $(BUILD3ALICESOURCES) \
	> Makefile.depend

-include Makefile.depend

##
## Global Installation
##

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib $(PREFIX)/lib/fundamental \
	$(PREFIX)/lib/system $(PREFIX)/lib/utility $(PREFIX)/lib/rtt \
	$(PREFIX)/lib/ml-yacc $(PREFIX)/compiler $(PREFIX)/compiler/top \
	$(PREFIX)/linker
INSTALLFILES = \
	$(INSTALL3TOPFILES:%=$(PREFIX)/%) \
	$(INSTALL3BINFILES:%=$(PREFIX)/bin/%) \
	$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%) \
	$(INSTALL3COMPILERFILES:%=$(PREFIX)/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=$(PREFIX)/compiler/top/%) \
	$(INSTALL3LINKERFILES:%=$(PREFIX)/linker/%)

install: build3-install $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

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

$(INSTALL3LINKERFILES:%=$(PREFIX)/linker/%): $(PREFIX)/%: \
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
