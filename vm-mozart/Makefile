###
### Author:
###   Leif Kornstaedt <kornstae@ps.uni-sb.de>
###
### Copyright:
###   Leif Kornstaedt, 1999-2001
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
## build1-all:
##    A complete source tree is created in subdirectory build1.
##    The native components are created using ozc.  The component
##    compiler/CompilerMain.ozf (and all its imports, transitively)
##    are built using the hybrid compiler via recursive invocation.
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
## build2-all:
##    A source tree for the libraries is created in subdirectory build2.
##    The native components are assigned signatures via --replacesign.
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
OZHOME = /opt/mozart-1.2.0
PREFIX = /opt/stockhausen-devel

# No configuration needed from here

VPATH = $(SRCTOP)

ALICEC0 = $(BOOTSTRAPDIR)/alicec
ALICEC1 = STOCKHOME=build1/install build1/install/bin/alicec
ALICEC2 = STOCKHOME=build2/install build2/install/bin/alicec

ALICEC_OPTS = --no-dump-elaboration-sig --no-implicit-import $(ALICEC_EXTRA_OPTS)
ALICEC_EXTRA_OPTS = --dump-target

ALICELINK0 = OZ_LOAD=pattern='?{x}'='?{x}'.ozf:cache=$(OZHOME)/cache/ ozl
ALICELINK1 = STOCKHOME=build1/install build1/install/bin/alicelink
ALICELINK2 = STOCKHOME=build2/install build2/install/bin/alicelink

ALICELINK_OPTS = -z9

ALICEDEP = sml @SMLload=$(BOOTSTRAPDIR)/alicedep

OZC = ozc
MLLEX = ml-lex
MLYACC = ml-yacc

##
## Enumerate All Files Making Up the Source Tree
##

NATIVECOMPONENTS = \
	IO TextIO OS_Process OS_FileSys Unix CommandLine \
	Tools UnsafePickle Debug Socket

LIBFUNDAMENTALSOURCES0 = \
	Pervasive.aml ToplevelTypes.aml ToplevelValues.aml \
	Fundamental.aml ALT-sig.aml ARRAY-sig.aml BOOL-sig.aml CELL-sig.aml \
	CHAR-sig.aml FUTURE-sig.aml GENERAL-sig.aml HOLE-sig.aml \
	IEEE_REAL-sig.aml INTEGER-sig.aml LIST-sig.aml LIST_PAIR-sig.aml \
	MATH-sig.aml OPTION-sig.aml PAIR-sig.aml PROMISE-sig.aml REAL-sig.aml \
	REF-sig.aml STRING-sig.aml STRING_CVT-sig.aml SUBSTRING-sig.aml \
	THREAD-sig.aml TIME-sig.aml VECTOR-sig.aml VECTOR_PAIR-sig.aml \
	WORD-sig.aml Alt.aml Array.aml Bool.aml Cell.aml Char.aml Future.aml \
	General.aml Hole.aml IEEEReal.aml Int.aml LargeInt.aml LargeReal.aml \
	LargeWord.aml List.aml ListPair.aml Math.aml Option.aml Pair.aml \
	Position.aml Promise.aml Real.aml Ref.aml String.aml StringCvt.aml \
	Substring.aml Thread.aml Time.aml Toplevel.aml Vector.aml \
	VectorPair.aml WideChar.aml WideString.aml Word.aml \
	PACKAGE-sig.aml Package.aml ByNeed.aml
LIBSYSTEMSOURCES0 = $(NATIVECOMPONENTS:%=%.asig) \
	COMMAND_LINE-sig.aml IO-sig.aml OS-sig.aml OS_FILE_SYS-sig.aml \
	OS_PROCESS-sig.aml SOCKET-sig.aml TEXT_IO-sig.aml UNIX-sig.aml \
	OS.aml PICKLE-sig.aml Pickle.aml HOME-sig.aml Home.aml
LIBUTILITYSOURCES0 = \
	ASSERT-sig.aml Assert.aml STAMP-sig.aml MkStamp.aml \
	GLOBAL_STAMP-sig.aml GlobalStamp.aml URL-sig.aml Url.aml \
	HASH_KEY-sig.aml LargeIntHashKey.aml StringHashKey.aml \
	IMP_SET-sig.aml MkHashImpSet.aml IMP_MAP-sig.aml MkHashImpMap.aml \
	SCOPED_IMP_SET-sig.aml MkScopedImpSet.aml SCOPED_IMP_MAP-sig.aml \
	MkScopedImpMap.aml LIST_SORT-sig.aml MkListSort.aml \
	DEPTH_FIRST_SEARCH-sig.aml MkDepthFirstSearch.aml \
	PRETTY_PRINT-sig.aml PrettyPrint.aml
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
	MkContextDumpingPhase.aml EmptyContext.aml SIGNATURE-sig.aml \
	COMPOSER-sig.aml TARGET-sig.aml MkHashScopedImpSet.aml \
	MkHashScopedImpMap.aml StringMap.aml PP_MISC-sig.aml PPMisc.aml
COMPILERINFRASTRUCTURESOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES0:%=compiler/infrastructure/%)

COMPILERCOMMONSOURCES0 = \
	Stamp.aml StampSet.aml StampMap.aml INTERMEDIATE_GRAMMAR-sig.aml \
	MkIntermediateGrammar.aml PP_INTERMEDIATE_GRAMMAR-sig.aml \
	MkPPIntermediateGrammar.aml IntermediateGrammar.aml \
	CHECK_INTERMEDIATE-sig.aml CheckIntermediate.aml
COMPILERCOMMONSOURCES = $(COMPILERCOMMONSOURCES0:%=compiler/common/%)

COMPILERFRONTENDCOMMONSOURCES0 = \
	LABEL_REFLECTION-sig.aml LabelReflection.aml \
	PATH_REFLECTION-sig.aml PathReflection.aml \
	TYPE_REFLECTION-sig.aml TypeReflection.aml \
	PERVASIVE_TYPE_REFLECTION-sig.aml PervasiveTypeReflection.aml \
	FIXITY_REFLECTION-sig.aml FixityReflection.aml \
	INF_REFLECTION-sig.aml InfReflection.aml \
	ABSTRACT_GRAMMAR-sig.aml MkAbstractGrammar.aml \
	PP_ABSTRACT_GRAMMAR-sig.aml MkPPAbstractGrammar.aml \
	AbstractGrammar.aml TypedGrammar.aml ENV-sig.aml Env.aml \
	ELABORATION_ERROR-sig.aml ElaborationError.aml \
	ELABORATION_PHASE-sig.aml MkElaborationPhase.aml \
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
COMPILERFRONTENDSMLSOURCES = $(COMPILERFRONTENDSMLSOURCES0:%=compiler/frontend-sml/%)

COMPILERBACKENDCOMMONSOURCES0 = \
	LABEL_SORT-sig.aml MkLabelSort.aml LabelSort.aml FLAT_GRAMMAR-sig.aml \
	FlatGrammar.aml OUTPUT_FLAT_GRAMMAR-sig.aml OutputFlatGrammar.aml \
	ARITY-sig.aml Arity.aml INTERMEDIATE_AUX-sig.aml IntermediateAux.aml \
	SIMPLIFY_MATCH-sig.aml SimplifyMatch.aml SIMPLIFY_REC-sig.aml \
	SimplifyRec.aml FLATTENING_PHASE-sig.aml MkFlatteningPhase.aml \
	PRIM_OPS-sig.aml PrimOps.aml VALUE_PROPAGATION_PHASE-sig.aml \
	ValuePropagationPhase.aml LIVENESS_ANALYSIS_PHASE-sig.aml \
	MkLivenessAnalysisPhase.aml DEAD_CODE_ELIMINATION_PHASE-sig.aml \
	DeadCodeEliminationPhase.aml MkBackendCommon.aml
COMPILERBACKENDCOMMONSOURCES = $(COMPILERBACKENDCOMMONSOURCES0:%=compiler/backend-common/%)

COMPILERBACKENDMOZARTSOURCES0 = \
	Builtins.oz Prebound.oz CodeEmitter.oz CodeStore.oz CodeGen.oz \
	Assembler.oz CodeGenPhase.oz CodeGenPhase.asig MkBackendMozart.aml
#--** CodeGenPhase.ozf should be built via replacesign
COMPILERBACKENDMOZARTSOURCES = $(COMPILERBACKENDMOZARTSOURCES0:%=compiler/backend-mozart/%)

COMPILERBACKENDDOTNETSOURCES0 = \
	SKELETON-sig.aml Skeleton.aml IL-sig.aml IL.aml System.aml \
	Alice.aml BUILTINS-sig.aml Builtins.aml CODE_STORE-sig.aml \
	CodeStore.aml CODE_GEN_PHASE-sig.aml CodeGenPhase.aml \
	MkBackendDotNet.aml
COMPILERBACKENDDOTNETSOURCES = $(COMPILERBACKENDDOTNETSOURCES0:%=compiler/backend-dotnet/%)

COMPILERTOPSOURCES0 = Signature.aml Composer.aml COMPILER-sig.aml MkCompiler.aml \
	RECURSIVE_COMPILER-sig.aml MkRecursiveCompiler.aml \
	SMLToMozartRecursiveCompiler.aml \
	SMLToDotNetRecursiveCompiler.aml \
	OPTION_PARSER-sig.aml MkOptionParser.aml \
	BATCH_COMPILER-sig.aml MkBatchCompiler.aml \
	SMLToMozartBatchCompiler.aml \
	SMLToDotNetBatchCompiler.aml \
	INTERACTIVE_TOPLEVEL-sig.aml MkToplevel.aml \
	SMLToMozartToplevel.aml
COMPILERTOPSOURCES = $(COMPILERTOPSOURCES0:%=compiler/top/%)

COMPILERSOURCES0 = CompilerMain.aml ToplevelMain.aml
COMPILERSOURCES = \
	$(COMPILERINFRASTRUCTURESOURCES) $(COMPILERCOMMONSOURCES) \
	$(COMPILERFRONTENDCOMMONSOURCES) $(COMPILERFRONTENDSMLSOURCES) \
	$(COMPILERBACKENDCOMMONSOURCES) $(COMPILERBACKENDMOZARTSOURCES) \
	$(COMPILERBACKENDDOTNETSOURCES) $(COMPILERTOPSOURCES) \
	$(COMPILERSOURCES0:%=compiler/%)
ALLSOURCES = VMMain.oz $(LIBSOURCES) $(COMPILERSOURCES)

SOURCEDIRS = \
	lib lib/fundamental lib/system lib/utility lib/rtt lib/ml-yacc \
	compiler compiler/infrastructure compiler/common compiler/top \
	compiler/frontend-common compiler/frontend-sml \
	compiler/backend-common compiler/backend-mozart \
	compiler/backend-dotnet

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
BUILD1SOURCES = build1/lib/fundamental/RTT.aml $(ALLSOURCES:%=build1/%)

INSTALL1DIRS = build1/install build1/install/bin build1/install/compiler
INSTALL1TOPFILES = VMMain.ozf
INSTALL1BINFILES = alicerun alicec alicelink
INSTALL1COMPILERFILES = CompilerMain.ozf
INSTALL1FILES = \
	$(INSTALL1TOPFILES:%=build1/install/%) \
	$(INSTALL1BINFILES:%=build1/install/bin/%) \
	$(INSTALL1COMPILERFILES:%=build1/install/compiler/%)

##
## Creating the Source Tree for Build 1
##

$(BUILD1DIRS):
	mkdir -p $@

Rel1.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../|g' < $< > $@

Rel2.import: lib/fundamental/Fundamental.import
	sed 's|x-alice:/|../../|g' < $< > $@

build1/lib/fundamental/RTT.aml: lib/fundamental/DummyRTT.aml
	cp $< $@

build1/lib/fundamental/%.aml: lib/fundamental/%.aml
	cp $< $@

build1/%.oz: vm-mozart/%.oz
	cp $< $@

build1/lib/system/%.asig: lib/system/%.asig Rel2.import
	cat Rel2.import $< > $@

build1/lib/%.aml: lib/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/infrastructure/%.aml: compiler/infrastructure/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/common/%.aml: compiler/common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/frontend-common/%.aml: compiler/frontend-common/%.aml Rel2.import
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
	 awk -f fixparser.awk < $<) > $@ || rm $@

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

build1/compiler/top/%.aml: compiler/top/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/compiler/%.aml: compiler/%.aml Rel1.import
	cat Rel1.import $< > $@

build1/%.oz: %.oz
	cp $< $@

##
## Performing Build 1
##

build1-all: $(BUILD1DIRS) $(BUILD1SOURCES) \
	build1/LinkedVMMain.ozf build1/compiler/LinkedCompilerMain.ozf

build1/%.ozf: build1/%.oz
	$(OZC) -c $< -o $@

build1/%.ozf: build1/%.aml
	$(ALICEC0) $(ALICEC_OPTS) --rtt-level=no -c $< -o $@

BUILD1OZSOURCES = $(filter %.oz, $(BUILD1SOURCES))
BUILD1OZLINPUTS = \
	$(BUILD1SOURCES) \
	$(NATIVECOMPONENTS:%=build1/lib/system/%.ozf) \
	$(BUILD1OZSOURCES:%.oz=%.ozf) \
	build1/compiler/CompilerMain.ozf

build1/LinkedVMMain.ozf: build1/VMMain.ozf $(BUILD1OZLINPUTS)
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. $< -o $@

build1/compiler/LinkedCompilerMain.ozf: $(BUILD1OZLINPUTS)
	$(ALICELINK0) $(ALICELINK_OPTS) --include=.. build1/compiler/CompilerMain.ozf -o $@

##
## Installing Build 1
##

build1-install: build1-all $(INSTALL1DIRS) $(INSTALL1FILES)

$(INSTALL1DIRS):
	mkdir -p $@

build1/install/VMMain.ozf: build1/LinkedVMMain.ozf
	install -c -m 444 $< $@

build1/install/compiler/CompilerMain.ozf: build1/compiler/LinkedCompilerMain.ozf
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
INSTALL2TOPFILES = VMMain.ozf Default.import
INSTALL2BINFILES = alicerun alicec alicelink
INSTALL2LIBFILES = rtt/RTT.ozf
INSTALL2COMPILERFILES = CompilerMain.ozf
INSTALL2FILES = \
	$(INSTALL2TOPFILES:%=build2/install/%) \
	$(INSTALL2BINFILES:%=build2/install/bin/%) \
	$(INSTALL2LIBFILES:%=build2/install/lib/%) \
	$(INSTALL2COMPILERFILES:%=build2/install/compiler/%)

##
## Creating the Source Tree for Build 2
##

$(BUILD2DIRS):
	mkdir -p $@

build2/lib/fundamental/RTT.aml: build1/lib/fundamental/RTT.aml
	cp $< $@

build2/%.aml: build1/%.aml
	cp $< $@

build2/LinkedVMMain.ozf build2/compiler/LinkedCompilerMain.ozf: build2/%: build1/%
	cp $< $@

build2/Default.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

$(NATIVECOMPONENTS:%=build2/lib/system/%.asig): build2/%: build1/%
	cp $< $@

##
## Performing Build 2
##

build2-all: build1-install $(BUILD2DIRS) $(BUILD2SOURCES) \
	build2/lib/rtt/LinkedRTT.ozf

build2/%.ozf: build2/%.aml
	$(ALICEC1) $(ALICEC_OPTS) --rtt-level=no -c $< -o $@

$(NATIVECOMPONENTS:%=build2/lib/system/%.ozf): build2/%.ozf: \
	build1/%.ozf build2/%.asig
	$(ALICEC1) $(ALICEC_OPTS) --rtt-level=no --replacesign $< build2/$*.asig $@

build2/lib/system/Unix.ozf: build2/lib/system/TextIO.ozf

build2/lib/rtt/LinkedRTT.ozf: build2/lib/rtt/RTT.ozf
	$(ALICELINK1) $(ALICELINK_OPTS) --include=.. $< -o $@

##
## Installing Build 2
##

build2-install: build2-all $(INSTALL2DIRS) $(INSTALL2FILES)

$(INSTALL2DIRS):
	mkdir -p $@

build2/install/VMMain.ozf: build1/install/VMMain.ozf
	install -c -m 444 $< $@

build2/install/Default.import: build2/Default.import
	install -c -m 444 $< $@

$(INSTALL2BINFILES:%=build2/install/bin/%): build2/%: build1/%
	install -c -m 555 $< $@

$(INSTALL2LIBFILES:%=build2/install/lib/%): build2/install/%: build2/%
	install -c -m 444 $< $@

build2/install/compiler/CompilerMain.ozf: build1/install/compiler/CompilerMain.ozf
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
	build3/install/compiler build3/install/compiler/top
INSTALL3TOPFILES = $(INSTALL2TOPFILES)
INSTALL3BINFILES = $(INSTALL2BINFILES) alice
INSTALL3LIBFILES = \
	$(filter %.ozf, $(LIBSOURCES0:%.aml=%.ozf)) \
	$(filter %.ozf, $(LIBSOURCES0:%.asig=%.ozf)) \
	fundamental/Fundamental.ozf
INSTALL3COMPILERFILES = $(INSTALL2COMPILERFILES) ToplevelMain.ozf
INSTALL3COMPILERTOPFILES = Composer.ozf \
	SMLToMozartRecursiveCompiler.ozf SMLToDotNetRecursiveCompiler.ozf
INSTALL3FILES = \
	$(INSTALL3TOPFILES:%=build3/install/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%.bat) \
	$(INSTALL3LIBFILES:%=build3/install/lib/%) \
	$(INSTALL3COMPILERFILES:%=build3/install/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=build3/install/compiler/top/%)

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
	build3/lib/fundamental/LinkedFundamental.ozf \
	build3/compiler/LinkedCompilerMain.ozf \
	build3/compiler/LinkedToplevelMain.ozf \
	build3/compiler/top/LinkedComposer.ozf \
	build3/compiler/top/LinkedSMLToMozartRecursiveCompiler.ozf \
	build3/compiler/top/LinkedSMLToDotNetRecursiveCompiler.ozf

build3/lib/fundamental/RTT.ozf: build2/lib/rtt/LinkedRTT.ozf
	cp $< $@

build3/%.ozf: build3/%.oz
	$(OZC) -c $< -o $@

build3/Default.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

$(NATIVECOMPONENTS:%=build3/lib/system/%.ozf): build3/%.ozf: \
	build1/%.ozf build3/%.asig build3/lib/fundamental/Fundamental.ozf
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=no --replacesign $< build3/$*.asig $@

build3/lib/system/Unix.ozf: build3/lib/system/TextIO.ozf

#--** --rtt-level=core is a problem for compiler/frontend-sml/MkParsingPhase.aml
build3/lib/fundamental/%.ozf: build3/lib/fundamental/%.aml
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/system/%.ozf: build3/lib/system/%.aml
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/utility/URL-sig.ozf: build3/lib/utility/URL-sig.aml
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/utility/Url.ozf: build3/lib/utility/Url.aml
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=full -c $< -o $@

build3/%.ozf: build3/%.aml
	$(ALICEC2) $(ALICEC_OPTS) --rtt-level=no -c $< -o $@

BUILD3 := $(shell pwd)/build3
TOP3 = $(BUILD3)/compiler/top

build3/lib/fundamental/LinkedFundamental.ozf: \
	build3/lib/fundamental/Fundamental.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) $< -o $@

build3/compiler/LinkedCompilerMain.ozf: build3/compiler/CompilerMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler $< -o $@

build3/compiler/LinkedToplevelMain.ozf: build3/compiler/ToplevelMain.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler $< -o $@

build3/compiler/top/LinkedComposer.ozf: build3/compiler/top/Composer.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../../ $< -o $@

build3/compiler/top/LinkedSMLToMozartRecursiveCompiler.ozf: \
	build3/compiler/top/SMLToMozartRecursiveCompiler.ozf \
	$(COMPILERBACKENDMOZARTSOURCES:%.oz=build3/%.ozf)
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../../ --include=.. \
	--exclude=$(BUILD3)/lib,$(BUILD3)/compiler/top/Composer $< -o $@

build3/compiler/top/LinkedSMLToDotNetRecursiveCompiler.ozf: \
	build3/compiler/top/SMLToDotNetRecursiveCompiler.ozf
	$(ALICELINK2) $(ALICELINK_OPTS) --rewrite=$(BUILD3)/=../../ --include=.. \
	--exclude=$(BUILD3)/lib,$(BUILD3)/compiler/top/Composer $< -o $@

##
## Installing Build 3
##

build3-install: build3-all $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALL3DIRS):
	mkdir -p $@

$(INSTALL3TOPFILES:%=build3/install/%): build3/install/%: build3/%
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

########################################################################
### THINGS OF A GLOBAL NATURE
########################################################################

##
## Automatic Dependency Management
##

BUILD2ALICESOURCES = $(filter %.aml %.asig, $(BUILD2SOURCES))
BUILD3ALICESOURCES = $(filter %.aml %.asig, $(BUILD3SOURCES))

depend: $(BUILD1DIRS) $(BUILD2DIRS) $(BUILD3DIRS) \
	$(BUILD2SOURCES) $(BUILD3SOURCES)
	$(ALICEDEP) $(BUILD2ALICESOURCES) $(BUILD3ALICESOURCES) > Makefile.depend

-include Makefile.depend

##
## Global Installation
##

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib $(PREFIX)/lib/fundamental \
	$(PREFIX)/lib/system $(PREFIX)/lib/utility $(PREFIX)/lib/rtt \
	$(PREFIX)/lib/ml-yacc $(PREFIX)/compiler $(PREFIX)/compiler/top
INSTALLFILES = \
	$(INSTALL3TOPFILES:%=$(PREFIX)/%) \
	$(INSTALL3BINFILES:%=$(PREFIX)/bin/%) \
	$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%) \
	$(INSTALL3COMPILERFILES:%=$(PREFIX)/compiler/%) \
	$(INSTALL3COMPILERTOPFILES:%=$(PREFIX)/compiler/top/%)

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

$(INSTALL3COMPILERTOPFILES:%=$(PREFIX)/compiler/top/%): $(PREFIX)/%: build3/install/%
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
