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
## The goal is to build a stoc using the hybrid SML/NJ+Mozart compiler.
## This is performed via the following targets.
##
## build1-all:
##    A complete source tree is created in subdirectory build1.
##    The native components are created using ozc.  The component
##    stoc/StocMain.ozf (and all its imports, transitively)
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
## (including stot), with runtime type information and a first-class
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
OZHOME = /opt/mozart-1.1.1
PREFIX = /opt/stockhausen-devel

# No configuration needed from here

VPATH = $(SRCTOP)

STOC0 = $(BOOTSTRAPDIR)/stoc
STOC1 = STOCKHOME=build1/install build1/install/bin/stoc
STOC2 = STOCKHOME=build2/install build2/install/bin/stoc

STOC_OPTS = --no-dump-elaboration-sig --no-implicit-import $(STOC_EXTRA_OPTS)
STOC_EXTRA_OPTS = --dump-target

STOL0 = OZ_LOAD=pattern='?{x}'='?{x}'.ozf:cache=$(OZHOME)/cache/ ozl
STOL1 = STOCKHOME=build1/install build1/install/bin/stol
STOL2 = STOCKHOME=build2/install build2/install/bin/stol

STOL_OPTS = -z9

STODEP = sml @SMLload=$(BOOTSTRAPDIR)/stodep

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
	OS.aml PICKLE-sig.aml Pickle.aml
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

STOCINFRASTRUCTURESOURCES0 = \
	CRASH-sig.aml Crash.aml SOURCE-sig.aml Source.aml ERROR-sig.aml \
	Error.aml SWITCHES-sig.aml MkSwitches.aml CONTEXT-sig.aml \
	REPRESENTATION-sig.aml PHASE-sig.aml PHASE_ERROR-sig.aml \
	ComposePhases.aml MkTracingPhase.aml MkResultDumpingPhase.aml \
	MkContextDumpingPhase.aml EmptyContext.aml SIGNATURE-sig.aml \
	COMPOSER-sig.aml TARGET-sig.aml MkHashScopedImpSet.aml \
	MkHashScopedImpMap.aml StringMap.aml PP_MISC-sig.aml PPMisc.aml
STOCINFRASTRUCTURESOURCES = \
	$(STOCINFRASTRUCTURESOURCES0:%=stoc/infrastructure/%)

STOCCOMMONSOURCES0 = \
	Stamp.aml StampSet.aml StampMap.aml INTERMEDIATE_GRAMMAR-sig.aml \
	MkIntermediateGrammar.aml PP_INTERMEDIATE_GRAMMAR-sig.aml \
	MkPPIntermediateGrammar.aml IntermediateGrammar.aml \
	CHECK_INTERMEDIATE-sig.aml CheckIntermediate.aml
STOCCOMMONSOURCES = $(STOCCOMMONSOURCES0:%=stoc/common/%)

STOCFRONTENDCOMMONSOURCES0 = \
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
STOCFRONTENDCOMMONSOURCES = \
	$(STOCFRONTENDCOMMONSOURCES0:%=stoc/frontend-common/%)

STOCFRONTENDSMLSOURCES0 = \
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
STOCFRONTENDSMLSOURCES = $(STOCFRONTENDSMLSOURCES0:%=stoc/frontend-sml/%)

STOCBACKENDCOMMONSOURCES0 = \
	LABEL_SORT-sig.aml MkLabelSort.aml LabelSort.aml FLAT_GRAMMAR-sig.aml \
	FlatGrammar.aml OUTPUT_FLAT_GRAMMAR-sig.aml OutputFlatGrammar.aml \
	ARITY-sig.aml Arity.aml INTERMEDIATE_AUX-sig.aml IntermediateAux.aml \
	SIMPLIFY_MATCH-sig.aml SimplifyMatch.aml SIMPLIFY_REC-sig.aml \
	SimplifyRec.aml FLATTENING_PHASE-sig.aml MkFlatteningPhase.aml \
	PRIM_OPS-sig.aml PrimOps.aml VALUE_PROPAGATION_PHASE-sig.aml \
	ValuePropagationPhase.aml LIVENESS_ANALYSIS_PHASE-sig.aml \
	MkLivenessAnalysisPhase.aml DEAD_CODE_ELIMINATION_PHASE-sig.aml \
	DeadCodeEliminationPhase.aml MkBackendCommon.aml
STOCBACKENDCOMMONSOURCES = $(STOCBACKENDCOMMONSOURCES0:%=stoc/backend-common/%)

STOCBACKENDMOZARTSOURCES0 = \
	Builtins.oz Prebound.oz CodeEmitter.oz CodeStore.oz CodeGen.oz \
	Assembler.oz CodeGenPhase.oz CodeGenPhase.asig MkBackendMozart.aml
#--** CodeGenPhase.ozf should be built via replacesign
STOCBACKENDMOZARTSOURCES = $(STOCBACKENDMOZARTSOURCES0:%=stoc/backend-mozart/%)

STOCBACKENDDOTNETSOURCES0 = \
	SKELETON-sig.aml Skeleton.aml IL-sig.aml IL.aml System.aml \
	Alice.aml BUILTINS-sig.aml Builtins.aml CODE_STORE-sig.aml \
	CodeStore.aml CODE_GEN_PHASE-sig.aml CodeGenPhase.aml \
	MkBackendDotNet.aml
STOCBACKENDDOTNETSOURCES = $(STOCBACKENDDOTNETSOURCES0:%=stoc/backend-dotnet/%)

STOCTOPSOURCES0 = Signature.aml Composer.aml COMPILER-sig.aml MkCompiler.aml \
	RECURSIVE_COMPILER-sig.aml MkRecursiveCompiler.aml \
	SMLToMozartRecursiveCompiler.aml \
	SMLToDotNetRecursiveCompiler.aml \
	OPTION_PARSER-sig.aml MkOptionParser.aml \
	BATCH_COMPILER-sig.aml MkBatchCompiler.aml \
	SMLToMozartBatchCompiler.aml \
	SMLToDotNetBatchCompiler.aml \
	INTERACTIVE_TOPLEVEL-sig.aml MkToplevel.aml \
	SMLToMozartToplevel.aml
STOCTOPSOURCES = $(STOCTOPSOURCES0:%=stoc/top/%)

STOCSOURCES0 = StocMain.aml StotMain.aml
STOCSOURCES = \
	$(STOCINFRASTRUCTURESOURCES) $(STOCCOMMONSOURCES) \
	$(STOCFRONTENDCOMMONSOURCES) $(STOCFRONTENDSMLSOURCES) \
	$(STOCBACKENDCOMMONSOURCES) $(STOCBACKENDMOZARTSOURCES) \
	$(STOCBACKENDDOTNETSOURCES) $(STOCTOPSOURCES) \
	$(STOCSOURCES0:%=stoc/%)
ALLSOURCES = StowMain.oz $(LIBSOURCES) $(STOCSOURCES)

SOURCEDIRS = \
	lib lib/fundamental lib/system lib/utility lib/rtt lib/ml-yacc \
	stoc stoc/infrastructure stoc/common stoc/frontend-common \
	stoc/frontend-sml stoc/backend-common stoc/backend-mozart \
	stoc/backend-dotnet stoc/top

##
## Main Target and Generally Applicable Rules
##

.PHONY: all depend install clean veryclean distclean
.PHONY: build1-all build1-install
.PHONY: build2-all build2-install
.PHONY: build3-all build3-install

all: build3-install

# Build anything using the compiler generated in pass 1
stoc1-%: build1-install
	$(MAKE) STOC0=$(STOC1) $*

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

INSTALL1DIRS = build1/install build1/install/bin build1/install/stoc
INSTALL1TOPFILES = StowMain.ozf
INSTALL1BINFILES = stow stoc stol
INSTALL1STOCFILES = StocMain.ozf
INSTALL1FILES = \
	$(INSTALL1TOPFILES:%=build1/install/%) \
	$(INSTALL1BINFILES:%=build1/install/bin/%) \
	$(INSTALL1STOCFILES:%=build1/install/stoc/%)

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

build1/stoc/infrastructure/%.aml: stoc/infrastructure/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/common/%.aml: stoc/common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/frontend-common/%.aml: stoc/frontend-common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/frontend-sml/%.aml: stoc/frontend-sml/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/frontend-sml/Lexer.lex: stoc/frontend-sml/Lexer.lex
	cp $< $@

build1/stoc/frontend-sml/Lexer.aml: build1/stoc/frontend-sml/Lexer.lex.sml \
	stoc/frontend-sml/Lexer.aml.header Rel2.import
	cat Rel2.import $(SRCTOP)/stoc/frontend-sml/Lexer.aml.header $< > $@

build1/stoc/frontend-sml/Parser.grm: stoc/frontend-sml/Parser.grm
	cp $< $@

build1/stoc/frontend-sml/PARSER-sig.aml: \
	build1/stoc/frontend-sml/Parser.grm.sig \
	Rel2.import stoc/frontend-sml/PARSER-sig.aml.header fixparser.awk
	(cat Rel2.import $(SRCTOP)/stoc/frontend-sml/PARSER-sig.aml.header && \
	 awk -f fixparser.awk < $<) > $@ || rm $@

build1/stoc/frontend-sml/Parser.aml: build1/stoc/frontend-sml/Parser.grm.sml \
	Rel2.import stoc/frontend-sml/Parser.aml.header
	cat Rel2.import $(SRCTOP)/stoc/frontend-sml/Parser.aml.header $< > $@

build1/stoc/backend-common/%.aml: stoc/backend-common/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/backend-mozart/%.aml: stoc/backend-mozart/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/backend-mozart/%.asig: stoc/backend-mozart/%.asig \
	Rel2.import
	cat Rel2.import $< > $@

build1/stoc/backend-dotnet/%.aml: stoc/backend-dotnet/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/top/%.aml: stoc/top/%.aml Rel2.import
	cat Rel2.import $< > $@

build1/stoc/%.aml: stoc/%.aml Rel1.import
	cat Rel1.import $< > $@

build1/%.oz: %.oz
	cp $< $@

##
## Performing Build 1
##

build1-all: $(BUILD1DIRS) $(BUILD1SOURCES) \
	build1/LinkedStowMain.ozf build1/stoc/LinkedStocMain.ozf

build1/%.ozf: build1/%.oz
	$(OZC) -c $< -o $@

build1/%.ozf: build1/%.aml
	$(STOC0) $(STOC_OPTS) -c $< -o $@

BUILD1OZSOURCES = $(filter %.oz, $(BUILD1SOURCES))
BUILD1OZLINPUTS = \
	$(BUILD1SOURCES) \
	$(NATIVECOMPONENTS:%=build1/lib/system/%.ozf) \
	$(BUILD1OZSOURCES:%.oz=%.ozf) \
	build1/stoc/StocMain.ozf

build1/LinkedStowMain.ozf: build1/StowMain.ozf $(BUILD1OZLINPUTS)
	$(STOL0) $(STOL_OPTS) --include=.. $< -o $@

build1/stoc/LinkedStocMain.ozf: $(BUILD1OZLINPUTS)
	$(STOL0) $(STOL_OPTS) --include=.. build1/stoc/StocMain.ozf -o $@

##
## Installing Build 1
##

build1-install: build1-all $(INSTALL1DIRS) $(INSTALL1FILES)

$(INSTALL1DIRS):
	mkdir -p $@

build1/install/StowMain.ozf: build1/LinkedStowMain.ozf
	install -c -m 444 $< $@

build1/install/stoc/StocMain.ozf: build1/stoc/LinkedStocMain.ozf
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
	build2/install/lib/rtt build2/install/lib/ml-yacc build2/install/stoc
INSTALL2TOPFILES = StowMain.ozf Default.import
INSTALL2BINFILES = stow stoc stol
INSTALL2LIBFILES = rtt/RTT.ozf
INSTALL2STOCFILES = StocMain.ozf
INSTALL2FILES = \
	$(INSTALL2TOPFILES:%=build2/install/%) \
	$(INSTALL2BINFILES:%=build2/install/bin/%) \
	$(INSTALL2LIBFILES:%=build2/install/lib/%) \
	$(INSTALL2STOCFILES:%=build2/install/stoc/%)

##
## Creating the Source Tree for Build 2
##

$(BUILD2DIRS):
	mkdir -p $@

build2/lib/fundamental/RTT.aml: build1/lib/fundamental/RTT.aml
	cp $< $@

build2/%.aml: build1/%.aml
	cp $< $@

build2/LinkedStowMain.ozf build2/stoc/LinkedStocMain.ozf: build2/%: build1/%
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
	$(STOC1) $(STOC_OPTS) -c $< -o $@

$(NATIVECOMPONENTS:%=build2/lib/system/%.ozf): build2/%.ozf: \
	build1/%.ozf build2/%.asig
	$(STOC1) $(STOC_OPTS) --replacesign $< build2/$*.asig $@

build2/lib/system/Unix.ozf: build2/lib/system/TextIO.ozf

build2/lib/rtt/LinkedRTT.ozf: build2/lib/rtt/RTT.ozf
	$(STOL1) $(STOL_OPTS) --include=.. $< -o $@

##
## Installing Build 2
##

build2-install: build2-all $(INSTALL2DIRS) $(INSTALL2FILES)

$(INSTALL2DIRS):
	mkdir -p $@

build2/install/StowMain.ozf: build1/install/StowMain.ozf
	install -c -m 444 $< $@

build2/install/Default.import: build2/Default.import
	install -c -m 444 $< $@

$(INSTALL2BINFILES:%=build2/install/bin/%): build2/%: build1/%
	install -c -m 555 $< $@

$(INSTALL2LIBFILES:%=build2/install/lib/%): build2/install/%: build2/%
	install -c -m 444 $< $@

build2/install/stoc/StocMain.ozf: build1/install/stoc/StocMain.ozf
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
	build3/install/stoc build3/install/stoc/top
INSTALL3TOPFILES = $(INSTALL2TOPFILES)
INSTALL3BINFILES = $(INSTALL2BINFILES) stot
INSTALL3LIBFILES = \
	$(filter %.ozf, $(LIBSOURCES0:%.aml=%.ozf)) \
	$(filter %.ozf, $(LIBSOURCES0:%.asig=%.ozf)) \
	fundamental/Fundamental.ozf
INSTALL3STOCFILES = $(INSTALL2STOCFILES) StotMain.ozf
INSTALL3STOCTOPFILES = Composer.ozf \
	SMLToMozartRecursiveCompiler.ozf SMLToDotNetRecursiveCompiler.ozf
INSTALL3FILES = \
	$(INSTALL3TOPFILES:%=build3/install/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%.bat) \
	$(INSTALL3LIBFILES:%=build3/install/lib/%) \
	$(INSTALL3STOCFILES:%=build3/install/stoc/%) \
	$(INSTALL3STOCTOPFILES:%=build3/install/stoc/top/%)

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
	build3/stoc/LinkedStocMain.ozf \
	build3/stoc/LinkedStotMain.ozf \
	build3/stoc/top/LinkedComposer.ozf \
	build3/stoc/top/LinkedSMLToMozartRecursiveCompiler.ozf \
	build3/stoc/top/LinkedSMLToDotNetRecursiveCompiler.ozf

build3/lib/fundamental/RTT.ozf: build2/lib/rtt/LinkedRTT.ozf
	cp $< $@

build3/%.ozf: build3/%.oz
	$(OZC) -c $< -o $@

build3/Default.import: lib/fundamental/Fundamental.import lib/Other.import
	cat $^ > $@

$(NATIVECOMPONENTS:%=build3/lib/system/%.ozf): build3/%.ozf: \
	build1/%.ozf build3/%.asig build3/lib/fundamental/Fundamental.ozf
	$(STOC2) $(STOC_OPTS) --replacesign $< build3/$*.asig $@

build3/lib/system/Unix.ozf: build3/lib/system/TextIO.ozf

#--** --rtt-level=core is a problem for stoc/frontend-sml/MkParsingPhase.aml
build3/lib/fundamental/%.ozf: build3/lib/fundamental/%.aml
	$(STOC2) $(STOC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/system/%.ozf: build3/lib/system/%.aml
	$(STOC2) $(STOC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/utility/URL-sig.ozf: build3/lib/utility/URL-sig.aml
	$(STOC2) $(STOC_OPTS) --rtt-level=full -c $< -o $@

build3/lib/utility/Url.ozf: build3/lib/utility/Url.aml
	$(STOC2) $(STOC_OPTS) --rtt-level=full -c $< -o $@

build3/%.ozf: build3/%.aml
	$(STOC2) $(STOC_OPTS) -c $< -o $@

BUILD3 := $(shell pwd)/build3
TOP3 = $(BUILD3)/stoc/top

build3/lib/fundamental/LinkedFundamental.ozf: \
	build3/lib/fundamental/Fundamental.ozf
	$(STOL2) $(STOL_OPTS) $< -o $@

build3/stoc/LinkedStocMain.ozf: build3/stoc/StocMain.ozf
	$(STOL2) $(STOL_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler $< -o $@

build3/stoc/LinkedStotMain.ozf: build3/stoc/StotMain.ozf
	$(STOL2) $(STOL_OPTS) --rewrite=$(BUILD3)/=../ \
	--exclude=$(TOP3)/SMLToMozartRecursiveCompiler \
	--exclude=$(TOP3)/SMLToDotNetRecursiveCompiler $< -o $@

build3/stoc/top/LinkedComposer.ozf: build3/stoc/top/Composer.ozf
	$(STOL2) $(STOL_OPTS) --rewrite=$(BUILD3)/=../../ $< -o $@

build3/stoc/top/LinkedSMLToMozartRecursiveCompiler.ozf: \
	build3/stoc/top/SMLToMozartRecursiveCompiler.ozf \
	$(STOCBACKENDMOZARTSOURCES:%.oz=build3/%.ozf)
	$(STOL2) $(STOL_OPTS) --rewrite=$(BUILD3)/=../../ --include=.. \
	--exclude=$(BUILD3)/lib,$(BUILD3)/stoc/top/Composer $< -o $@

build3/stoc/top/LinkedSMLToDotNetRecursiveCompiler.ozf: \
	build3/stoc/top/SMLToDotNetRecursiveCompiler.ozf
	$(STOL2) $(STOL_OPTS) --rewrite=$(BUILD3)/=../../ --include=.. \
	--exclude=$(BUILD3)/lib,$(BUILD3)/stoc/top/Composer $< -o $@

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

build3/install/stoc/%: build3/stoc/Linked%
	install -c -m 444 $< $@

build3/install/stoc/top/%: build3/stoc/top/Linked%
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
	$(STODEP) $(BUILD2ALICESOURCES) $(BUILD3ALICESOURCES) > Makefile.depend

-include Makefile.depend

##
## Global Installation
##

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib $(PREFIX)/lib/fundamental \
	$(PREFIX)/lib/system $(PREFIX)/lib/utility $(PREFIX)/lib/rtt \
	$(PREFIX)/lib/ml-yacc $(PREFIX)/stoc $(PREFIX)/stoc/top
INSTALLFILES = \
	$(INSTALL3TOPFILES:%=$(PREFIX)/%) \
	$(INSTALL3BINFILES:%=$(PREFIX)/bin/%) \
	$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%) \
	$(INSTALL3STOCFILES:%=$(PREFIX)/stoc/%) \
	$(INSTALL3STOCTOPFILES:%=$(PREFIX)/stoc/top/%)

install: build3-install $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(INSTALL3TOPFILES:%=$(PREFIX)/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3BINFILES:%=$(PREFIX)/bin/%): $(PREFIX)/%: build3/install/%
	install -c -m 555 $< $@

$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3STOCFILES:%=$(PREFIX)/stoc/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3STOCTOPFILES:%=$(PREFIX)/stoc/top/%): $(PREFIX)/%: build3/install/%
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
