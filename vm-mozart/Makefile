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
##    Identical to veryclean.
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
##    stoc/top/StocMain.ozf (and all its imports, transitively)
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

PREFIX = /opt/stockhausen-devel

STOC0 = STOC_MOZART=../../stoc/backend-mozart/stoc-mozart.exe \
	sml @SMLload=../../stoc/stoc-mozart
STOC1 = build1/install/bin/stoc
STOC2 = build2/install/bin/stoc

STOC_OPTS = $(STOC_EXTRA_OPTS) --no-dump-elaboration-sig --no-implicit-import
STOC_EXTRA_OPTS = --dump-target
STODEP = sml @SMLload=../../stoc/stodep

OZHOME = /opt/mozart-1.1.1
OZC = ozc
OZL = OZ_LOAD=pattern='?{x}'='?{x}'.ozf:cache=$(OZHOME)/cache/ ozl -z9
MLLEX = ml-lex
MLYACC = ml-yacc

##
## Enumerate All Files Making Up the Source Tree
##

NATIVECOMPONENTS = IO TextIO OS Unix CommandLine Tools Pickle Debug Socket
LIBSOURCES0 = Base.sml $(NATIVECOMPONENTS:%=%.ozf.sig)
MLYACCSOURCES0 = base.sig join.sml lrtable.sml stream.sml parser2.sml
LIBSOURCES = $(LIBSOURCES0:%=lib/%) $(MLYACCSOURCES0:%=lib/ml-yacc/%)

STOCMISCSOURCES0 = \
	Assert.sml HASH_KEY.sig StringHashKey.sml LargeIntHashKey.sml \
	IMP_SET.sig MakeHashImpSet.sml IMP_MAP.sig MakeHashImpMap.sml \
	SCOPED_IMP_SET.sig MakeScopedImpSet.sml MakeHashScopedImpSet.sml \
	SCOPED_IMP_MAP.sig MakeScopedImpMap.sml MakeHashScopedImpMap.sml \
	STAMP.sig MakeStamp.sml GLOBAL_STAMP.sig GlobalStamp.sml \
	PRETTY_PRINT.sig PrettyPrint.sml PP_MISC.sig PPMisc.sml \
	URL.sig Url.sml SORT.sig MakeSort.sml DEPTH_FIRST_SEARCH.sig \
	MakeDepthFirstSearch.sml
STOCMISCSOURCES = $(STOCMISCSOURCES0:%=stoc/misc/%)

STOCINFRASTRUCTURESOURCES0 = \
	CRASH.sig Crash.sml SOURCE.sig Source.sml ERROR.sig Error.sml \
	SWITCHES.sig MakeSwitches.sml CONTEXT.sig REPRESENTATION.sig \
	PHASE.sig PHASE_ERROR.sig ComposePhases.sml MakeTracingPhase.sml \
	MakeResultDumpingPhase.sml MakeContextDumpingPhase.sml \
	EmptyContext.sml SIGNATURE.sig COMPOSER.sig TARGET.sig
STOCINFRASTRUCTURESOURCES = \
	$(STOCINFRASTRUCTURESOURCES0:%=stoc/infrastructure/%)

STOCCOMMONSOURCES0 = \
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
	TYPE_REFLECTION.sig TypeReflection.sml \
	PERVASIVE_TYPE_REFLECTION.sig PervasiveTypeReflection.sml \
	FIXITY_REFLECTION.sig FixityReflection.sml \
	INF_REFLECTION.sig InfReflection.sml
STOCCOMMONSOURCES = $(STOCCOMMONSOURCES0:%=stoc/common/%)

STOCFRONTENDCOMMONSOURCES0 = \
	StringMap.sml ABSTRACT_GRAMMAR.sig MakeAbstractGrammar.sml \
	PP_ABSTRACT_GRAMMAR.sig MakePPAbstractGrammar.sml \
	AbstractGrammar.sml TypedGrammar.sml ENV.sig Env.sml \
	ELABORATION_ERROR.sig ElaborationError.sml ELABORATION_PHASE.sig \
	MakeElaborationPhase.sml TRANSLATION_PHASE.sig \
	MakeTranslationPhase.sml MakeFrontendCommon.sml
STOCFRONTENDCOMMONSOURCES = \
	$(STOCFRONTENDCOMMONSOURCES0:%=stoc/frontend-common/%)

STOCFRONTENDSMLSOURCES0 = \
	LAB.sig Lab.sml ID.sig MakeId.sml Ids.sml SCON.sig SCon.sml \
	INPUT_GRAMMAR.sig MakeInputGrammar.sml InputGrammar.sml \
	PARSING_ERROR.sig ParsingError.sml ABSTRACTION_ERROR.sig \
	AbstractionError.sml INFIX.sig Infix.sml BIND_ENV.sig BindEnv.sml \
	BIND_ENV_FROM_SIG.sig BindEnvFromSig.sml SHARING.sig Sharing.sml \
	ABSTRACTION_PHASE.sig MakeAbstractionPhase.sml DERIVED_FORMS.sig \
	PARSER.sig Parser.sml DerivedForms.sml LEXER_ERROR.sig \
	LexerError.sml Lexer.sml CountPosLexer.sml PARSING_PHASE.sig \
	ParsingPhase.sml MakeFrontendSML.sml
STOCFRONTENDSMLSOURCES = $(STOCFRONTENDSMLSOURCES0:%=stoc/frontend-sml/%)

STOCBACKENDCOMMONSOURCES0 = \
	LABEL_SORT.sig MakeLabelSort.sml LabelSort.sml FLAT_GRAMMAR.sig \
	FlatGrammar.sml OUTPUT_FLAT_GRAMMAR.sig OutputFlatGrammar.sml \
	ARITY.sig Arity.sml INTERMEDIATE_AUX.sig IntermediateAux.sml \
	SIMPLIFY_MATCH.sig SimplifyMatch.sml SIMPLIFY_REC.sig SimplifyRec.sml \
	FLATTENING_PHASE.sig FlatteningPhase.sml PRIM_OPS.sig \
	PrimOps.sml VALUE_PROPAGATION_PHASE.sig ValuePropagationPhase.sml \
	LIVENESS_ANALYSIS_PHASE.sig MakeLivenessAnalysisPhase.sml \
	DEAD_CODE_ELIMINATION_PHASE.sig DeadCodeEliminationPhase.sml \
	MakeBackendCommon.sml
STOCBACKENDCOMMONSOURCES = $(STOCBACKENDCOMMONSOURCES0:%=stoc/backend-common/%)

STOCBACKENDMOZARTSOURCES0 = \
	Builtins.oz Prebound.oz CodeEmitter.oz CodeStore.oz CodeGen.oz \
	Assembler.oz CodeGenPhase.oz CodeGenPhase.ozf.sig MakeBackendMozart.sml
STOCBACKENDMOZARTSOURCES = $(STOCBACKENDMOZARTSOURCES0:%=stoc/backend-mozart/%)

STOCTOPSOURCES0 = Signature.sml Composer.sml COMPILER.sig MakeCompiler.sml \
	RECURSIVE_COMPILER.sig MakeRecursiveCompiler.sml \
	SMLToMozartRecursiveCompiler.sml \
	BATCH_COMPILER.sig MakeBatchCompiler.sml \
	SMLToMozartBatchCompiler.sml StocMain.sml \
	INTERACTIVE_TOPLEVEL.sig MakeEmacsToplevel.sml \
	SMLToMozartEmacsToplevel.sml StotMain.sml
STOCTOPSOURCES = $(STOCTOPSOURCES0:%=stoc/top/%)

STOCSOURCES = \
	$(STOCMISCSOURCES) $(STOCINFRASTRUCTURESOURCES) $(STOCCOMMONSOURCES) \
	$(STOCFRONTENDCOMMONSOURCES) $(STOCFRONTENDSMLSOURCES) \
	$(STOCBACKENDCOMMONSOURCES) $(STOCBACKENDMOZARTSOURCES) \
	$(STOCTOPSOURCES)
ALLSOURCES = Init.oz $(LIBSOURCES) $(STOCSOURCES)

SOURCEDIRS = \
	lib lib/ml-yacc stoc stoc/misc stoc/infrastructure stoc/common \
	stoc/frontend-common stoc/frontend-sml stoc/backend-common \
	stoc/backend-mozart stoc/top

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
BUILD1SOURCES = $(ALLSOURCES:%=build1/%)

INSTALL1DIRS = build1/install build1/install/bin
INSTALL1TOPFILES = stow.ozf stoc.ozf
INSTALL1BINFILES = stow stoc
INSTALL1FILES = \
	$(INSTALL1TOPFILES:%=build1/install/%) \
	$(INSTALL1BINFILES:%=build1/install/bin/%)

##
## Creating the Source Tree for Build 1
##

$(BUILD1DIRS):
	mkdir -p $@

Rel1Base.import: Base.import
	sed 's|x-alice:/|../|g' < $< > $@

Rel2Base.import: Base.import
	sed 's|x-alice:/|../../|g' < $< > $@

build1/lib/Base.sml: ../../lib/bootstrap/Bootstrap.aus
	cp $< $@

build1/lib/%: ../%
	cp $< $@

build1/lib/%.ozf.sig: ../../lib/bootstrap/%.sig Rel1Base.import
	(cat Rel1Base.import && sed 's|x-alice:/||g' < $<) > $@

build1/lib/ml-yacc/%: ../../lib/bootstrap/ml-yacc/% lib/ml-yacc/%.header \
	Rel2Base.import
	cat Rel2Base.import lib/ml-yacc/$*.header $< > $@

build1/stoc/misc/%: misc/%.body Rel2Base.import
	cat Rel2Base.import $< > $@

build1/stoc/misc/%: ../../stoc/misc/% misc/%.header Rel2Base.import
	cat Rel2Base.import misc/$*.header $< > $@

build1/stoc/infrastructure/%: ../../stoc/infrastructure/% \
	infrastructure/%.header Rel2Base.import
	cat Rel2Base.import infrastructure/$*.header $< > $@

build1/stoc/common/%: ../../stoc/common/% common/%.header Rel2Base.import
	cat Rel2Base.import common/$*.header $< > $@

build1/stoc/frontend-common/%: ../../stoc/frontend-common/% \
	frontend-common/%.header Rel2Base.import
	cat Rel2Base.import frontend-common/$*.header $< > $@

build1/stoc/frontend-sml/%: ../../stoc/frontend-sml/% frontend-sml/%.header \
	Rel2Base.import
	cat Rel2Base.import frontend-sml/$*.header $< > $@

build1/stoc/frontend-sml/Lexer.lex: ../../stoc/frontend-sml/Lexer.lex
	cp $< $@

build1/stoc/frontend-sml/Lexer.sml: build1/stoc/frontend-sml/Lexer.lex.sml \
	frontend-sml/Lexer.sml.header Rel2Base.import
	cat Rel2Base.import frontend-sml/Lexer.sml.header $< > $@

build1/stoc/frontend-sml/Parser.grm: ../../stoc/frontend-sml/Parser.grm
	cp $< $@

build1/stoc/frontend-sml/PARSER.sig: build1/stoc/frontend-sml/Parser.grm.sig \
	frontend-sml/PARSER.sig.header Rel2Base.import fixparser.awk
	(cat Rel2Base.import frontend-sml/PARSER.sig.header && \
	 awk -f fixparser.awk < $<) > $@ || rm $@

build1/stoc/frontend-sml/Parser.sml: build1/stoc/frontend-sml/Parser.grm.sml \
	frontend-sml/Parser.sml.header Rel2Base.import
	cat Rel2Base.import frontend-sml/Parser.sml.header $< > $@

build1/stoc/backend-common/%: ../../stoc/backend-common/% \
	backend-common/%.header Rel2Base.import
	cat Rel2Base.import backend-common/$*.header $< > $@

build1/stoc/backend-mozart/%: backend-mozart/%.body Rel2Base.import
	cat Rel2Base.import $< > $@

build1/stoc/backend-mozart/%.ozf.sig: ../../stoc/backend-mozart/%.ozf.sig \
	Rel2Base.import
	cat Rel2Base.import $< > $@

build1/stoc/top/%: top/%.body Rel2Base.import
	cat Rel2Base.import $< > $@

build1/stoc/top/%: ../../stoc/top/% top/%.header Rel2Base.import
	cat Rel2Base.import top/$*.header $< > $@

build1/%.oz: ../../%.oz
	cp $< $@

build1/%: %
	cp $< $@

##
## Performing Build 1
##

build1-all: $(BUILD1DIRS) build1/stow.ozf build1/stoc.ozf

build1/%.ozf: build1/%.oz
	$(OZC) -c $< -o $@

build1/%.ozf: build1/%.sig
	$(STOC0) $(STOC_OPTS) -c $< -o $@

build1/%.ozf: build1/%.sml
	$(STOC0) $(STOC_OPTS) -c $< -o $@

BUILD1OZSOURCES = $(filter %.oz, $(BUILD1SOURCES))
BUILD1OZLINPUTS = \
	$(BUILD1SOURCES) \
	$(NATIVECOMPONENTS:%=build1/lib/%.ozf) \
	$(filter-out build1/stoc/backend-mozart/CodeEmitter.ozf, \
	  $(BUILD1OZSOURCES:%.oz=%.ozf)) \
	build1/stoc/top/StocMain.ozf

build1/stow.ozf: build1/Init.ozf $(BUILD1OZLINPUTS)
	$(OZL) --include=.. $< -o $@

build1/stoc.ozf: $(BUILD1OZLINPUTS)
	$(OZL) --include=.. build1/stoc/top/StocMain.ozf -o $@

build1/stoc/backend-mozart/CodeStore.oz: \
	build1/stoc/backend-mozart/CodeEmitter.oz

##
## Installing Build 1
##

build1-install: build1-all $(INSTALL1DIRS) $(INSTALL1FILES)

$(INSTALL1DIRS):
	mkdir -p $@

build1/install/%.ozf: build1/%.ozf
	install -c -m 444 $< $@

build1/install/bin/%: %
	install -c -m 555 $< $@

########################################################################
### BUILD 2
########################################################################

BUILD2DIRS = build2 build2/lib

INSTALL2DIRS = build2/install build2/install/bin build2/install/lib
INSTALL2TOPFILES = stow.ozf stoc.ozf Default.import
INSTALL2BINFILES = stow stoc
INSTALL2LIBFILES = Base.ozf $(NATIVECOMPONENTS:%=%.ozf)
INSTALL2FILES = \
	$(INSTALL2TOPFILES:%=build2/install/%) \
	$(INSTALL2BINFILES:%=build2/install/bin/%) \
	$(INSTALL2LIBFILES:%=build2/install/lib/%)

##
## Creating the Source Tree for Build 2
##

$(BUILD2DIRS):
	mkdir -p $@

build2/lib/Base.sml: build1/lib/Base.sml
	cp $< $@

build2/Default.import: Base.import Other.import
	cat Base.import Other.import > $@

$(NATIVECOMPONENTS:%=build2/lib/%.ozf.sig): build2/%: build1/%
	cp $< $@

##
## Performing Build 2
##

build2-all: build1-all $(BUILD2DIRS)

build2/%.ozf: build2/%.sig
	$(STOC1) $(STOC_OPTS) -c $< -o $@

build2/%.ozf: build2/%.sml
	$(STOC1) $(STOC_OPTS) -c $< -o $@

$(NATIVECOMPONENTS:%=build2/lib/%.ozf): build2/%.ozf: \
	build1/%.ozf build2/%.ozf.sig
	$(STOC1) $(STOC_OPTS) --replacesign $< $@.sig $@

##
## Installing Build 2
##

build2-install: build2-all $(INSTALL2DIRS) $(INSTALL2FILES)

$(INSTALL2DIRS):
	mkdir -p $@

build2/install/bin/stoc: build1/install/bin/stoc
	install -c -m 555 $< $@

build2/install/bin/stow: build1/install/bin/stow
	install -c -m 555 $< $@

build2/install/stoc.ozf: build1/install/stoc.ozf
	install -c -m 444 $< $@

build2/install/stow.ozf: build1/install/stow.ozf
	install -c -m 444 $< $@

build2/install/Default.import: build2/Default.import
	install -c -m 444 $< $@

$(INSTALL2LIBFILES:%=build2/install/lib/%): build2/install/%: build2/%
	install -c -m 444 $< $@

########################################################################
### BUILD 3
########################################################################

BUILD3DIRS = build3 $(SOURCEDIRS:%=build3/%)
BUILD3SOURCES = $(ALLSOURCES:%=build3/%)

INSTALL3DIRS = build3/install build3/install/bin build3/install/lib
INSTALL3TOPFILES = stow.ozf stoc.ozf stot.ozf Default.import
INSTALL3BINFILES = stow stoc stot
INSTALL3LIBFILES = Base.ozf $(NATIVECOMPONENTS:%=%.ozf)
INSTALL3FILES = \
	$(INSTALL3TOPFILES:%=build3/install/%) \
	$(INSTALL3BINFILES:%=build3/install/bin/%) \
	$(INSTALL3LIBFILES:%=build3/install/lib/%)

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

BUILD3OZSOURCES = $(filter %.oz, $(BUILD3SOURCES))
BUILD3SIGSOURCES = $(filter %.sig, $(filter-out %.ozf.sig, $(BUILD3SOURCES)))
BUILD3SMLSOURCES = $(filter %.sml, $(BUILD3SOURCES))

BUILD3OZLINPUTS = \
	$(NATIVECOMPONENTS:%=build3/lib/%.ozf) \
	$(filter-out build3/stoc/backend-mozart/CodeEmitter.ozf, \
	  $(BUILD3OZSOURCES:%.oz=%.ozf)) \
	$(BUILD3SIGSOURCES:%.sig=%.ozf) \
	$(BUILD3SMLSOURCES:%.sml=%.ozf)

build3-all: build2-all $(BUILD3DIRS) \
	build3/stow.ozf build3/stoc.ozf build3/stot.ozf

build3/%.ozf: build3/%.sig
	$(STOC2) $(STOC_OPTS) -c $< -o $@

build3/%.ozf: build3/%.sml
	$(STOC2) $(STOC_OPTS) -c $< -o $@

$(NATIVECOMPONENTS:%=build3/lib/%.ozf): build3/%.ozf: \
	build1/%.ozf build3/%.ozf.sig build3/lib/Base.ozf
	$(STOC2) $(STOC_OPTS) --replacesign $< $@.sig $@

build3/stow.ozf: $(BUILD3OZLINPUTS)
	$(OZL) --include=.. build3/Init.ozf -o $@

build3/stoc.ozf: $(BUILD3OZLINPUTS)
	$(OZL) --include=.. build3/stoc/top/StocMain.ozf -o $@

build3/stot.ozf: $(BUILD3OZLINPUTS)
	$(OZL) --include=.. build3/stoc/top/StotMain.ozf -o $@

build3/Default.import: Base.import Other.import
	cat Base.import Other.import > $@

##
## Installing Build 3
##

build3-install: build3-all $(INSTALL3DIRS) $(INSTALL3FILES)

$(INSTALL3DIRS):
	mkdir -p $@

$(INSTALL3TOPFILES:%=build3/install/%): build3/install/%: build3/%
	install -c -m 444 $< $@

$(INSTALL3BINFILES:%=build3/install/bin/%): build3/install/bin/%: %
	install -c -m 555 $< $@

$(INSTALL3LIBFILES:%=build3/install/lib/%): build3/install/%: build3/%
	install -c -m 444 $< $@

########################################################################
### THINGS OF A GLOBAL NATURE
########################################################################

##
## Automatic Dependency Management
##

BUILD3ALICESOURCES = $(BUILD3SIGSOURCES) $(BUILD3SMLSOURCES)

depend: $(BUILD1DIRS) $(BUILD3DIRS) $(BUILD3SOURCES) $(BUILD3ALICESOURCES)
	$(STODEP) $(BUILD3ALICESOURCES) > Makefile.depend

-include Makefile.depend

##
## Global Installation
##

INSTALLDIRS = $(PREFIX) $(PREFIX)/bin $(PREFIX)/lib
INSTALLFILES = \
	$(INSTALL3TOPFILES:%=$(PREFIX)/%) \
	$(INSTALL3BINFILES:%=$(PREFIX)/bin/%) \
	$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%)

install: build3-install $(INSTALLDIRS) $(INSTALLFILES)

$(INSTALLDIRS):
	mkdir -p $@

$(INSTALL3TOPFILES:%=$(PREFIX)/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

$(INSTALL3BINFILES:%=$(PREFIX)/bin/%): $(PREFIX)/%: build3/install/%
	install -c -m 555 $< $@

$(INSTALL3LIBFILES:%=$(PREFIX)/lib/%): $(PREFIX)/%: build3/install/%
	install -c -m 444 $< $@

##
## Cleaning up
##

clean:
	-rm -f Rel1Base.import Rel2Base.import
	-rm -rf build1 build2 build3

veryclean: clean

distclean: veryclean
