STOC = ../../stoc/backend-mozart/stoc-mozart --image=../../stoc/top/stoc-mozart
MERGE = cat

WORD_FILES = Base.import
MLYACC_FILES = \
	Base.import \
	Word.import \
	TextIO.import \
	/opt/smlnj/src/ml-yacc/lib/base.sig \
	/opt/smlnj/src/ml-yacc/lib/join.sml \
	/opt/smlnj/src/ml-yacc/lib/lrtable.sml \
	/opt/smlnj/src/ml-yacc/lib/stream.sml \
	/opt/smlnj/src/ml-yacc/lib/parser2.sml

%.ozf: %.aus
	$(STOC) --in=$< --out=$@

all: Base.ozf Word.ozf MLYacc.ozf

Base.ozf: ../../lib/bootstrap/Bootstrap.aus
	$(STOC) --in=$< --out=$@

Word.aus: $(WORD_FILES)
	$(MERGE) $(WORD_FILES) > $@

MLYacc.aus: $(MLYACC_FILES)
	$(MERGE) $(MLYACC_FILES) > $@
