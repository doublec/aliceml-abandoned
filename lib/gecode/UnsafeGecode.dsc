// Spaces

class Space {
  IntVar new_intvar(DomSpec&); new_intvar;
  BoolVar new_boolvar(void); new_boolvar;
  SetVar new_setvar(void); new_setvar;
  void commit(int, BranchingDesc*); commit;
};

class IntVar {
  int min(void); int_getMin;
  int max(void); int_getMax;
  int med(void); int_getMed;
  VarRanges<IntVar> ranges(void); int_getRanges;
  int size(void); int_getDomainSize;
  bool range(void); int_isRange;
  bool assigned(void); int_isAssigned;
};

class SetVar {
  UBIter<SetVar> set_getUpperBound(void); set_getUpperBound;
  LBIter<SetVar> set_getLowerBound(void); set_getLowerBound;
  unsigned int upperBoundSize(void); set_getUpperBoundSize;
  unsigned int lowerBoundSize(void); set_getLowerBoundSize;
  unsigned int unknownSize(void); set_getUnknownSize;
  UnknownIter<SetVar> set_getUnknown(void); set_getUnknown;
  int cardMin(void); set_getCardinalityMin;
  int cardMax(void); set_getCardinalityMax;
  bool assigned(void); set_isAssigned;
};

// Domain
void dom(IntVar, DomSpec&); int_dom;
void dom(IntVar, DomSpec&, BoolVar); int_domR;
// Propagators
void rel(IntVar, reltype, IntVar); int_rel;
void rel(IntVar, reltype, int); int_relI;
void rel(IntVar, reltype, IntVar, BoolVar); int_relR;
void rel(IntVar, reltype, int, BoolVar); int_relIR;

void eq(IntVar, IntVar, conlevel); int_eq;
void eq(const IntVarArgs&, conlevel); int_eqV;
void eq(IntVar, IntVar, BoolVar, conlevel); int_eqR;

// Distinct constraints
void distinct(const IntVarArgs&, conlevel); int_distinct;
void distinct(const IntArgs&, const IntVarArgs&, conlevel); int_distinctI;

// Linear equations
void linear(const IntArgs&, const IntVarArgs&, reltype, int, conlevel); int_linear;
void linear(const IntArgs&, const IntVarArgs&, reltype, int, BoolVar, conlevel); int_linearR;


// Counting constraints
void count(const IntVarArgs&, reltype, int, reltype, int); int_countII;
void count(const IntVarArgs&, reltype, int, reltype, IntVar); int_countIV;
void count(const IntVarArgs&, reltype, IntVar, reltype, int); int_countVI;
void count(const IntVarArgs&, reltype, IntVar, reltype, IntVar); int_countVV;

// Access constraints

void element(const IntVarArgs&, IntVar, IntVar); int_element;
void element(const IntArgs&, IntVar, IntVar); int_elementI;
void lex(const IntVarArgs&, reltype, const IntVarArgs&); int_lex;

// Boolean constraints
void bool_not(BoolVar, BoolVar); int_bool_not;
void bool_and(BoolVar, BoolVar, BoolVar); int_bool_and;
void bool_or(BoolVar, BoolVar, BoolVar); int_bool_or;
void bool_imp(BoolVar, BoolVar, BoolVar); int_bool_imp;
void bool_eqv(BoolVar, BoolVar, BoolVar); int_bool_eq;
void bool_xor(BoolVar, BoolVar, BoolVar); int_bool_xor;

void bool_and(const BoolVarArgs&, BoolVar); int_bool_andV;
void bool_or(const BoolVarArgs&, BoolVar); int_bool_orV;

// Arithmetic constraints

void min(const IntVarArgs&, IntVar); int_min;
void max(const IntVarArgs&, IntVar); int_max;
void abs(IntVar, IntVar, conlevel); int_abs;
void mult(IntVar, IntVar, IntVar); int_mult;
//void power(IntVar, IntVar, IntVar); int_power;

// Value assignment

void assign(const IntVarArgs&, AvalSel); int_assign;

// Branching
void branch(const IntVarArgs&, BvarSel, BvalSel); int_branch;

// Finite Set Variables / Constraints

//UBIter<SetVar> getUpperBound(setvar); set_getUpperBound;
//LBIter<SetVar> getLowerBound(setvar); set_getLowerBound;
//Diff<UBIter<SetVar>, LBIter<SetVar> > getUnknown(setvar); set_getUnknown;
//int set_getCardinalityMin(setvar);
//int set_getCardinalityMax(setvar);
//bool set_getAssigned(setvar);

//unsigned int set_getUpperBoundSize(setvar);
//unsigned int set_getLowerBoundSize(setvar);

void include(SetVar, DomSpec&); set_lowerBound;
void intersect(SetVar, DomSpec&); set_upperBound;

void include(SetVar, IntVar); set_include;
void exclude(SetVar, IntVar); set_exclude;
void the(SetVar, IntVar); set_the;
void minElement(SetVar, IntVar); set_min;
void maxElement(SetVar, IntVar); set_max;
void match(SetVar, const IntVarArgs&); set_match;
void card(SetVar, IntVar); set_card;
void cardRange(SetVar, int, int); set_cardRange;

void superOfInter(SetVar, SetVar, SetVar); set_superOfInter;
void subOfUnion(SetVar, SetVar, SetVar); set_subOfUnion;

void subset(SetVar, SetVar); set_subset;
void noSubset(SetVar, SetVar); set_nosubset;
void disjoint(SetVar, SetVar); set_disjoint;
void distinct(SetVar, SetVar); set_distinct;
void distinct(const SetVarArgs&); set_distinctn;
void equals(SetVar, SetVar); set_equals;
void convex(SetVar); set_convex;
void convexHull(SetVar, SetVar); set_convexHull;
void fsunion(SetVar, SetVar, SetVar); set_union;
void complement(SetVar, SetVar); set_complement;
void intersection(SetVar, SetVar, SetVar); set_intersection;
void difference(SetVar, SetVar, SetVar); set_difference;
void partition(SetVar, SetVar, SetVar); set_partition;
void fsunion(const SetVarArgs&, SetVar); set_unionn;
void intersection(const SetVarArgs&, SetVar); set_intersectionn;
void partition(const SetVarArgs&, SetVar); set_partitionn;
void seq(const SetVarArgs&); set_seq;
void seqU(const SetVarArgs&, SetVar); set_seqU;

void include(SetVar, IntVar, BoolVar); set_includeR;
void include(SetVar, int, BoolVar); set_includeRI;
void equal(SetVar, SetVar, BoolVar); set_equalR;
void subset(SetVar, SetVar, BoolVar); set_subsetR;

void selectUnion(SetVar, const SetVarArgs&, SetVar); set_selectUnion;
void selectInter(SetVar, const SetVarArgs&, SetVar); set_selectInter;
void selectDisjoint(const SetVarArgs&, SetVar); set_selectDisjoint;
void selectSets(SetVar, const SetVarArgs&, IntVar); set_selectSets;

void branch(const SetVarArgs&, SetBvarSel, SetBvalSel); set_branch;
//void randomBranch(const SetVarArgs&, int); set_randomBranch;
