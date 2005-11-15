// Spaces

class Space {
  IntVar new_intvar(IntSet&); new_intvar;
  BoolVar new_boolvar(void); new_boolvar;
  SetVar new_setvar(void); new_setvar;
  void commit(int, BranchingDesc*); commit;
};

class IntVar {
  int min(void); int_getMin;
  int max(void); int_getMax;
  int med(void); int_getMed;
  int size(void); int_getDomainSize;
  bool range(void); int_isRange;
  bool assigned(void); int_isAssigned;
};

class SetVar {
  SetVarLubRanges set_getUpperBound(void); set_getUpperBound;
  SetVarGlbRanges set_getLowerBound(void); set_getLowerBound;
  unsigned int lubSize(void); set_getUpperBoundSize;
  unsigned int glbSize(void); set_getLowerBoundSize;
  unsigned int unknownSize(void); set_getUnknownSize;
  SetVarUnknownRanges set_getUnknown(void); set_getUnknown;
  unsigned int cardMin(void); set_getCardinalityMin;
  unsigned int cardMax(void); set_getCardinalityMax;
  bool assigned(void); set_isAssigned;
};

// Domain
void dom(IntVar, IntSet&); int_dom;
void dom(IntVar, IntSet&, BoolVar); int_domR;
// Propagators
void rel(IntVar, IntRelType, IntVar); int_rel;
void rel(IntVar, IntRelType, int); int_relI;
void rel(IntVar, IntRelType, IntVar, BoolVar); int_relR;
void rel(IntVar, IntRelType, int, BoolVar); int_relIR;

void eq(IntVar, IntVar, IntConLevel); int_eq;
void eq(const IntVarArgs&, IntConLevel); int_eqV;
void eq(IntVar, IntVar, BoolVar, IntConLevel); int_eqR;

// Distinct constraints
void distinct(const IntVarArgs&, IntConLevel); int_distinct;
void distinct(const IntArgs&, const IntVarArgs&, IntConLevel); int_distinctI;

// Sortedness
void sortedness(const IntVarArgs&, const IntVarArgs&, IntConLevel); int_sortedness;
void sortedness(const IntVarArgs&, const IntVarArgs&, const IntVarArgs&, IntConLevel); int_permsort;

// Global cardinality
void gcc(const IntVarArgs&, const IntArgs&, const IntArgs&, IntConLevel); int_gcc;

// Linear equations
void linear(const IntArgs&, const IntVarArgs&, IntRelType, int, IntConLevel); int_linear;
void linear(const IntArgs&, const IntVarArgs&, IntRelType, int, BoolVar, IntConLevel); int_linearR;


// Counting constraints
void count(const IntVarArgs&, int, IntRelType, int); int_countII;
void count(const IntVarArgs&, int, IntRelType, IntVar); int_countIV;
void count(const IntVarArgs&, IntVar, IntRelType, int); int_countVI;
void count(const IntVarArgs&, IntVar, IntRelType, IntVar); int_countVV;

// Access constraints

void element(const IntVarArgs&, IntVar, IntVar); int_element;
void element(const IntArgs&, IntVar, IntVar); int_elementI;
void lex(const IntVarArgs&, IntRelType, const IntVarArgs&); int_lex;

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
void abs(IntVar, IntVar, IntConLevel); int_abs;
void mult(IntVar, IntVar, IntVar, IntConLevel); int_mult;

// Value assignment

void assign(const IntVarArgs&, AvalSel); int_assign;

// Branching
void branch(const IntVarArgs&, BvarSel, BvalSel); int_branch;

// Finite Set Propagators

void dom(SetVar, SetRelType, IntSet&); set_dom;
void dom(SetVar, SetRelType, IntSet&, BoolVar); set_domR;
void cardinality(SetVar, unsigned int, unsigned int); set_cardRange;

void rel(SetVar, SetRelType, SetVar); set_rel;
void rel(SetVar, SetRelType, SetVar, BoolVar); set_relR;
void rel(SetVar, SetOpType, SetVar, SetRelType, SetVar); set_relOp;
void rel(SetVar, SetRelType, IntVar); set_relI;
void rel(SetVar, SetRelType, IntVar, BoolVar); set_relIR;
void rel(SetVar, IntRelType, IntVar); set_relII;
void rel(SetOpType, const SetVarArgs&, SetVar); set_relN;
void rel(SetOpType, const IntVarArgs&, SetVar); set_relNI;
void rel(IntSet&, SetOpType, SetVar, SetRelType, SetVar); set_relCSS;
void rel(SetVar, SetOpType, IntSet&, SetRelType, SetVar); set_relSCS;
void rel(SetVar, SetOpType, SetVar, SetRelType, IntSet&); set_relSSC;
void rel(IntSet&, SetOpType, IntSet&, SetRelType, SetVar); set_relCCS;
void rel(IntSet&, SetOpType, SetVar, SetRelType, IntSet&); set_relCSC;
void rel(SetVar, SetOpType, IntSet&, SetRelType, IntSet&); set_relSCC;

void convex(SetVar); set_convex;
void convexHull(SetVar, SetVar); set_convexHull;
void sequence(const SetVarArgs&); set_seq;
void sequentialUnion(const SetVarArgs&, SetVar); set_seqU;

void minElement(SetVar, IntVar); set_min;
void maxElement(SetVar, IntVar); set_max;
void match(SetVar, const IntVarArgs&); set_match;
void cardinality(SetVar, IntVar); set_cardinality;

void selectUnion(const SetVarArgs&, SetVar, SetVar); set_selectUnion;
void selectInter(const SetVarArgs&, SetVar, SetVar); set_selectInter;
void selectInterIn(const SetVarArgs&, SetVar, SetVar, IntSet&); set_selectInterIn;
void selectDisjoint(const SetVarArgs&, SetVar); set_selectDisjoint;
void selectSets(const SetVarArgs&, IntVar, SetVar); set_selectSets;

void branch(const SetVarArgs&, SetBvarSel, SetBvalSel); set_branch;
