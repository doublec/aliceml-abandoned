// Spaces
commit : int - void

new_intvar : DomSpec& - intvar
new_boolvar : void - boolvar
new_setvar : void - setvar
commitDescription : int, BranchDesc* - void

// Inspect variable information

int_getMin : intvar - int
int_getMax : intvar - int
int_getRanges : intvar - VarRanges<IntVar>
// Domain
int_dom : intvar, DomSpec& - void
int_domR : intvar, DomSpec&, boolvar - void
// Propagators
int_rel : intvar, reltype, intvar - void
int_relI : intvar, reltype, int - void
int_relR : intvar, reltype, intvar, boolvar - void
int_relIR : intvar, reltype, int, boolvar - void

int_eq : intvar, intvar, conlevel - void
int_eqV : const intvarargs&, conlevel - void
int_eqR : intvar, intvar, boolvar, conlevel - void

// Distinct constraints
int_distinct : const intvarargs&, conlevel - void
int_distinctI : const IntArgs&, const intvarargs&, conlevel - void

// Linear equations
int_linear : const IntArgs&, const intvarargs&, reltype, int, conlevel - void
int_linearR : const IntArgs&, const intvarargs&, reltype, int, boolvar, conlevel - void


// Counting constraints
int_countII : const intvarargs&, reltype, int, reltype, int - void 
int_countIV : const intvarargs&, reltype, int, reltype, intvar - void
int_countVI : const intvarargs&, reltype, intvar, reltype, int - void
int_countVV : const intvarargs&, reltype, intvar, reltype, intvar - void

// Access constraints

int_element : const intvarargs&, intvar, intvar - void
int_elementI : const IntArgs&, intvar, intvar - void
int_lex : const intvarargs&, reltype, const intvarargs& - void

// Boolean constraints
int_bool_not : boolvar, boolvar - void
int_bool_and : boolvar, boolvar, boolvar - void
int_bool_or : boolvar, boolvar, boolvar - void
int_bool_imp : boolvar, boolvar, boolvar - void
int_bool_eq : boolvar, boolvar, boolvar - void
int_bool_xor : boolvar, boolvar, boolvar - void

int_bool_andV : const boolvarargs&, boolvar - void
int_bool_orV : const boolvarargs&, boolvar - void

// Arithmetic constraints

int_min : const intvarargs&, intvar - void
int_max : const intvarargs&, intvar - void
int_abs : intvar, intvar, conlevel - void
int_mult : intvar, intvar, intvar - void
int_power : intvar, intvar, intvar - void

// Value assignment

int_assign : const intvarargs&, AvalSel - void

// Branching
int_branch : const intvarargs&, BvarSel, BvalSel - void

// Faling
fail : void - void

// Finite Set Variables / Constraints

set_getUpperBound : setvar - UBIter<SetVar>
set_getLowerBound : setvar - LBIter<SetVar>
set_getUnknown : setvar - Iter::Ranges::Diff<UBIter<SetVar>, LBIter<SetVar> >
set_getCardinalityMin : setvar - int
set_getCardinalityMax : setvar - int
set_getAssigned : setvar - bool

set_getUpperBoundSize : setvar - unsigned int
set_getLowerBoundSize : setvar - unsigned int

set_lowerBound : setvar, I& - void
set_upperBound : setvar, I& - void

set_include : intvar, setvar - void
set_exclude : intvar, setvar - void
set_the : intvar, setvar - void
set_min : intvar, setvar - void
set_max : intvar, setvar - void
set_match : setvar, const intvarargs& - void
set_card : setvar, intvar - void
set_cardRange : setvar, int, int - void

set_superOfInter : setvar, setvar, setvar - void
set_subOfUnion : setvar, setvar, setvar - void

set_subset : setvar, setvar - void
set_nosubset : setvar, setvar - void
set_disjoint : setvar, setvar - void
set_distinct : setvar, setvar - void
set_distinctn : const setvarargs& - void
set_equals : setvar, setvar - void
set_convex : setvar - void
set_convexHull : setvar, setvar - void
set_union : setvar, setvar, setvar - void
set_complement : setvar, setvar - void
set_intersection : setvar, setvar, setvar - void
set_difference : setvar, setvar, setvar - void
set_partition : setvar, setvar, setvar - void
set_unionn : const setvarargs&, setvar - void
set_intersectionn : const setvarargs&, setvar - void
set_partitionn : const setvarargs&, setvar - void

set_includeR : intvar, setvar, boolvar - void
set_includeRI : int, setvar, boolvar - void
set_equalR : setvar, setvar, boolvar - void
set_subsetR : setvar, setvar, boolvar - void

set_selectUnion : setvar, const setvarargs&, setvar - void
set_selectInter : setvar, const setvarargs&, setvar - void
set_selectSets : setvar, const setvarargs&, intvar - void

set_branch : const setvarargs&, SetBvarSel, SetBvalSel - void
set_randomBranch : const setvarargs&, int - void

set_print : setvar - void
