/*
 * Authors:
 *  Guido Tack <tack@ps.uni-sb.de>
 *
 * Copyright:
 *  Guido Tack, 2003
 *
 *  Last change:
 *    $Date$ by $Author$
 *    $Revision$
 * 
 */

#ifndef __GECODESPACE_HH__
#define __GECODESPACE_HH__

#include "gecode-int.hh"
#include "gecode-set.hh"

#define makeintvarargs(a,vars)                                 \
  IntVarArgs a(vars.size());                                   \
{ int s = vars.size(); for (int i=s; i--;) a[i] = is[vars[i]]; }

#define intvar2boolvar(intvar)        \
  static_cast<BoolVar>(intvar.core())

#define makeboolvarargs(a,vars)           \
  BoolVarArgs a(vars.size());             \
{ int s = vars.size(); for (int i=s; i--;) \
  a[i] = intvar2boolvar(is[vars[i]]); }

#define makefsvarargs(a,vars)                                 \
  SetVarArgs a(vars.size());                              \
{ int s = vars.size(); for (int i=s; i--;) a[i] = fss[vars[i]]; }

typedef int intvar;
typedef int boolvar;
typedef int setvar;
typedef IntArgs intvarargs;
typedef IntArgs boolvarargs;
typedef IntArgs setvarargs;

class GecodeSpace : private Space {
protected:
  IntVarArray is;
  int noOfIntVars;
  int intArraySize;
  
  SetVarArray fss;
  int noOfSetVars;
  int fsArraySize;

  void EnlargeIntVarArray(void);
  void EnlargeSetVarArray(void);

public:
  using Space::clone;
  using Space::status;
  using Space::commit;
  using Space::description;
  using Space::operator new;
  using Space::operator delete;

  GecodeSpace() : is(3, 0,0), noOfIntVars(0),
		  intArraySize(3),
		  fss(3), noOfSetVars(0), fsArraySize(3)
  {}

  explicit
  GecodeSpace(GecodeSpace& s) : Space(s), 
				is(s.is.copy()),
                                noOfIntVars(s.noOfIntVars),
                                intArraySize(s.intArraySize),
 				fss(s.fss.copy()),
 				noOfSetVars(s.noOfSetVars),
 				fsArraySize(s.fsArraySize)
  {}

  virtual Space* copy(void) {
    return new GecodeSpace(*this);
  }

  intvar new_intvar(DomSpec&);
  boolvar new_boolvar(void);

  // commit with description (for batch recomputation)
  void commitDescription(int alt, BranchDesc*);

  // Inspect variable information
  int int_getMin(intvar);
  int int_getMax(intvar);
  VarRanges<IntVar> int_getRanges(intvar);

  // Domain
  void int_dom(intvar, DomSpec& ds);
  void int_domR(intvar, DomSpec& ds, boolvar);

  // Propagators
  void int_rel(intvar, reltype, intvar);
  void int_relI(intvar, reltype, int);
  void int_relR(intvar, reltype, intvar, boolvar);
  void int_relIR(intvar, reltype, int, boolvar);

  void int_eq(intvar, intvar, conlevel);
  void int_eqV(const intvarargs&, conlevel);
  void int_eqR(intvar, intvar, boolvar, conlevel);

  // Distinct constraints
  void int_distinct(const intvarargs&, conlevel);
  void int_distinctI(const IntArgs& offsets, const intvarargs&, conlevel);

  // Linear equations
  void int_linear(const IntArgs& coefficients, const intvarargs& vars,
                  reltype rel, int constant, conlevel cl);
  void int_linearR(const IntArgs& coefficients, const intvarargs& vars,
               reltype, int, boolvar, conlevel);


  // Counting constraints
  void int_countII(const intvarargs&, reltype,
	       int, reltype, int);
  void int_countIV(const intvarargs&, reltype,
	       int, reltype, int);
  void int_countVI(const intvarargs& vars, reltype,
	       int, reltype, int);
  void int_countVV(const intvarargs& vars, reltype,
	       int, reltype, int);

  // Access constraints

  void int_element(const intvarargs&, intvar, intvar);
  void int_elementI(const IntArgs&, intvar, intvar);
  void int_lex(const intvarargs&, reltype, const intvarargs& vars2);

  // Boolean constraints
  void int_bool_not(boolvar, boolvar);
  void int_bool_and(boolvar, boolvar, boolvar);
  void int_bool_or(boolvar, boolvar, boolvar);
  void int_bool_imp(boolvar, boolvar, boolvar);
  void int_bool_eq(boolvar, boolvar, boolvar);
  void int_bool_xor(boolvar, boolvar, boolvar);
  
  void int_bool_andV(const boolvarargs&, boolvar);
  void int_bool_orV(const boolvarargs&, boolvar);

  // Arithmetic constraints

  void int_min(const intvarargs&, intvar);
  void int_max(const intvarargs& vars, intvar);
  void int_abs(intvar, intvar, conlevel);
  void int_mult(intvar, intvar, intvar);
  void int_power(intvar, intvar, intvar);

  // Value assignment

  void int_assign(const intvarargs&, AvalSel);

  // Branching
  void int_branch(const intvarargs& vars, BvarSel, BvalSel);

  // Faling
  void fail(void);

  // Finite Set Variables / Constraints

  setvar new_setvar(void);
  
  UBIter<SetVar> set_getUpperBound(setvar);
  LBIter<SetVar> set_getLowerBound(setvar);
  RangesMinus<UBIter<SetVar>, LBIter<SetVar> > set_getUnknown(setvar);
  int set_getCardinalityMin(setvar);
  int set_getCardinalityMax(setvar);
  bool set_getAssigned(setvar);

  template<class I>
  void set_lowerBound(setvar s, I& i) {
    if (!enter()) return;
    if (Space::failed())
      return;
    
    GECODE_CME_SPACE(fss[s].includeI(i));
  }

  template<class I>
  void set_upperBound(setvar s, I& i) {
    if (!enter()) return;
    if (Space::failed())
      return;
    
    GECODE_CME_SPACE(fss[s].intersectI(i));
  }


  unsigned int set_getUpperBoundSize(setvar);
  unsigned int set_getLowerBoundSize(setvar);

  void set_include(intvar, setvar);
  void set_exclude(intvar, setvar);
  void set_the(intvar, setvar);
  void set_min(intvar, setvar);
  void set_max(intvar, setvar);
  void set_match(setvar, const intvarargs&);
  void set_card(setvar, intvar);
  void set_cardRange(setvar, int min, int max);

  void set_superOfInter(setvar, setvar, setvar);
  void set_subOfUnion(setvar, setvar, setvar);
  
  void set_subset(setvar, setvar);
  void set_nosubset(setvar, setvar);
  void set_disjoint(setvar, setvar);
  void set_distinct(setvar, setvar);
  void set_distinctn(const setvarargs&);
  void set_equals(setvar, setvar);
  void set_convex(setvar);
  void set_convexHull(setvar, setvar);
  void set_union(setvar, setvar, setvar);
  void set_complement(setvar, setvar);
  void set_intersection(setvar, setvar, setvar);
  void set_difference(setvar, setvar, setvar);
  void set_partition(setvar, setvar, setvar);
  void set_unionn(const setvarargs&, setvar);
  void set_intersectionn(const setvarargs&, setvar);
  void set_partitionn(const setvarargs&, setvar);

  void set_includeR(intvar, setvar, boolvar);
  void set_includeRI(int, setvar, boolvar);
  void set_equalR(setvar, setvar, boolvar);
  void set_subsetR(setvar, setvar, boolvar);

  void set_selectUnion(setvar, const setvarargs&, setvar);
  void set_selectInter(setvar, const setvarargs&, setvar);
  void set_selectSets(setvar, const setvarargs&, intvar);

  void set_branch(const setvarargs&, SetBvarSel, SetBvalSel);
  void set_randomBranch(const setvarargs&, int);
  
  void set_print(setvar);

};

#endif
