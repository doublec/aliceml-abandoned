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

class GecodeSpace : private Space {
protected:
  IntVarArray is;
  int noOfIntVars;
  int intArraySize;
  
  SetVarArray fss;
  int noOfSetVars;
  int fsArraySize;

  void EnlargeIntVarArray();
  void EnlargeSetVarArray();

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

  int AddIntVariable(DomSpec& ds);
  int AddIntVariableR(DomSpec& ds, int boolVar);  
  int AddBoolVariable();

  // commit with description (for batch recomputation)
  void commitDescription(int alt, BranchDesc *desc);

  // Inspect variable information
  int vmin(int var);
  int vmax(int var);
  VarRanges<IntVar> vranges(int var);

  // Domain
  void dom(int var, DomSpec& ds);
  void domR(int var, DomSpec& ds, int boolvar);

  // Propagators
  void rel(int, reltype, int);
  void relI(int, reltype, int);
  void relR(int, reltype, int, int);
  void relIR(int, reltype, int, int);

  void eq(int, int, conlevel);
  void eqV(const IntArgs& a, conlevel);
  void eqR(int, int, int, conlevel);

  // Distinct constraints
  void distinct(const IntArgs& a, conlevel cl);
  void distinctI(const IntArgs& offsets, const IntArgs& vars, conlevel cl);

  // Linear equations
  void linear(const IntArgs& coefficients, const IntArgs& vars, reltype rel,
	      int constant, conlevel cl);
  void linearR(const IntArgs& coefficients, const IntArgs& vars, reltype rel,
	       int constant, int boolVar, conlevel cl);


  // Counting constraints
  void countII(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void countIV(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void countVI(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void countVV(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);

  // Access constraints

  void element(const IntArgs& vars, int i, int j);
  void elementI(const IntArgs& vars, int i, int j);
  void lex(const IntArgs& vars1, reltype rel,
	   const IntArgs& vars2);

  // Boolean constraints
  void bool_not(int, int);
  void bool_and(int, int, int);
  void bool_or(int, int, int);
  void bool_imp(int, int, int);
  void bool_eq(int, int, int);
  void bool_xor(int, int, int);
  
  void bool_andV(const IntArgs& vars, int);
  void bool_orV(const IntArgs& vars, int);

  // Arithmetic constraints

  void min(const IntArgs& vars, int i);
  void max(const IntArgs& vars, int i);
  void abs(int i, int j, conlevel cl);
  void mult(int i, int j, int k);
  void power(int i, int j, int k);

  // Value assignment

  void assign(const IntArgs& vars, AvalSel as);

  // Branching
  void branch(const IntArgs& vars, BvarSel, BvalSel);

  // Faling
  void fail();

  // Finite Set Variables / Constraints

  int AddSetVariable();
  
  UBIter<SetVar> fs_upperBound(int);
  LBIter<SetVar> fs_lowerBound(int);
  RangesMinus<UBIter<SetVar>, LBIter<SetVar> > fs_unknown(int);
  int fs_cardinalityMin(int);
  int fs_cardinalityMax(int);
  bool fs_assigned(int);

  template<class I>
  void fs_lowerBound(int s, I& i) {
    if (!enter()) return;
    if (Space::failed())
      return;
    
    GECODE_CME_SPACE(fss[s].includeI(i));
  }

  template<class I>
  void fs_upperBound(int s, I& i) {
    if (!enter()) return;
    if (Space::failed())
      return;
    
    GECODE_CME_SPACE(fss[s].intersectI(i));
  }


  unsigned int fs_upperBoundSize(int);
  unsigned int fs_lowerBoundSize(int);

  void fs_include(int, int);
  void fs_exclude(int, int);
  void fs_the(int, int);
  void fs_min(int, int);
  void fs_max(int, int);
  void fs_match(int, const IntArgs&);
  void fs_card(int, int);
  void fs_cardRange(int, int, int);

  void fs_superOfInter(int, int, int);
  void fs_subOfUnion(int, int, int);
  
  void fs_subset(int, int);
  void fs_nosubset(int, int);
  void fs_disjoint(int, int);
  void fs_distinct(int, int);
  void fs_distinctn(const IntArgs&);
  void fs_equals(int, int);
  void fs_convex(int);
  void fs_convexHull(int, int);
  void fs_union(int, int, int);
  void fs_complement(int, int);
  void fs_intersection(int, int, int);
  void fs_difference(int, int, int);
  void fs_partition(int, int, int);
  void fs_unionn(const IntArgs&, int);
  void fs_intersectionn(const IntArgs&, int);
  void fs_partitionn(const IntArgs&, int);

  void fs_includeR(int, int, int);
  void fs_includeRI(int, int, int);
  void fs_equalR(int, int, int);
  void fs_subsetR(int, int, int);

  void fs_selectUnion(int, const IntArgs&, int);
  void fs_selectInter(int, const IntArgs&, int);
  void fs_selectSets(int, const IntArgs&, int);

  void fs_branch(const IntArgs&, SetBvarSel, SetBvalSel);
  
  void fs_print(int);

};

#endif
