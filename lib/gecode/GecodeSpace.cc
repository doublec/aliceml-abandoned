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

#include "GecodeSpace.hh"

void GecodeSpace::commit(int alt, BranchDesc *desc) {
  Space::commit(alt, desc);
}

int GecodeSpace::new_intvar(DomSpec& ds) {
  if (!enter()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, ds);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::new_boolvar(void) {
  if (!enter()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }

  BoolVarArray tmp(1);

  is[noOfIntVars] = static_cast<IntVar>(tmp[0]);

  noOfIntVars++;
  return noOfIntVars-1;
  
}

void GecodeSpace::EnlargeIntVarArray(void) {
  if (!enter()) return;

  IntVarArray na(intArraySize*2, 0,0);
  for (int i=noOfIntVars; i--;)
    na[i] = is[i];

  is = na;

  intArraySize *= 2;

  return;
}

int GecodeSpace::int_getMin(int var) {
  return is[var].min();
}

int GecodeSpace::int_getMax(int var) {
  return is[var].max();
}

VarRanges<IntVar> GecodeSpace::int_getRanges(int var) {
  VarRanges<IntVar> r(is[var]);
  return r;
}

// Domain
void GecodeSpace::int_dom(int var, DomSpec& ds) {
  if (!enter()) return;
  ::dom(is[var], ds);
}

void GecodeSpace::int_domR(int var, DomSpec& ds, int boolvar) {
  if (!enter()) return;
  ::dom(is[var], ds, intvar2boolvar(is[boolvar]));
}

// Propagators
void GecodeSpace::int_rel(int var1, reltype relation, int var2) {
  if (!enter()) return;
  ::rel(is[var1], relation, is[var2]);
}
void GecodeSpace::int_relI(int var1, reltype relation, int i) {
  if (!enter()) return;
  ::rel(is[var1], relation, i);
}
void GecodeSpace::int_relR(int var1, reltype relation, int var2, int boolVar) {
  if (!enter()) return;
  ::rel(is[var1], relation, is[var2], intvar2boolvar(is[boolVar]));
}
void GecodeSpace::int_relIR(int var1, reltype relation, int i, int boolVar) {
  if (!enter()) return;
  ::rel(is[var1], relation, i, intvar2boolvar(is[boolVar]));
}
void GecodeSpace::int_eq(int var1, int var2, conlevel cl) {
  if (!enter()) return;
  ::eq(is[var1], is[var2], cl);
}
void GecodeSpace::int_eqV(const IntArgs& vars, conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::eq(a, cl);
}
void GecodeSpace::int_eqR(int var1, int var2, int boolVar, conlevel cl) {
  if (!enter()) return;
  ::eq(is[var1], is[var2], intvar2boolvar(is[boolVar]), cl);
}

// Distinct constraints
void GecodeSpace::int_distinct(const IntArgs& vars, conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::distinct(a, cl);
}
void GecodeSpace::int_distinctI(const IntArgs& offsets, const IntArgs& vars,
			    conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::distinct(offsets,a, cl);
}

void GecodeSpace::int_linear(const IntArgs& coefficients, const IntArgs& vars,
			 reltype rel,
			 int constant, conlevel cl) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, cl);
}

void GecodeSpace::int_linearR(const IntArgs& coefficients, const IntArgs& vars,
			   reltype rel,
			   int constant, int boolVar, conlevel cl) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, 
	   intvar2boolvar(is[boolVar]), cl);
}

// Counting constraints
void GecodeSpace::int_countII(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, j);
}
void GecodeSpace::int_countIV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, is[j]);
}
void GecodeSpace::int_countVI(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, j);
}
void GecodeSpace::int_countVV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, is[j]);
}


// Access constraints

void GecodeSpace::int_element(const IntArgs& vars, int i, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::element(a, is[i], is[j]);
}
void GecodeSpace::int_elementI(const IntArgs& args, int i, int j) {
  if (!enter()) return;
  ::element(args, is[i], is[j]);
}
void GecodeSpace::int_lex(const IntArgs& vars1, reltype rel,
		      const IntArgs& vars2) {
  makeintvarargs(a, vars1);
  makeintvarargs(b, vars2);
  if (!enter()) return;
  ::lex(a, rel, b);
}


// Boolean constraints

void GecodeSpace::int_bool_not(int a, int b) {
  if  (!enter()) return;
  ::bool_not(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]));
}
void GecodeSpace::int_bool_and(int a, int b, int c) {
  if (!enter()) return;
  ::bool_and(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::int_bool_or(int a, int b, int c) {
  if (!enter()) return;
  ::bool_or(intvar2boolvar(is[a]),
	    intvar2boolvar(is[b]),
	    intvar2boolvar(is[c]));
}
void GecodeSpace::int_bool_imp(int a, int b, int c) {
  if (!enter()) return;
  ::bool_imp(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::int_bool_eq(int a, int b, int c) {
  if (!enter()) return;
  ::bool_eqv(intvar2boolvar(is[a]),
             intvar2boolvar(is[b]),
             intvar2boolvar(is[c]));
}
void GecodeSpace::int_bool_xor(int a, int b, int c) {
  if (!enter()) return;
  ::bool_xor(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::int_bool_andV(const IntArgs& vars, int b) {
  makeboolvarargs(a, vars);
  if (!enter()) return;
  ::bool_and(a, intvar2boolvar(is[b]));
}
void GecodeSpace::int_bool_orV(const IntArgs& vars, int b) {
  makeboolvarargs(a, vars);
  if (!enter()) return;
  ::bool_or(a, intvar2boolvar(is[b]));
}


// Arithmetic constraints

void GecodeSpace::int_min(const IntArgs& vars, int i) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::min(a, is[i]);
}
void GecodeSpace::int_max(const IntArgs& vars, int i) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::max(a, is[i]);
}
void GecodeSpace::int_abs(int i, int j, conlevel cl) {
  if (!enter()) return;
  ::abs(is[i], is[j], cl);
}
void GecodeSpace::int_mult(int i, int j, int k) {
  if (!enter()) return;
  ::mult(is[i], is[j], is[k]);
}
void GecodeSpace::int_power(int i, int j, int k) {
  if (!enter()) return;
  ::pow(is[i], is[j], is[k]);
}

// Value assignment

void GecodeSpace::int_assign(const IntArgs& vars, AvalSel as) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::assign(a, as);
}

void GecodeSpace::int_branch(const IntArgs& vars,
			 BvarSel varSel, BvalSel valSel) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::branch(a,varSel,valSel);
}

void GecodeSpace::fail(void) {
  if (!enter()) return;
  Space::fail();
}

// FS Variables

int GecodeSpace::new_setvar(void) {
  if (!enter()) return -1;

  if (noOfSetVars >= fsArraySize) {
    EnlargeSetVarArray();
  }
  
  noOfSetVars++;
  return noOfSetVars-1;
}

void GecodeSpace::EnlargeSetVarArray(void) {
  if (!enter()) return;

  SetVarArray na(fsArraySize*2);
  for (int i=noOfSetVars; i--;)
    na[i] = fss[i];

  fss = na;

  fsArraySize *= 2;

  return;
}

UBIter<SetVar> GecodeSpace::set_getUpperBound(int s) {
  enter();
  UBIter<SetVar> ub(fss[s]);
  return ub;
}
LBIter<SetVar> GecodeSpace::set_getLowerBound(int s) {
  enter();
  LBIter<SetVar> lb(fss[s]);
  return lb;
}
Iter::Ranges::Diff<UBIter<SetVar>, LBIter<SetVar> >
GecodeSpace::set_getUnknown(int s) {
  enter();
  UBIter<SetVar> ub(fss[s]);
  LBIter<SetVar> lb(fss[s]);
  Iter::Ranges::Diff<UBIter<SetVar>, LBIter<SetVar> > m(ub,lb);
  return m;
}

unsigned int GecodeSpace::set_getUpperBoundSize(int s) {
  return fss[s].upperBoundSize();
}
unsigned int GecodeSpace::set_getLowerBoundSize(int s) {
  return fss[s].lowerBoundSize();
}

int GecodeSpace::set_getCardinalityMin(int s) {
  enter();
  return fss[s].cardMin();
}
int GecodeSpace::set_getCardinalityMax(int s) {
  enter();
  return fss[s].cardMax();
}
bool GecodeSpace::set_getAssigned(int s) {
  enter();
  return fss[s].assigned();
}

void GecodeSpace::set_include(int d, int s) {
  if (!enter()) return;
  ::include(fss[s], is[d]);
}
void GecodeSpace::set_exclude(int d, int s) {
  if (!enter()) return;
  ::exclude(fss[s], is[d]);
}
void GecodeSpace::set_the(int d, int s) {
  if (!enter()) return;
  ::the(fss[s], is[d]);
}
void GecodeSpace::set_min(int d, int s) {
  if (!enter()) return;
  ::minElement(fss[s], is[d]);
}
void GecodeSpace::set_max(int d, int s) {
  if (!enter()) return;
  ::maxElement(fss[s], is[d]);
}
void GecodeSpace::set_match(int s, const IntArgs& vars) {
  if (!enter()) return;
  makeintvarargs(a, vars);
  ::match(fss[s], a);
}
void GecodeSpace::set_card(int s, int d) {
  if (!enter()) return;
  ::card(fss[s], is[d]);
}
void GecodeSpace::set_cardRange(int s, int min, int max) {
  if (!enter()) return;
  ::cardRange(fss[s], min, max);
}

void GecodeSpace::set_superOfInter(int s1, int s2, int s3) {
  if (!enter()) return;
  ::superOfInter(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::set_subOfUnion(int s1, int s2, int s3) {
  if (!enter()) return;
  ::subOfUnion(fss[s1], fss[s2], fss[s3]);
}

void GecodeSpace::set_subset(int s1, int s2) {
  if (!enter()) return;
  ::subset(fss[s1], fss[s2]);
}
void GecodeSpace::set_nosubset(int s1, int s2) {
  if (!enter()) return;
  ::noSubset(fss[s1], fss[s2]);
}
void GecodeSpace::set_disjoint(int s1, int s2) {
  if (!enter()) return;
  ::disjoint(fss[s1], fss[s2]);
}
void GecodeSpace::set_distinct(int s1, int s2) {
  if (!enter()) return;
  ::distinct(fss[s1], fss[s2]);
}
void GecodeSpace::set_distinctn(const IntArgs& vars) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::distinct(a);
}
void GecodeSpace::set_equals(int s1, int s2) {
  if (!enter()) return;
  ::equals(fss[s1], fss[s2]);
}
void GecodeSpace::set_convex(int s) {
  if (!enter()) return;
  ::convex(fss[s]);
}
void GecodeSpace::set_convexHull(int s1, int s2) {
  if (!enter()) return;
  ::convexHull(fss[s1], fss[s2]);
}
void GecodeSpace::set_union(int s1, int s2, int s3) {
  if (!enter()) return;
  ::fsunion(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::set_complement(int s1, int s2) {
  if (!enter()) return;
  ::complement(fss[s1], fss[s2]);
}
void GecodeSpace::set_intersection(int s1, int s2, int s3) {
  if (!enter()) return;
  ::intersection(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::set_difference(int s1, int s2, int s3) {
  if (!enter()) return;
  ::difference(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::set_partition(int s1, int s2, int s3) {
  if (!enter()) return;
  ::partition(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::set_unionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::fsunion(a, fss[s]);
}
void GecodeSpace::set_intersectionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::intersection(a, fss[s]);
}
void GecodeSpace::set_partitionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::partition(a, fss[s]);
}
void GecodeSpace::set_includeR(int d, int s, int b) {
  if (!enter()) return;
  ::include(fss[s],is[d],intvar2boolvar(is[b]));
}
void GecodeSpace::set_includeRI(int i, int s, int b) {
  if (!enter()) return;
  ::include(fss[s],i,intvar2boolvar(is[b]));
}
void GecodeSpace::set_equalR(int s1, int s2, int b) {
  if (!enter()) return;
  ::equal(fss[s1],fss[s2],intvar2boolvar(is[b]));
}
void GecodeSpace::set_subsetR(int s1, int s2, int b) {
  if (!enter()) return;
  ::subset(fss[s1],fss[s2],intvar2boolvar(is[b]));
}
void GecodeSpace::set_selectUnion(int s, const IntArgs& vars, int ss) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectUnion(fss[s], a, fss[ss]);
}
void GecodeSpace::set_selectInter(int s, const IntArgs& vars, int ss) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectInter(fss[s], a, fss[ss]);
}
void GecodeSpace::set_selectDisjoint(const IntArgs& vars, int ss) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectDisjoint(a, fss[ss]);
}
void GecodeSpace::set_selectSets(int s, const IntArgs& vars, int d) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectSets(fss[s], a, is[d]);
}

void GecodeSpace::set_branch(const IntArgs& vars, SetBvarSel varSel, 
			    SetBvalSel valSel) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::branch(a, varSel, valSel);
}

void GecodeSpace::set_randomBranch(const IntArgs& vars, int seed) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::randomBranch(a, seed);
}

void GecodeSpace::set_print(int s) {
  if (!enter()) return;
  std::cout << fss[s];
}
