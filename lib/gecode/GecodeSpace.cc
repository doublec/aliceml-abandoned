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

int GecodeSpace::AddIntVariable(DomSpec& ds) {
  if (!enter()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, ds);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddIntVariableR(DomSpec& ds, int boolVar) {
  if (!enter()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, ds);
  is[noOfIntVars] = tmp[0];
  ::dom(tmp[0], ds, intvar2boolvar(is[boolVar]));

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddBoolVariable() {
  if (!enter()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }

  BoolVarArray tmp(1);

  is[noOfIntVars] = static_cast<IntVar>(tmp[0]);

  noOfIntVars++;
  return noOfIntVars-1;
  
}

void GecodeSpace::EnlargeIntVarArray() {
  if (!enter()) return;

  IntVarArray na(intArraySize*2, 0,0);
  for (int i=noOfIntVars; i--;)
    na[i] = is[i];

  is = na;

  intArraySize *= 2;

  return;
}

int GecodeSpace::vmin(int var) {
  return is[var].min();
}

int GecodeSpace::vmax(int var) {
  return is[var].max();
}

// Domain
void GecodeSpace::dom(int var, DomSpec& ds) {
  if (!enter()) return;
  ::dom(is[var], ds);
}

void GecodeSpace::domR(int var, DomSpec& ds, int boolvar) {
  if (!enter()) return;
  ::dom(is[var], ds, intvar2boolvar(is[boolvar]));
}

// Propagators
void GecodeSpace::rel(int var1, reltype relation, int var2) {
  if (!enter()) return;
  ::rel(is[var1], relation, is[var2]);
}
void GecodeSpace::relI(int var1, reltype relation, int i) {
  if (!enter()) return;
  ::rel(is[var1], relation, i);
}
void GecodeSpace::relR(int var1, reltype relation, int var2, int boolVar) {
  if (!enter()) return;
  ::rel(is[var1], relation, is[var2], intvar2boolvar(is[boolVar]));
}
void GecodeSpace::relIR(int var1, reltype relation, int i, int boolVar) {
  if (!enter()) return;
  ::rel(is[var1], relation, i, intvar2boolvar(is[boolVar]));
}
void GecodeSpace::eq(int var1, int var2, conlevel cl) {
  if (!enter()) return;
  ::eq(is[var1], is[var2], cl);
}
void GecodeSpace::eqV(const IntArgs& vars, conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::eq(a, cl);
}
void GecodeSpace::eqR(int var1, int var2, int boolVar, conlevel cl) {
  if (!enter()) return;
  ::eq(is[var1], is[var2], intvar2boolvar(is[boolVar]));
}

// Distinct constraints
void GecodeSpace::distinct(const IntArgs& vars, conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::distinct(a, cl);
}
void GecodeSpace::distinctI(const IntArgs& offsets, const IntArgs& vars,
			    conlevel cl) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::distinct(offsets,a, cl);
}

void GecodeSpace::linear(const IntArgs& coefficients, const IntArgs& vars,
			 reltype rel,
			 int constant, conlevel cl) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, cl);
}

void GecodeSpace::linearR(const IntArgs& coefficients, const IntArgs& vars,
			   reltype rel,
			   int constant, int boolVar, conlevel cl) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, 
	   intvar2boolvar(is[boolVar]), cl);
}

// Counting constraints
void GecodeSpace::countII(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, j);
}
void GecodeSpace::countIV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, is[j]);
}
void GecodeSpace::countVI(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, j);
}
void GecodeSpace::countVV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, is[j]);
}


// Access constraints

void GecodeSpace::element(const IntArgs& vars, int i, int j) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::element(a, is[i], is[j]);
}
void GecodeSpace::elementI(const IntArgs& args, int i, int j) {
  if (!enter()) return;
  ::element(args, is[i], is[j]);
}
void GecodeSpace::lex(const IntArgs& vars1, reltype rel,
		      const IntArgs& vars2) {
  makeintvarargs(a, vars1);
  makeintvarargs(b, vars2);
  if (!enter()) return;
  ::lex(a, rel, b);
}


// Boolean constraints

void GecodeSpace::bool_not(int a, int b) {
  if  (!enter()) return;
  ::bool_not(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]));
}
void GecodeSpace::bool_and(int a, int b, int c) {
  if (!enter()) return;
  ::bool_and(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::bool_or(int a, int b, int c) {
  if (!enter()) return;
  ::bool_or(intvar2boolvar(is[a]),
	    intvar2boolvar(is[b]),
	    intvar2boolvar(is[c]));
}
void GecodeSpace::bool_imp(int a, int b, int c) {
  if (!enter()) return;
  ::bool_imp(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::bool_eq(int a, int b, int c) {
  if (!enter()) return;
  ::bool_eqv(intvar2boolvar(is[a]),
             intvar2boolvar(is[b]),
             intvar2boolvar(is[c]));
}
void GecodeSpace::bool_xor(int a, int b, int c) {
  if (!enter()) return;
  ::bool_xor(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::bool_andV(const IntArgs& vars, int b) {
  makeboolvarargs(a, vars);
  if (!enter()) return;
  ::bool_and(a, intvar2boolvar(is[b]));
}
void GecodeSpace::bool_orV(const IntArgs& vars, int b) {
  makeboolvarargs(a, vars);
  if (!enter()) return;
  ::bool_or(a, intvar2boolvar(is[b]));
}


// Arithmetic constraints

void GecodeSpace::min(const IntArgs& vars, int i) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::min(a, is[i]);
}
void GecodeSpace::max(const IntArgs& vars, int i) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::max(a, is[i]);
}
void GecodeSpace::abs(int i, int j, conlevel cl) {
  if (!enter()) return;
  ::abs(is[i], is[j], cl);
}
void GecodeSpace::mult(int i, int j, int k) {
  if (!enter()) return;
  ::mult(is[i], is[j], is[k]);
}
void GecodeSpace::power(int i, int j, int k) {
  if (!enter()) return;
  ::pow(is[i], is[j], is[k]);
}

// Value assignment

void GecodeSpace::assign(const IntArgs& vars, AvalSel as) {
  makeintvarargs(a, vars);
  if (!enter()) return;
  ::assign(a, as);
}

void GecodeSpace::branch(const IntArgs& vars,
			 BvarSel varSel, BvalSel valSel) {
  makeintvarargs(a,vars);
  if (!enter()) return;
  ::branch(a,varSel,valSel);
}

void GecodeSpace::fail() {
  if (!enter()) return;
  Space::fail();
}

// FS Variables

int GecodeSpace::AddSetVariable() {
  if (!enter()) return -1;

  if (noOfSetVars >= fsArraySize) {
    EnlargeSetVarArray();
  }
  
  noOfSetVars++;
  return noOfSetVars-1;
}

void GecodeSpace::EnlargeSetVarArray() {
  if (!enter()) return;

  SetVarArray na(fsArraySize*2);
  for (int i=noOfSetVars; i--;)
    na[i] = fss[i];

  fss = na;

  fsArraySize *= 2;

  return;
}

UBIter<SetVar> GecodeSpace::fs_upperBound(int s) {
  enter();
  UBIter<SetVar> ub(fss[s]);
  return ub;
}
LBIter<SetVar> GecodeSpace::fs_lowerBound(int s) {
  enter();
  LBIter<SetVar> lb(fss[s]);
  return lb;
}
RangesMinus<UBIter<SetVar>, LBIter<SetVar> > GecodeSpace::fs_unknown(int s) {
  enter();
  UBIter<SetVar> ub(fss[s]);
  LBIter<SetVar> lb(fss[s]);
  RangesMinus<UBIter<SetVar>, LBIter<SetVar> > m(ub,lb);
  return m;
}

unsigned int GecodeSpace::fs_upperBoundSize(int s) {
  return fss[s].upperBoundSize();
}
unsigned int GecodeSpace::fs_lowerBoundSize(int s) {
  return fss[s].lowerBoundSize();
}

int GecodeSpace::fs_cardinalityMin(int s) {
  enter();
  return fss[s].cardMin();
}
int GecodeSpace::fs_cardinalityMax(int s) {
  enter();
  return fss[s].cardMax();
}
bool GecodeSpace::fs_assigned(int s) {
  enter();
  return fss[s].assigned();
}

void GecodeSpace::fs_include(int d, int s) {
  if (!enter()) return;
  ::include(fss[s], is[d]);
}
void GecodeSpace::fs_exclude(int d, int s) {
  if (!enter()) return;
  ::exclude(fss[s], is[d]);
}
void GecodeSpace::fs_the(int d, int s) {
  if (!enter()) return;
  ::the(fss[s], is[d]);
}
void GecodeSpace::fs_min(int d, int s) {
  if (!enter()) return;
  ::minElement(fss[s], is[d]);
}
void GecodeSpace::fs_max(int d, int s) {
  if (!enter()) return;
  ::maxElement(fss[s], is[d]);
}
void GecodeSpace::fs_match(int s, const IntArgs& vars) {
  if (!enter()) return;
  makeintvarargs(a, vars);
  ::match(fss[s], a);
}
void GecodeSpace::fs_card(int s, int d) {
  if (!enter()) return;
  ::card(fss[s], is[d]);
}
void GecodeSpace::fs_cardRange(int s, int min, int max) {
  if (!enter()) return;
  ::cardRange(fss[s], min, max);
}

void GecodeSpace::fs_superOfInter(int s1, int s2, int s3) {
  if (!enter()) return;
  ::superOfInter(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::fs_subOfUnion(int s1, int s2, int s3) {
  if (!enter()) return;
  ::subOfUnion(fss[s1], fss[s2], fss[s3]);
}

void GecodeSpace::fs_subset(int s1, int s2) {
  if (!enter()) return;
  ::subset(fss[s1], fss[s2]);
}
void GecodeSpace::fs_nosubset(int s1, int s2) {
  if (!enter()) return;
  ::noSubset(fss[s1], fss[s2]);
}
void GecodeSpace::fs_disjoint(int s1, int s2) {
  if (!enter()) return;
  ::disjoint(fss[s1], fss[s2]);
}
void GecodeSpace::fs_distinct(int s1, int s2) {
  if (!enter()) return;
  ::distinct(fss[s1], fss[s2]);
}
void GecodeSpace::fs_distinctn(const IntArgs& vars) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::distinct(a);
}
void GecodeSpace::fs_equals(int s1, int s2) {
  if (!enter()) return;
  ::equals(fss[s1], fss[s2]);
}
void GecodeSpace::fs_convex(int s) {
  if (!enter()) return;
  ::convex(fss[s]);
}
void GecodeSpace::fs_convexHull(int s1, int s2) {
  if (!enter()) return;
  ::convexHull(fss[s1], fss[s2]);
}
void GecodeSpace::fs_union(int s1, int s2, int s3) {
  if (!enter()) return;
  ::fsunion(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::fs_complement(int s1, int s2) {
  if (!enter()) return;
  ::complement(fss[s1], fss[s2]);
}
void GecodeSpace::fs_intersection(int s1, int s2, int s3) {
  if (!enter()) return;
  ::intersection(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::fs_difference(int s1, int s2, int s3) {
  if (!enter()) return;
  ::difference(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::fs_partition(int s1, int s2, int s3) {
  if (!enter()) return;
  ::partition(fss[s1], fss[s2], fss[s3]);
}
void GecodeSpace::fs_unionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::fsunion(a, fss[s]);
}
void GecodeSpace::fs_intersectionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::intersection(a, fss[s]);
}
void GecodeSpace::fs_partitionn(const IntArgs& vars, int s) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::partition(a, fss[s]);
}
void GecodeSpace::fs_includeR(int d, int s, int b) {
  if (!enter()) return;
  ::include(fss[s],is[d],intvar2boolvar(is[b]));
}
void GecodeSpace::fs_includeRI(int i, int s, int b) {
  if (!enter()) return;
  ::include(fss[s],i,intvar2boolvar(is[b]));
}
void GecodeSpace::fs_equalR(int s1, int s2, int b) {
  if (!enter()) return;
  ::equal(fss[s1],fss[s2],intvar2boolvar(is[b]));
}
void GecodeSpace::fs_subsetR(int s1, int s2, int b) {
  if (!enter()) return;
  ::subset(fss[s1],fss[s2],intvar2boolvar(is[b]));
}
void GecodeSpace::fs_selectUnion(int s, const IntArgs& vars, int ss) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectUnion(fss[s], a, fss[ss]);
}
void GecodeSpace::fs_selectInter(int s, const IntArgs& vars, int ss) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectUnion(fss[s], a, fss[ss]);
}
void GecodeSpace::fs_selectSets(int s, const IntArgs& vars, int d) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::selectSets(fss[s], a, is[d]);
}

void GecodeSpace::fs_branch(const IntArgs& vars, SetBvarSel varSel, 
			    SetBvalSel valSel) {
  if (!enter()) return;
  makefsvarargs(a, vars);
  ::branch(a, varSel, valSel);
}

void GecodeSpace::fs_print(int s) {
  if (!enter()) return;
  std::cout << fss[s];
}
