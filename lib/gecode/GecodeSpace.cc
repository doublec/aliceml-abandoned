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
  for (int i = noOfIntVars; i<intArraySize*2; i++) {
    na[i] = tmpVar[0];
  }

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
  makeintvararray(a,vars);
  if (!enter()) return;
  ::eq(a, cl);
}
void GecodeSpace::eqR(int var1, int var2, int boolVar, conlevel cl) {
  if (!enter()) return;
  ::eq(is[var1], is[var2], intvar2boolvar(is[boolVar]));
}
void GecodeSpace::eqVR(const IntArgs& vars, int boolVar, conlevel cl) {
  makeintvararray(a,vars);
  if (!enter()) return;
  ::eq(a, intvar2boolvar(is[boolVar]), cl);
}

// Distinct constraints
void GecodeSpace::distinct(const IntArgs& vars, conlevel cl) {
  makeintvararray(a,vars);
  if (!enter()) return;
  ::distinct(a, cl);
}
void GecodeSpace::distinctI(const IntArgs& offsets, const IntArgs& vars,
			    conlevel cl) {
  makeintvararray(a,vars);
  if (!enter()) return;
  ::distinct(offsets,a, cl);
}

void GecodeSpace::linear(const IntArgs& coefficients, const IntArgs& vars,
			 reltype rel,
			 int constant, conlevel cl) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, cl);
}

void GecodeSpace::linearR(const IntArgs& coefficients, const IntArgs& vars,
			   reltype rel,
			   int constant, int boolVar, conlevel cl) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::linear(coefficients, a, rel, constant, 
	   intvar2boolvar(is[boolVar]), cl);
}

// Counting constraints
void GecodeSpace::countII(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, j);
}
void GecodeSpace::countIV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::count(a, rel, i, rel2, is[j]);
}
void GecodeSpace::countVI(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, j);
}
void GecodeSpace::countVV(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::count(a, rel, is[i], rel2, is[j]);
}


// Access constraints

void GecodeSpace::element(const IntArgs& vars, int i, int j) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::element(a, is[i], is[j]);
}
void GecodeSpace::elementI(const IntArgs& args, int i, int j) {
  if (!enter()) return;
  ::element(args, is[i], is[j]);
}
void GecodeSpace::lex(const IntArgs& vars1, reltype rel,
		      const IntArgs& vars2) {
  makeintvararray(a, vars1);
  makeintvararray(b, vars2);
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
  ::bool_eq(intvar2boolvar(is[a]),
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
  makeboolvararray(a, vars);
  if (!enter()) return;
  ::bool_and(a, intvar2boolvar(is[b]));
}
void GecodeSpace::bool_orV(const IntArgs& vars, int b) {
  makeboolvararray(a, vars);
  if (!enter()) return;
  ::bool_or(a, intvar2boolvar(is[b]));
}


// Arithmetic constraints

void GecodeSpace::min(const IntArgs& vars, int i) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::min(a, is[i]);
}
void GecodeSpace::max(const IntArgs& vars, int i) {
  makeintvararray(a, vars);
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
  ::powerB(is[i], is[j], is[k]);
}

// Value assignment

void GecodeSpace::assign(const IntArgs& vars, AvalSel as) {
  makeintvararray(a, vars);
  if (!enter()) return;
  ::assign(a, as);
}

void GecodeSpace::branch(const IntArgs& vars,
			 BvarSel varSel, BvalSel valSel) {
  makeintvararray(a,vars);
  if (!enter()) return;
  ::branch(a,varSel,valSel);
}

void GecodeSpace::fail() {
  if (!enter()) return;
  Space::fail();
}
