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
  enter();
  if (failed()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, ds);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddIntVariableR(DomSpec& ds, int boolVar) {
  enter();
  if (failed()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, ds);
  is[noOfIntVars] = tmp[0];
  dom(tmp[0], ds, intvar2boolvar(is[boolVar]));

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddBoolVariable() {
  enter();
  if (failed()) return -1;

  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }

  BoolVarArray tmp(1);

  is[noOfIntVars] = static_cast<IntVar>(tmp[0]);

  noOfIntVars++;
  return noOfIntVars-1;
  
}

void GecodeSpace::EnlargeIntVarArray() {
  enter();
  if (failed()) return;

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

void TrimVars() {
  // Trims the variable arrays to the size actually used

  return;
}

int GecodeSpace::vmin(int var) {
  return is[var].min();
}

int GecodeSpace::vmax(int var) {
  return is[var].max();
}

// Domain
void GecodeSpace::tdom(int var, DomSpec& ds) {
  enter();
  if (!failed())
    dom(is[var], ds);
}

void GecodeSpace::tdom(int var, DomSpec& ds, int boolvar) {
  enter();
  if (!failed())
    dom(is[var], ds, intvar2boolvar(is[boolvar]));
}

// Propagators
void GecodeSpace::trel(int var1, reltype relation, int var2) {
  enter();
  if (!failed())
    rel(is[var1], relation, is[var2]);
}
void GecodeSpace::treli(int var1, reltype relation, int i) {
  enter();
  if (!failed())
    rel(is[var1], relation, i);
}
void GecodeSpace::trelR(int var1, reltype relation, int var2, int boolVar) {
  enter();
  if (!failed())
    rel(is[var1], relation, is[var2], intvar2boolvar(is[boolVar]));
}
void GecodeSpace::treliR(int var1, reltype relation, int i, int boolVar) {
  enter();
  if (!failed())
    rel(is[var1], relation, i, intvar2boolvar(is[boolVar]));
}
void GecodeSpace::teq(int var1, int var2, conlevel cl) {
  enter();
  if (!failed())
    eq(is[var1], is[var2], cl);
}
void GecodeSpace::teq(const IntArgs& vars, conlevel cl) {
  makeintvararray(a,vars);
  enter();
  if (!failed())
    eq(a, cl);
}
void GecodeSpace::teqR(int var1, int var2, int boolVar, conlevel cl) {
  enter();
  if (!failed())
    eq(is[var1], is[var2], intvar2boolvar(is[boolVar]));
}
void GecodeSpace::teqR(const IntArgs& vars, int boolVar, conlevel cl) {
  makeintvararray(a,vars);
  enter();
  if (!failed())
    eq(a, intvar2boolvar(is[boolVar]), cl);
}

// Distinct constraints
void GecodeSpace::tdistinct(const IntArgs& vars, conlevel cl) {
  makeintvararray(a,vars);
  enter();
  if (!failed())
    distinct(a, cl);
}
void GecodeSpace::tdistinct(const IntArgs& offsets, const IntArgs& vars,
			    conlevel cl) {
  makeintvararray(a,vars);
  enter();
  if (!failed())
    distinct(offsets,a, cl);
}

void GecodeSpace::tlinear(const IntArgs& coefficients, const IntArgs& vars,
			  reltype rel,
			  int constant, conlevel cl) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    linear(coefficients, a, rel, constant, cl);
}

void GecodeSpace::tlinearR(const IntArgs& coefficients, const IntArgs& vars,
			   reltype rel,
			   int constant, int boolVar, conlevel cl) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    linear(coefficients, a, rel, constant, 
	   intvar2boolvar(is[boolVar]), cl);
}

// Counting constraints
void GecodeSpace::tcountii(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    count(a, rel, i, rel2, j);
}
void GecodeSpace::tcountiv(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    count(a, rel, i, rel2, is[j]);
}
void GecodeSpace::tcountvi(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    count(a, rel, is[i], rel2, j);
}
void GecodeSpace::tcountvv(const IntArgs& vars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars);
  enter();
  if (!failed())
    count(a, rel, is[i], rel2, is[j]);
}


// Access constraints

void GecodeSpace::telement(const IntArgs& vars, int i, int j) {
  makeintvararray(a, vars);
  enter();
  if(!failed())
    element(a, is[i], is[j]);
}
void GecodeSpace::telementi(const IntArgs& args, int i, int j) {
  enter();
  if(!failed())
    element(args, is[i], is[j]);
}
void GecodeSpace::tlex(const IntArgs& vars1, reltype rel,
		       const IntArgs& vars2) {
  makeintvararray(a, vars1);
  makeintvararray(b, vars2);
  enter();
  if(!failed())
    lex(a, rel, b);
}


// Boolean constraints

void GecodeSpace::tbool_not(int a, int b) {
  enter();
  if (!failed())
    bool_not(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]));
}
void GecodeSpace::tbool_and(int a, int b, int c) {
  enter();
  if (!failed())
    bool_and(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::tbool_or(int a, int b, int c) {
  enter();
  if (!failed())
    bool_or(intvar2boolvar(is[a]),
	    intvar2boolvar(is[b]),
	    intvar2boolvar(is[c]));
}
void GecodeSpace::tbool_imp(int a, int b, int c) {
  enter();
  if (!failed())
    bool_imp(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::tbool_eq(int a, int b, int c) {
  enter();
  if (!failed())
    bool_eq(intvar2boolvar(is[a]),
	    intvar2boolvar(is[b]),
	    intvar2boolvar(is[c]));
}
void GecodeSpace::tbool_xor(int a, int b, int c) {
  enter();
  if (!failed())
    bool_xor(intvar2boolvar(is[a]),
	     intvar2boolvar(is[b]),
	     intvar2boolvar(is[c]));
}
void GecodeSpace::tbool_and(const IntArgs& vars, int b) {
  makeboolvararray(a, vars);
  enter();
  if (!failed())
    bool_and(a, intvar2boolvar(is[b]));
}
void GecodeSpace::tbool_or(const IntArgs& vars, int b) {
  makeboolvararray(a, vars);
  enter();
  if (!failed())
    bool_or(a, intvar2boolvar(is[b]));
}


// Arithmetic constraints

void GecodeSpace::tmin(const IntArgs& vars, int i) {
  makeintvararray(a, vars);
  enter();
  if(!failed())
    min(a, is[i]);
}
void GecodeSpace::tmax(const IntArgs& vars, int i) {
  makeintvararray(a, vars);
  enter();
  if(!failed())
    max(a, is[i]);
}
void GecodeSpace::tabs(int i, int j, conlevel cl) {
  enter();
  if(!failed())
    abs(is[i], is[j], cl);
}
void GecodeSpace::tmult(int i, int j, int k) {
  enter();
  if(!failed())
    mult(is[i], is[j], is[k]);
}

// Value assignment

void GecodeSpace::tassign(const IntArgs& vars, AvalSel as) {
  makeintvararray(a, vars);
  enter();
  if(!failed())
    assign(a, as);
}

void GecodeSpace::tbranch(const IntArgs& vars,
			 BvarSel varSel, BvalSel valSel) {
  makeintvararray(a,vars);
  enter();
  if (!failed())
    branch(a,varSel,valSel);
}
