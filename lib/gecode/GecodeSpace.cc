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

int GecodeSpace::AddIntVariable(int i, int j) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, i, j);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddIntVariable(const DomSpec& d) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, d);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddIntVariableR(int i, int j, int boolVar) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, i, j);
  is[noOfIntVars] = tmp[0];
  dom(tmp[0], i, j, static_cast<BoolVar>(is[boolVar].core()));

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddIntVariableR(DomSpec& d, int boolVar) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(1, d);
  is[noOfIntVars] = tmp[0];
  dom(tmp[0], d, static_cast<BoolVar>(is[boolVar].core()));

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::AddBoolVariable() {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }

  BoolVarArray tmp(1);

  is[noOfIntVars] = static_cast<IntVar>(tmp[0]);

  noOfIntVars++;
  return noOfIntVars-1;
  
}

void GecodeSpace::EnlargeIntVarArray() {
  IntVarArray na(intArraySize*2);
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
void GecodeSpace::tdom(int var, int min, int max) {
  enter();
  if (!failed())
    dom(is[var], min, max);
}
//  void dom(int var, int min, int max, int boolvar);

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
    rel(is[var1], relation, is[var2], static_cast<BoolVar>(is[boolVar].core()));
}
void GecodeSpace::treliR(int var1, reltype relation, int i, int boolVar) {
  enter();
  if (!failed())
    rel(is[var1], relation, i, static_cast<BoolVar>(is[boolVar].core()));
}
void GecodeSpace::teq(int var1, int var2, conlevel cl) {
  enter();
  if (!failed())
    eq(is[var1], is[var2], cl);
}
void GecodeSpace::teq(int vars[], int noOfVars, conlevel cl) {
  makeintvararray(a,vars,noOfVars);
  enter();
  if (!failed())
    eq(a, cl);
}
void GecodeSpace::teqR(int var1, int var2, int boolVar, conlevel cl) {
  enter();
  if (!failed())
    eq(is[var1], is[var2], static_cast<BoolVar>(is[boolVar].core()), cl);
}
void GecodeSpace::teqR(int vars[], int noOfVars, int boolVar, conlevel cl) {
  makeintvararray(a,vars,noOfVars);
  enter();
  if (!failed())
    eq(a, static_cast<BoolVar>(is[boolVar].core()), cl);
}

// Distinct constraints
void GecodeSpace::tdistinct(int vars[], int noOfVars, conlevel cl) {
  makeintvararray(a,vars,noOfVars);
  enter();
  if (!failed())
    distinct(a, cl);
}
void GecodeSpace::tdistinct(int offsets[], int vars[],
			   int noOfVars, conlevel cl) {
  makeintvararray(a,vars,noOfVars);
  enter();
  if (!failed())
    distinct(offsets,a, cl);
}


void GecodeSpace::tlinear(int coefficients[], int vars[], int noOfVars,
			  reltype rel,
			  int constant, conlevel cl) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    linear(coefficients, a, rel, constant, cl);
}

void GecodeSpace::tlinearR(int coefficients[], int vars[], int noOfVars,
			   reltype rel,
			   int constant, int boolVar, conlevel cl) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    linear(coefficients, a, rel, constant, 
	   static_cast<BoolVar>(is[boolVar].core()), cl);
}

// Counting constraints
void GecodeSpace::tcountii(int vars[], int noOfVars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    count(a, rel, i, rel2, j);
}
void GecodeSpace::tcountiv(int vars[], int noOfVars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    count(a, rel, i, rel2, is[j]);
}
void GecodeSpace::tcountvi(int vars[], int noOfVars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    count(a, rel, is[i], rel2, j);
}
void GecodeSpace::tcountvv(int vars[], int noOfVars, reltype rel,
			   int i, reltype rel2, int j) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    count(a, rel, is[i], rel2, is[j]);
}


// Access constraints

void GecodeSpace::telement(int vars[], int noOfVars, int i, int j) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if(!failed())
    element(a, is[i], is[j]);
}
void GecodeSpace::telementi(int args[], int noOfArgs, int i, int j) {
  enter();
  if(!failed())
    element(args, noOfArgs, is[i], is[j]);
}
void GecodeSpace::tlex(int vars1[], int noOfVars1, reltype rel,
		       int vars2[], int noOfVars2) {
  makeintvararray(a, vars1, noOfVars1);
  makeintvararray(b, vars2, noOfVars2);
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
void GecodeSpace::tbool_and(int vars[], int noOfVars, int b) {
  makeboolvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    bool_and(a, intvar2boolvar(is[b]));
}
void GecodeSpace::tbool_or(int vars[], int noOfVars, int b) {
  makeboolvararray(a, vars, noOfVars);
  enter();
  if (!failed())
    bool_or(a, intvar2boolvar(is[b]));
}


// Arithmetic constraints

void GecodeSpace::tmin(int vars[], int noOfVars, int i) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if(!failed())
    min(a, is[i]);
}
void GecodeSpace::tmax(int vars[], int noOfVars, int i) {
  makeintvararray(a, vars, noOfVars);
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

void GecodeSpace::tassign(int vars[], int noOfVars, AvalSel as) {
  makeintvararray(a, vars, noOfVars);
  enter();
  if(!failed())
    assign(a, as);
}

void GecodeSpace::tbranch(int vars[], int noOfVars,
			 BvarSel varSel, BvalSel valSel) {
  makeintvararray(a,vars,noOfVars);
  enter();
  if (!failed())
    branch(a,varSel,valSel);
}
