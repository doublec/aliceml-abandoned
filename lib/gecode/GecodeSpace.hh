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
#include "gecode-search.hh"

#define makeintvararray(a,vars)                                 \
  IntVarArray a(vars.size(), 0,0);                              \
{ int s = vars.size(); for (int i=s; i--;) a[i] = is[vars[i]]; }

#define intvar2boolvar(intvar)        \
  static_cast<BoolVar>(intvar.core())

#define makeboolvararray(a,vars)           \
  BoolVarArray a(vars.size());             \
{ int s = vars.size(); for (int i=s; i--;) \
  a[i] = intvar2boolvar(is[vars[i]]); }

class GecodeSpace : private Space {
protected:
  IntVarArray is;
  IntVarArray tmpVar;
  int noOfIntVars;
  int intArraySize;

  void EnlargeIntVarArray();

  
public:
  using Space::clone;
  using Space::status;
  using Space::commit;
  using Space::operator new;
  using Space::operator delete;

  GecodeSpace() : tmpVar(1,0,0), is(10, 0,0), noOfIntVars(0),
		  intArraySize(10)
  {
    for (int i=10; i--;)
      is[i] = tmpVar[0];
  }

  explicit
  GecodeSpace(GecodeSpace& s) : Space(s), 
				is(s.is.copy()),
                                noOfIntVars(s.noOfIntVars),
                                intArraySize(s.intArraySize),
                                tmpVar(s.tmpVar.copy()) {}

  virtual Space* copy(void) {
    return new GecodeSpace(*this);
  }

  int AddIntVariable(DomSpec& ds);
  int AddIntVariableR(DomSpec& ds, int boolVar);  
  int AddBoolVariable();

  // Inspect variable information
  int vmin(int var);
  int vmax(int var);

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
  void eqVR(const IntArgs& a, int, conlevel);

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
};

#endif
