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

#define makeintvararray(a,vars,noofvars)               \
  IntVarArray a(noOfVars);                             \
{ for (int i=noOfVars; i--;) a[i] = is[vars[i]]; }

#define intvar2boolvar(intvar)                \
  static_cast<BoolVar>(intvar.core())

#define makeboolvararray(a,vars,noofvars)               \
  BoolVarArray a(noOfVars);                             \
{ for (int i=noOfVars; i--;) a[i] = intvar2boolvar(is[vars[i]]); }

class GecodeSpace : public Space {
protected:
  IntVarArray is;
  IntVarArray tmpVar;
  int noOfIntVars;
  int intArraySize;

  void EnlargeIntVarArray();

  
public:
  GecodeSpace() : is(10), noOfIntVars(0), intArraySize(10), tmpVar(1,0,0)
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

  int AddIntVariable(int i, int j);
  int AddIntVariable(const DomSpec &d);
  int AddIntVariableR(int i, int j, int boolVar);
  int AddIntVariableR(DomSpec &d, int boolVar);
  
  int AddBoolVariable();
  
  void TrimVars();

  // Inspect variable information
  int vmin(int var);
  int vmax(int var);

  // Domain
  void tdom(int var, int min, int max);
  //  void dom(int var, int min, int max, int boolvar);

  // Propagators
  void trel(int, reltype, int);
  void treli(int, reltype, int);
  void trelR(int, reltype, int, int);
  void treliR(int, reltype, int, int);

  void teq(int, int, conlevel);
  void teq(int[], int, conlevel);
  void teqR(int, int, int, conlevel);
  void teqR(int[], int, int, conlevel);

  // Distinct constraints
  void tdistinct(int vars[], int noOfVars, conlevel cl);
  void tdistinct(int offsets[], int vars[], int noOfVars, conlevel cl);

  // Linear equations
  void tlinear(int coefficients[], int vars[], int noOfVars, reltype rel,
	       int constant, conlevel cl);
  void tlinearR(int coefficients[], int vars[], int noOfVars, reltype rel,
		int constant, int boolVar, conlevel cl);

  // Boolean constraints
  void tbool_not(int, int);
  void tbool_and(int, int, int);
  void tbool_or(int, int, int);
  void tbool_imp(int, int, int);
  void tbool_eq(int, int, int);
  void tbool_xor(int, int, int);
  
  void tbool_and(int[], int, int);
  void tbool_or(int[], int, int);


  // Branching
  void tbranch(int vars[], int noOfVars, BvarSel, BvalSel);
};

#endif
