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

#define makeintvararray(a,vars)               \
  IntVarArray a(vars.size(), 0,0);                             \
{ int s = vars.size(); for (int i=s; i--;) a[i] = is[vars[i]]; }

#define intvar2boolvar(intvar)                \
  static_cast<BoolVar>(intvar.core())

#define makeboolvararray(a,vars)               \
  BoolVarArray a(vars.size());                             \
{ int s = vars.size(); for (int i=s; i--;) \
  a[i] = intvar2boolvar(is[vars[i]]); }

class GecodeSpace : public Space {
protected:
  IntVarArray is;
  IntVarArray tmpVar;
  int noOfIntVars;
  int intArraySize;

  void EnlargeIntVarArray();

  
public:
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

  //  int AddIntVariable(int i, int j);
  int AddIntVariable(DomSpec& ds);
  //  int AddIntVariableR(int i, int j, int boolVar);
  int AddIntVariableR(DomSpec& ds, int boolVar);  

  int AddBoolVariable();
  
  void TrimVars();

  // Inspect variable information
  int vmin(int var);
  int vmax(int var);

  // Domain
  void tdom(int var, DomSpec& ds);
  void tdom(int var, DomSpec& ds, int boolvar);
  //  void dom(int var, int min, int max, int boolvar);

  // Propagators
  void trel(int, reltype, int);
  void treli(int, reltype, int);
  void trelR(int, reltype, int, int);
  void treliR(int, reltype, int, int);

  void teq(int, int, conlevel);
  void teq(const IntArgs& a, conlevel);
  void teqR(int, int, int, conlevel);
  void teqR(const IntArgs& a, int, conlevel);

  // Distinct constraints
  void tdistinct(const IntArgs& a, conlevel cl);
  void tdistinct(const IntArgs& offsets, const IntArgs& vars, conlevel cl);

  // Linear equations
  void tlinear(const IntArgs& coefficients, const IntArgs& vars, reltype rel,
	       int constant, conlevel cl);
  void tlinearR(const IntArgs& coefficients, const IntArgs& vars, reltype rel,
		int constant, int boolVar, conlevel cl);


  // Counting constraints
  void tcountii(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void tcountiv(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void tcountvi(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);
  void tcountvv(const IntArgs& vars, reltype rel,
	       int i, reltype rel2, int j);

  // Access constraints

  void telement(const IntArgs& vars, int i, int j);
  void telementi(const IntArgs& vars, int i, int j);
  void tlex(const IntArgs& vars1, reltype rel,
	    const IntArgs& vars2);

  // Boolean constraints
  void tbool_not(int, int);
  void tbool_and(int, int, int);
  void tbool_or(int, int, int);
  void tbool_imp(int, int, int);
  void tbool_eq(int, int, int);
  void tbool_xor(int, int, int);
  
  void tbool_and(const IntArgs& vars, int);
  void tbool_or(const IntArgs& vars, int);

  // Arithmetic constraints

  void tmin(const IntArgs& vars, int i);
  void tmax(const IntArgs& vars, int i);
  void tabs(int i, int j, conlevel cl);
  void tmult(int i, int j, int k);

  // Value assignment

  void tassign(const IntArgs& vars, AvalSel as);

  // Branching
  void tbranch(const IntArgs& vars, BvarSel, BvalSel);
};

#endif
