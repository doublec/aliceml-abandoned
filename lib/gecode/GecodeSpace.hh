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

class GecodeSpace : public Space {
public:
  IntVarArray is;
  int noOfIntVars;
  int intArraySize;
  
  SetVarArray fss;
  int noOfSetVars;
  int fsArraySize;

  void EnlargeIntVarArray(void);
  void EnlargeSetVarArray(void);

public:
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

  virtual Space* copy(Space* home) {
    return new GecodeSpace(*this);
  }

  intvar new_intvar(DomSpec&);
  boolvar new_boolvar(void);
  setvar new_setvar(void);

};

#endif
