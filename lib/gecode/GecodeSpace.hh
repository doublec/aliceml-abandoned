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

#include "int.hh"
#include "set.hh"

using namespace Gecode;

#define makeintvarargs(a,vars)                                 \
  IntVarArgs a(vars.size());                                   \
{ int s = vars.size(); for (int i=s; i--;) a[i] = is[vars[i]]; }

#define intvar2boolvar(b, intvar)		\
  Int::IntView iv(intvar);				\
  Int::BoolView bv(iv);				\
  BoolVar b(bv);

#define makeboolvarargs(a,vars)           \
  BoolVarArgs a(vars.size());             \
  { int s = vars.size();                  \
    for (int i=s; i--;) {                 \
      intvar2boolvar(tmp, is[vars[i]]);   \
      a[i] = tmp;                         \
    }                                     \
  }

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
  GecodeSpace() : is(this,3, 0,0), noOfIntVars(0),
		  intArraySize(3),
		  fss(this,3), noOfSetVars(0), fsArraySize(3)
  {}

  explicit
  GecodeSpace(GecodeSpace& s, bool share=true) : Space(s, share),
                                                 noOfIntVars(s.noOfIntVars),
                                                 noOfSetVars(s.noOfSetVars),
                                                 intArraySize(s.intArraySize),
                                                 fsArraySize(s.fsArraySize) {
    is.update(this, s.is, share);
    fss.update(this, s.fss, share);
  }

  virtual Space* copy(bool share) { 
    return new GecodeSpace(*this,share); 
  }

  intvar new_intvar(DomSpec&);
  boolvar new_boolvar(void);
  setvar new_setvar(void);

};

#endif
