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

int GecodeSpace::new_intvar(DomSpec& ds) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }
  
  IntVarArray tmp(this,1, ds);
  is[noOfIntVars] = tmp[0];

  noOfIntVars++;
  return noOfIntVars-1;
}

int GecodeSpace::new_boolvar(void) {
  if (noOfIntVars >= intArraySize) {
    EnlargeIntVarArray();
  }

  BoolVarArray tmp(this,1);

  is[noOfIntVars] = static_cast<IntVar>(tmp[0]);

  noOfIntVars++;
  return noOfIntVars-1;
  
}

void GecodeSpace::EnlargeIntVarArray(void) {
  IntVarArray na(this,intArraySize*2, 0,0);
  for (int i=noOfIntVars; i--;)
    na[i] = is[i];

  is = na;

  intArraySize *= 2;

  return;
}

// FS Variables

int GecodeSpace::new_setvar(void) {
  if (noOfSetVars >= fsArraySize) {
    EnlargeSetVarArray();
  }
  
  noOfSetVars++;
  return noOfSetVars-1;
}

void GecodeSpace::EnlargeSetVarArray(void) {
  SetVarArray na(this, fsArraySize*2);
  for (int i=noOfSetVars; i--;)
    na[i] = fss[i];

  fss = na;

  fsArraySize *= 2;

  return;
}
