//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_JITTER_IMMEDIATE_ENV_HH__
#define __ALICE_JITTER_IMMEDIATE_ENV_HH__

#include "Seam.hh"

class ImmediateEnv {
protected:
  static u_int index;
  static u_int size;
  static Tuple *values;
public:
  // ImmediateEnv Static Constructor
  static void Init() {
    index  = 0;
    size   = 5;
    values = Tuple::New(size);
  }
  // ImmediateEnv Methods
  static u_int Register(word item) {
    Assert(item != (word) 0);
    if (index >= size) {
      u_int oldsize = size;
      size = ((size * 3) >> 1); 
      Tuple *newValues = Tuple::New(size);
      for (u_int i = oldsize; i--;)
	newValues->Init(i, values->Sel(i));
      values = newValues;
    }
    values->Init(index, item);
    return index++;
  }
  static word Sel(u_int index) {
    return values->Sel(index);
  }
  static void Replace(u_int index, word item) {
    values->Init(index, item);
  }
  static word ExportEnv() {
    return values->ToWord();
  }
};

#endif
