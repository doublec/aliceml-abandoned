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

#include "alice/Base.hh"

#if HAVE_LIGHTNING

class ImmediateEnv {
protected:
  u_int index;
  u_int size;
  Tuple *values;
public:
  ImmediateEnv() {}

  void Init() {
    index  = 0;
    size   = 5;
    values = Tuple::New(size);
  }
  u_int Register(word item) {
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
  word Sel(u_int index) {
    return values->Sel(index);
  }
  void Replace(u_int index, word item) {
    values->Init(index, item);
  }
  word ExportEnv() {
    return values->ToWord();
  }
};

#endif

#endif
