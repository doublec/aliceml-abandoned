//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdlib>
#include "alice/Authoring.hh"

DEFINE0(UnsafeRand_rand) {
  RETURN_INT(rand());
} END

DEFINE1(UnsafeRand_srand) {
  DECLARE_INT(seed, x0);
  srand(seed);
  RETURN_UNIT;
} END

static inline s_int min (s_int a, s_int b) {
  if (a <= b) 
    return a;
  else
    return b;
}

DEFINE0(UnsafeRand_randLimits) {
  Tuple *limits = Tuple::New(2);
  limits->Init(0, Store::IntToWord(0));
  limits->Init(1, Store::IntToWord(min (RAND_MAX, MAX_VALID_INT) ));
  RETURN(limits->ToWord());
} END

AliceDll word UnsafeRand() {
  Record *record = Record::New(3);
  INIT_STRUCTURE(record, "UnsafeRand", "rand",
		 UnsafeRand_rand, 0);
  INIT_STRUCTURE(record, "UnsafeRand", "srand",
		 UnsafeRand_srand, 1);
  INIT_STRUCTURE(record, "UnsafeRand", "randLimits",
		 UnsafeRand_randLimits, 0);
  RETURN_STRUCTURE("UnsafeRand$", record);
}
