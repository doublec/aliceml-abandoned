//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#include <cstdlib>
#include "emulator/Authoring.hh"

DEFINE0(UnsafeRand_rand) {
  RETURN_INT(rand());
} END

DEFINE1(UnsafeRand_srand) {
  DECLARE_INT(seed, x0);
  srand(seed);
  RETURN_UNIT;
} END

DEFINE0(UnsafeRand_randLimits) {
  Tuple *limits = Tuple::New(2);
  limits->Init(0, Store::IntToWord(0));
  limits->Init(1, Store::IntToWord(RAND_MAX));
  RETURN(limits->ToWord());
} END

word UnsafeRand(void) {
  Tuple *t = Tuple::New(3);
  t->Init(0, Primitive::MakeClosure("UnsafeRand_rand",
				    UnsafeRand_rand, 0, true));
  t->Init(1, Primitive::MakeClosure("UnsafeRand_randLimits",
				    UnsafeRand_randLimits, 0, true));
  t->Init(2, Primitive::MakeClosure("UnsafeRand_srand",
				    UnsafeRand_srand, 1, true));
  RETURN_STRUCTURE(t);
}
