//
// Author:
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "generic/Finalization.hh"
#endif

#include "generic/RootSet.hh"
#include "generic/Finalization.hh"

static const int initialSize = 19;

word Finalization::dictionary;
u_int Finalization::counter;

void Finalization::Init() {
  dictionary =
    WeakDictionary::New(initialSize, INVALID_POINTER)->ToWord();
  RootSet::Add(dictionary);
  counter = 0;
}
