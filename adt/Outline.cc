//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//   Leif Kornstaedt, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "adt/IntMap.hh"
#pragma implementation "adt/Queue.hh"
#pragma implementation "adt/Stack.hh"
#endif

#include "adt/IntMap.hh"
#include "store/BaseMap.cc"
#include "adt/Queue.hh"
#include "adt/Stack.hh"

template class BaseMap<IntKey>;
