//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#if defined(INTERFACE)
#pragma implementation "store/WeakMap.hh"
#endif

#include "store/WeakMap.hh"
#include "store/BaseMap.cc"

template class BaseMap<TokenKey>;
