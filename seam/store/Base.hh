//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__BASE_HH__
#define __STORE__BASE_HH__

#include "../Base.hh"

#ifdef STORE_DEBUG
#define AssertStore(Cond) AssertBase(Cond, #Cond)
#else
#define AssertStore(Cond)
#endif

#endif
