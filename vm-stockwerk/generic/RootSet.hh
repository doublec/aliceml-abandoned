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

#ifndef __GENERIC__ROOT_SET_HH__
#define __GENERIC__ROOT_SET_HH__

#if defined(INTERFACE)
#pragma interface "generic/RootSet.hh"
#endif

#include "store/Store.hh"

class SeamDll RootSet {
public:
  static void Init();
  static void Add(word &root);
  static void Remove(word &root);
  static void DoGarbageCollection();
};

#endif
