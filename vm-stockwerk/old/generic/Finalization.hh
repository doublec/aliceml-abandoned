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

#ifndef __GENERIC__FINALIZATION_HH__
#define __GENERIC__FINALIZATION_HH__

#if defined(INTERFACE)
#pragma interface "generic/Finalization.hh"
#endif

#include "store/Store.hh"

class Finalization {
private:
  static word dictionary;
  static u_int counter;
public:
  static void Init();

  static u_int Register(word w) {
    u_int key = counter++;
    WeakDictionary::FromWordDirect(dictionary)->InsertItem(key, w);
    return key;
  }
  static void Unregister(u_int key) {
    WeakDictionary::FromWordDirect(dictionary)->DeleteItem(key);
  }
};

#endif __GENERIC__FINALIZATION_HH__
