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

#ifndef __GENERIC__FINALIZATION_SET_HH__
#define __GENERIC__FINALIZATION_SET_HH__

#if defined(INTERFACE)
#pragma interface "generic/FinalizationSet.hh"
#endif

#include "store/WeakMap.hh"
#include "generic/RootSet.hh"

class SeamDll FinalizationSet: public Finalization {
private:
  static const u_int initialSize = 4; //--** to be determined

  u_int keyCounter;
  word wWeakDictionary;
public:
  FinalizationSet() {
    keyCounter = 0;
    wWeakDictionary = WeakMap::New(initialSize, this)->ToWord();
    RootSet::Add(wWeakDictionary);
  }

  u_int Register(word value) {
    WeakMap *weakDictionary = WeakMap::FromWordDirect(wWeakDictionary);
    u_int key = keyCounter++;
    weakDictionary->Put(Store::IntToWord(key), value);
    return key;
  }
  void Unregister(u_int key) {
    WeakMap *weakDictionary = WeakMap::FromWordDirect(wWeakDictionary);
    weakDictionary->Remove(Store::IntToWord(key));
  }
  virtual void Finalize(word value) = 0;
};

#endif
