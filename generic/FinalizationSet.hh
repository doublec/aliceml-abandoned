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

#include "store/WeakDictionary.hh"
#include "generic/RootSet.hh"

class FinalizationSet: public Finalization {
private:
  static const u_int initialSize = 4; //--** to be determined

  u_int keyCounter;
  word wWeakDictionary;
public:
  FinalizationSet() {
    keyCounter = 0;
    wWeakDictionary = WeakDictionary::New(initialSize, this)->ToWord();
    RootSet::Add(wWeakDictionary);
  }

  u_int Register(word value) {
    WeakDictionary *weakDictionary =
      WeakDictionary::FromWordDirect(wWeakDictionary);
    u_int key = keyCounter++;
    weakDictionary->InsertItem(key, value);
    return key;
  }
  void Unregister(u_int key) {
    WeakDictionary *weakDictionary =
      WeakDictionary::FromWordDirect(wWeakDictionary);
    weakDictionary->DeleteItem(key);
  }
  virtual void Finalize(word value) = 0;
};

#endif
