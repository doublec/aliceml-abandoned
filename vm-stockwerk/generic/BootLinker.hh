//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__BOOT_LINKER_HH__
#define __GENERIC__BOOT_LINKER_HH__

#if defined(INTERFACE)
#pragma interface "generic/BootLinker.hh"
#endif

#include "adt/HashTable.hh"
#include "adt/Queue.hh"
#include "generic/String.hh"

class String;

struct NativeComponent {
  const char *name;
  word (*init)(void);
};

// Component
class Component : private Block {
private:
  static const u_int ENTRY_LABEL = MIN_DATA_LABEL;
  static const u_int SIGN_POS    = 0;
  static const u_int STR_POS     = 1;
  static const u_int SIZE        = 2;
public:
  using Block::ToWord;
  // Component Accessors
  word GetSign() {
    return GetArg(SIGN_POS);
  }
  word GetStr() {
    return GetArg(STR_POS);
  }
  // Component Constructor
  static Component *New(word sign, word str) {
    Block *p = Store::AllocBlock((BlockLabel) ENTRY_LABEL, SIZE);
    p->InitArg(SIGN_POS, sign);
    p->InitArg(STR_POS, str);
    return static_cast<Component *>(p);
  }
  // Component Untagging
  static Component *FromWordDirect(word entry) {
    Block *p = Store::DirectWordToBlock(entry);
    Assert(p->GetLabel() == (BlockLabel) ENTRY_LABEL);
    return static_cast<Component *>(p);
  }
};

class BootLinker {
private:
  static word componentTable;
  static word keyQueue;
  static u_int numberOfEntries;
  static HashTable *GetComponentTable() {
    return HashTable::FromWordDirect(componentTable);
  }
public:
  // BootLinker Functions
  static Queue *GetKeyQueue() {
    return Queue::FromWordDirect(keyQueue);
  }
  static u_int GetNumberOfEntries() {
    return numberOfEntries;
  }
  static void EnterComponent(String *key, word sign, word str);
  static Component *LookupComponent(String *key);
  static word Link(String *url);
  // BootLinker Static Constructor
  static void Init(NativeComponent *nativeComponents);
};

#endif
