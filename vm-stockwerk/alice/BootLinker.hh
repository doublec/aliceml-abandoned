//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//   Leif Kornstaedt <kornstae@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//   Leif Kornstaedt, 2002-2003
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __GENERIC__BOOT_LINKER_HH__
#define __GENERIC__BOOT_LINKER_HH__

#if defined(INTERFACE)
#pragma interface "alice/BootLinker.hh"
#endif

#include "adt/ChunkMap.hh"
#include "adt/Queue.hh"
#include "generic/String.hh"

class String;

struct DllExport NativeComponent {
  const char *name;
  word (*init)();
};

// Component
class DllExport Component: private Block {
private:
  static const u_int ENTRY_LABEL = MIN_DATA_LABEL;
  enum { SIGN_POS, STR_POS, SIZE };
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

class DllExport BootLinker {
private:
  static word componentTable;
  static word keyQueue;
  static u_int numberOfEntries;
  static ChunkMap *GetComponentTable() {
    return ChunkMap::FromWordDirect(componentTable);
  }
public:
  // BootLinker Static Constructor
  static void Init(NativeComponent *nativeComponents);
  // BootLinker Functions
  static Queue *GetKeyQueue() {
    return Queue::FromWordDirect(keyQueue);
  }
  static u_int GetNumberOfEntries() {
    return numberOfEntries;
  }
  static void EnterComponent(String *key, word sign, word str);
  static Component *LookupComponent(String *key);
  static void Link(String *url);
};

#endif
