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

#ifndef __EMULATOR__BOOTLINKER_HH__
#define __EMULATOR__BOOTLINKER_HH__

#if defined(INTERFACE)
#pragma interface "emulator/BootLinker.hh"
#endif

#include "adt/HashTable.hh"
#include "adt/Queue.hh"

class String;

typedef struct {
  const char *name;
  word (*init)(void);
} prim_table;

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
    return (Component *) p;
  }
  // Component Untagging
  static Component *FromWord(word entry) {
    Block *p = Store::DirectWordToBlock(entry);
    Assert(p != INVALID_POINTER && p->GetLabel() == (BlockLabel) ENTRY_LABEL);
    return (Component *) p;
  }
};

class BootLinker {
private:
  static word componentTable;
  static word keyQueue;
  static u_int numberOfEntries;
  static u_int traceFlag;
  static char *aliceHome;
  static HashTable *GetComponentTable() {
    return HashTable::FromWordDirect(componentTable);
  }
public:
  // BootLinker Functions
  static void Trace(const char *prefix, Chunk *key);
  static Queue *GetKeyQueue() {
    return Queue::FromWordDirect(keyQueue);
  }
  static u_int GetNumberOfEntries() {
    return numberOfEntries;
  }
  static void EnterComponent(Chunk *key, word sign, word str);
  static Component *LookupComponent(Chunk *key);
  static void SetTraceMode(u_int trace) {
    traceFlag = trace;
  }
  static Chunk *MakeFileName(Chunk *key);
  static word Link(Chunk *url);
  // BootLinker Static Constructor
  static void Init(char *home, prim_table *table);
  static void Print(Chunk *c);
};

#endif
