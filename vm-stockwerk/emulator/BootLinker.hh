//
// Authors:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
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

class String;

typedef struct {
  const char *name;
  word (*module)(void);
} prim_table;

// ModuleEntry
class ModuleEntry : private Block {
private:
  static const u_int ENTRY_LABEL = MIN_DATA_LABEL;
  static const u_int SIGN_POS    = 0;
  static const u_int MODULE_POS  = 1;
  static const u_int SIZE        = 2;
public:
  using Block::ToWord;
  // ModuleEntry Accessors
  word GetSign() {
    return GetArg(SIGN_POS);
  }
  word GetModule() {
    return GetArg(MODULE_POS);
  }
  // ModuleEntry Constructor
  static ModuleEntry *New(word sign, word module) {
    Block *p = Store::AllocBlock((BlockLabel) ENTRY_LABEL, SIZE);
    p->InitArg(SIGN_POS, sign);
    p->InitArg(MODULE_POS, module);
    return (ModuleEntry *) p;
  }
  // ModuleEntry Untagging
  static ModuleEntry *FromWord(word entry) {
    Block *p = Store::DirectWordToBlock(entry);
    Assert(p != INVALID_POINTER && p->GetLabel() == (BlockLabel) ENTRY_LABEL);
    return (ModuleEntry *) p;
  }
};

class BootLinker {
private:
  static word moduleTable;
  static u_int traceFlag;
  static char *aliceHome;
  static HashTable *GetModuleTable() {
    return HashTable::FromWordDirect(moduleTable);
  }
public:
  // BootLinker Functions
  static void Trace(const char *prefix, Chunk *key);
  static void EnterComponent(Chunk *key, word sign, word str) {
    GetModuleTable()->InsertItem(key->ToWord(),
				 ModuleEntry::New(sign, str)->ToWord());
  }
  static ModuleEntry *LookupComponent(Chunk *key) {
    HashTable *moduleTable = GetModuleTable();
    word keyWord = key->ToWord();
    if (moduleTable->IsMember(keyWord)) {
      return ModuleEntry::FromWord(moduleTable->GetItem(keyWord));
    } else {
      return INVALID_POINTER;
    }
  }
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
