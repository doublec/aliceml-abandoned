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
  char *name;
  word (*module)(void);
} prim_table;

class BootLinker {
private:
  static word moduleTable;
  static u_int traceFlag;
  static char *aliceHome;
public:
  // BootLinker Functions
  static void Trace(const char *prefix, Chunk *key);
  static HashTable *GetModuleTable() {
    return HashTable::FromWordDirect(moduleTable);
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
