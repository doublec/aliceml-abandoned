//
// Author:
//   Christian Mueller <cmueller@ps.uni-sb.de>
//
// Copyright:
//   Christian Mueller, 2005
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//

#ifndef __ALICE_BYTE_CODE_HH__
#define __ALICE_BYTE_CODE_HH__

#if defined(INTERFACE)
#pragma interface "alice/ByteCode.hh"
#endif

#include "alice/Authoring.hh"
#include "alice/ByteCodeAlign32.hh"

#define INSTR(instr) instr,

namespace ByteCodeInstr {
  enum instr {
#include "alice/ByteCodeInstrs.hh"
    NUMBER_OF_INSTRS
  };
};

class AliceDll ByteCode {
#ifdef THREADED
private:
  static void **instrTable;
  static word threadedTable;
#endif
 
public:

#ifdef THREADED
  static void RegisterInstrTable(void **table) {
    instrTable = table;
  }
  static void* LookupInstr(u_int ins) {
    // instrTable must be registered first
    return instrTable[ins];
  }
  static u_int ThreadedToNumber(void *instr) {
    // Initialize threaded table if it is unkown.
    // Do not create it statically, because Disassemble is seldomly used
    // in real applications. 
    if(threadedTable == INVALID_POINTER) {
      IntMap *map = IntMap::New(2 * ByteCodeInstr::NUMBER_OF_INSTRS);
      for(u_int i=0; i<ByteCodeInstr::NUMBER_OF_INSTRS; i++) {
	word wInstr = Store::UnmanagedPointerToWord(instrTable[i]);
	map->Put(wInstr,Store::IntToWord(i));
      }
      threadedTable = map->ToWord();
      RootSet::Add(threadedTable); // it should survive GC
    }
    IntMap *map = IntMap::FromWordDirect(threadedTable);
    word wInstr = Store::UnmanagedPointerToWord(instr);
    if(map->IsMember(wInstr))
      return Store::DirectWordToInt(map->Get(wInstr));
    else
      return 7711; // unkown
  }
#endif // THREADED

  static void Disassemble(std::FILE *f, u_int pc, Chunk *code, Tuple *imEnv); 
  static int DisassembleOne(std::FILE *f, u_int pc, Chunk *code, Tuple *imEnv);
};

#undef INSTR

#endif // __ALICE_BYTE_CODE_HH__
