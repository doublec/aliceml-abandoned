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
#include "alice/ByteCodeAlign.hh"

#define INSTR(instr, args) instr,

namespace ByteCodeInstr {

  enum args {
    ARGS_NONE,
    ARGS_1R,
    ARGS_1R_1I,
    ARGS_1R_1I_STATR1,
    ARGS_1R_1I_STATR2,
    ARGS_1R_1I_STATR3,
    ARGS_1R_1I_STATR4,
    ARGS_1R_1I_DYNR,
    ARGS_1R_1I_DYNI,
    ARGS_1R_2I,
    ARGS_1R_DYNR,
    ARGS_1R_DYNI,
    ARGS_2R,
    ARGS_2R_1I,
    ARGS_2R_2I,
    ARGS_3R,
    ARGS_3R_1I,
    ARGS_4R,
    ARGS_DYNR,
    ARGS_1I,
    ARGS_1I_1R,
    ARGS_1I_2R,
    ARGS_1I_3R,
    ARGS_1I_DYNR,
    ARGS_2I,
    ARGS_2I_1R,
    ARGS_2I_2R,
    ARGS_2I_3R,
    ARGS_2I_DYNR,
    ARGS_DYNI,
  };

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
  
  /**
   * Zero the least significant bit of the pointer so that it
   * can be encoded with Store::UnmanagedPointerToWord
   */
  static void *Align(void *p) {
    return reinterpret_cast<void*>(reinterpret_cast<u_int>(p) & ~static_cast<u_int>(1));
  }
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
	word wInstr = Store::UnmanagedPointerToWord(Align(instrTable[i]));
	map->Put(wInstr,Store::IntToWord(i));
      }
      threadedTable = map->ToWord();
      RootSet::Add(threadedTable); // it should survive GC
    }
    IntMap *map = IntMap::FromWordDirect(threadedTable);
    word wInstr = Store::UnmanagedPointerToWord(Align(instr));
    if(map->IsMember(wInstr))
      return Store::DirectWordToInt(map->Get(wInstr));
    else
      return reinterpret_cast<u_int>(instr); // unkown
  }
#endif // THREADED

  static void Disassemble(std::FILE *f, ProgramCounter pc, Chunk *code, Tuple *imEnv); 
  static ProgramCounter DisassembleOne(std::FILE *f, ProgramCounter startPC,
                                      ProgramCounter pc, Chunk *code, Tuple *imEnv);
};

#undef INSTR

#endif // __ALICE_BYTE_CODE_HH__
