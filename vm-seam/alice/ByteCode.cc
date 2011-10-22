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

#if defined(INTERFACE)
#pragma implementation "alice/ByteCode.hh"
#endif

#include <cstdio>
#include "alice/ByteCode.hh"
using namespace ByteCodeInstr;

#ifdef THREADED
void **ByteCode::instrTable = NULL;
word ByteCode::threadedTable = INVALID_POINTER;
#endif // THREADED


namespace {
  
  class ByteCodeDisassembler {
  private:
    std::FILE *file;
    Chunk *code;
    ProgramCounter curPC;
    
    
    ProgramCounter GetStartPC() {
#ifdef THREADED
      return reinterpret_cast<ProgramCounter>(code->GetBase());
#else
      return 0;
#endif
    }
    
    
    ProgramCounter GetEndPC() {
#ifdef THREADED
      return reinterpret_cast<ProgramCounter>(code->GetBase() + code->GetSize());
#else
      return code->GetSize() / sizeof(word);
#endif
    }
    
    
    ReadBuffer *GetCodeBuffer() {
      return ReadBuffer::New(code);
    }
    
    
    void PrintRegister(u_int r) {
      std::fprintf(file, " R%"U_INTF, r);
    }
   
   
    void PrintImmediate(u_int i) {
      std::fprintf(file, " I%"U_INTF, i);
    }
   
   
    u_int PrintListStart(u_int len = 0) {
      if (len == 0) { // list length is dynamic
        GET_1I(GetCodeBuffer(), curPC, n);
	len = n;
        std::fprintf(file, " %"U_INTF"#[", len);
      }
      else {
        std::fprintf(file, " [");
      }
      return len;
    }
    
    
    void PrintListEnd(){ 
      std::fprintf(file, "]");
    }
   
   
    void PrintRegisterList(u_int len = 0) {
      len = PrintListStart(len);
      for (u_int i=0; i<len; i++) {
        if (i > 0) {
          std::fprintf(file, " ");
        }
        GET_1R(GetCodeBuffer(), curPC, r);
        std::fprintf(file, "R%"U_INTF, r);
      }
      PrintListEnd();
    }


    void PrintImmediateList(u_int len = 0) {
      len = PrintListStart(len);
      for (u_int i=0; i<len; i++) {
	if (i > 0) {
	  std::fprintf(file, " ");
	}
	GET_1I(GetCodeBuffer(), curPC, im);
	std::fprintf(file, "I%"U_INTF, im);
      }
      PrintListEnd();
    }

    
    void PrintJumpList() {
      u_int len = PrintListStart();
      ProgramCounter fromPC = curPC;
      for (u_int i=0; i<len; i++) {
	if (i > 0) {
	  std::fprintf(file, " ");
	}
        GET_1I(GetCodeBuffer(), curPC, offset);
        std::fprintf(file, "0x%04x", static_cast<int>(((fromPC - GetStartPC()) + offset) * sizeof(word)));
      }
      PrintListEnd();
    }


    void PrintArgs(args args) {
      switch(args) {
	case ARGS_NONE: {
	  break;
	}
	case ARGS_1R: {
	  GET_1R(GetCodeBuffer(), curPC, r);
	  PrintRegister(r);
	  break;
	}
	case ARGS_1R_1I: {
	  PrintArgs(ARGS_1R);
	  PrintArgs(ARGS_1I);
	  break;
	}
	case ARGS_1R_1I_DYNR: {
	  PrintArgs(ARGS_1R_1I);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_1R_1I_DYNI: {
	  PrintArgs(ARGS_1R_1I);
	  PrintArgs(ARGS_DYNI);
	  break;
	}
	case ARGS_1R_2I: {
	  PrintArgs(ARGS_1R);
	  PrintArgs(ARGS_2I);
	  break;
	}
	case ARGS_1R_1I_STATR1: {
	  PrintArgs(ARGS_1R_1I);
	  PrintRegisterList(1);
	  break;
	}
	case ARGS_1R_1I_STATR2: {
	  PrintArgs(ARGS_1R_1I);
	  PrintRegisterList(2);
	  break;
	}
	case ARGS_1R_1I_STATR3: {
	  PrintArgs(ARGS_1R_1I);
	  PrintRegisterList(3);
	  break;
	}
	case ARGS_1R_1I_STATR4: {
	  PrintArgs(ARGS_1R_1I);
	  PrintRegisterList(4);
	  break;
	}
	case ARGS_1R_1I_JUMP: {
	  PrintArgs(ARGS_1R_1I);
	  PrintArgs(ARGS_JUMP);
	  break;
	}
	case ARGS_1R_1I_DYNJUMP: {
	  PrintArgs(ARGS_1R_1I);
	  PrintJumpList();
	  break;
	}
	case ARGS_1R_DYNR: {
	  PrintArgs(ARGS_1R);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_1R_DYNI: {
	  PrintArgs(ARGS_1R);
	  PrintArgs(ARGS_DYNI);
	  break;
	}
	case ARGS_1R_DYNJUMP: {
	  PrintArgs(ARGS_1R);
	  PrintJumpList();
	  break;
	}
	case ARGS_2R: {
	  GET_2R(GetCodeBuffer(), curPC, r0, r1);
	  PrintRegister(r0);
	  PrintRegister(r1);
	  break;
	}
	case ARGS_2R_1I: {
	  PrintArgs(ARGS_2R);
	  PrintArgs(ARGS_1I);
	  break;
	}
	case ARGS_2R_2I: {
	  PrintArgs(ARGS_2R);
	  PrintArgs(ARGS_2I);
	  break;
	}
	case ARGS_2R_JUMP: {
	  PrintArgs(ARGS_2R);
	  PrintArgs(ARGS_JUMP);
	  break;
	}
	case ARGS_3R: {
	  PrintArgs(ARGS_1R);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_3R_1I: {
	  PrintArgs(ARGS_3R);
	  PrintArgs(ARGS_1I);
	  break;
	}
	case ARGS_4R: {
	  PrintArgs(ARGS_2R);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_DYNR: {
	  PrintRegisterList();
	  break;
	}
	case ARGS_1I: {
	  GET_1I(GetCodeBuffer(), curPC, i);
	  PrintImmediate(i);
	  break;
	}
	case ARGS_1I_1R: {
	  PrintArgs(ARGS_1I);
	  PrintArgs(ARGS_1R);
	  break;
	}
	case ARGS_1I_2R: {
	  PrintArgs(ARGS_1I);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_1I_3R: {
	  PrintArgs(ARGS_1I);
	  PrintArgs(ARGS_3R);
	  break;
	}
	case ARGS_1I_DYNR: {
	  PrintArgs(ARGS_1I);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_2I: {
	  GET_2I(GetCodeBuffer(), curPC, i0, i1);
	  PrintImmediate(i0);
	  PrintImmediate(i1);
	  break;
	}
	case ARGS_2I_1R: {
	  PrintArgs(ARGS_2I);
	  PrintArgs(ARGS_1R);
	  break;
	}
	case ARGS_2I_2R: {
	  PrintArgs(ARGS_2I);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_2I_3R: {
	  PrintArgs(ARGS_2I);
	  PrintArgs(ARGS_3R);
	  break;
	}
	case ARGS_2I_DYNR: {
	  PrintArgs(ARGS_2I);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_DYNI: {
	  PrintImmediateList();
	  break;
	}
	case ARGS_JUMP: {
	  GET_1I(GetCodeBuffer(), curPC, offset);
	  std::fprintf(file, " 0x%04x", static_cast<int>(((curPC - GetStartPC()) + offset) * sizeof(word)));
	  break;
	}
	default: {
	  Assert(false);
	}
      }
    }


  public:
    
    ByteCodeDisassembler(std::FILE *f, Chunk *code)
      : file(f), code(code), curPC(GetStartPC()) {}
    
    
    ByteCodeDisassembler(std::FILE *f, Chunk *code, ProgramCounter curPC)
      : file(f), code(code), curPC(curPC) {}
    
    
    void PrintInstr() {
      std::fprintf(file, "0x%04x: ", static_cast<int>((curPC - GetStartPC()) * sizeof(word)));
      u_int instr;
      GET_INSTR(GetCodeBuffer(), curPC, instr);

#ifdef THREADED
      instr = ByteCode::ThreadedToNumber(reinterpret_cast<void*>(instr));
#endif
      
      switch(instr) {
#define INSTR(name, args)			\
	case ByteCodeInstr::name: {		\
          std::fprintf(file, #name);		\
          PrintArgs(args);			\
          std::fprintf(file, "\n");		\
          break;				\
        }
 #include "alice/ByteCodeInstrs.hh"
        default: {
	  Assert(false);
	}
      }
    }
    
    
    void PrintAllInstrs() {
      while (GetCurPC() < GetEndPC()) {
	PrintInstr();
      }
    }


    ProgramCounter GetCurPC() {
      return curPC;
    }

  };

}


ProgramCounter ByteCode::DisassembleOne(std::FILE *f, ProgramCounter pc, Chunk *code, Tuple *imEnv) {
  ByteCodeDisassembler bd(f, code, pc);
  bd.PrintInstr();
  return bd.GetCurPC();
}


void ByteCode::Disassemble(std::FILE *f, ProgramCounter pc, Chunk *code, Tuple *imEnv, u_int nRegisters) {
  fprintf(f, "(* registers: %"U_INTF", code size: %"U_INTF" bytes *)\n", nRegisters, code->GetSize());
  ByteCodeDisassembler bd(f, code);
  bd.PrintAllInstrs();
}
