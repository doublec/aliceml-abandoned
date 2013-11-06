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
#include <ostream>
#include <string>
#include <iomanip>
#include "alice/ByteCode.hh"
#include "alice/AbstractCodeInterpreter.hh"


using namespace ByteCodeInstr;


#ifdef THREADED
void **ByteCode::instrTable = NULL;
word ByteCode::threadedTable = INVALID_POINTER;
#endif


namespace {
  
  class ByteCodeDisassembler {
  private:
    std::ostream &out;
    Chunk *code;
    Tuple *immediateEnv;
    ProgramCounter curPC;
    u_int printedInstrs;
    
    
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
      out << " R" << r;
    }
   
   
    void PrintImmediate(u_int i) {
      out << " I" << i;
    }
   
   
    u_int PrintListStart(u_int len = 0) {
      if (len == 0) { // list length is dynamic
        GET_1I(GetCodeBuffer(), curPC, n);
	len = n;
        out << " " << len << "#[";
      }
      else {
        out << " [";
      }
      return len;
    }
    
    
    void PrintListEnd(){ 
      out << "]";
    }
   
   
    void PrintRegisterList(u_int len = 0) {
      len = PrintListStart(len);
      for (u_int i=0; i<len; i++) {
        if (i > 0) {
          out << " ";
        }
        GET_1R(GetCodeBuffer(), curPC, r);
        out << "R" << r;
      }
      PrintListEnd();
    }


    void PrintImmediateList(u_int len = 0) {
      len = PrintListStart(len);
      for (u_int i=0; i<len; i++) {
	if (i > 0) {
	  out << " ";
	}
	GET_1I(GetCodeBuffer(), curPC, im);
	out << "I" << im;
      }
      PrintListEnd();
    }


    void PrintAddr(u_int addr) {
      out << "0x" << std::hex << std::setfill('0') << std::setw(4)
        << addr << std::dec;
    }

    
    void PrintJumpList() {
      u_int len = PrintListStart();
      ProgramCounter fromPC = curPC;
      for (u_int i=0; i<len; i++) {
	if (i > 0) {
	  out << " ";
	}
        GET_1I(GetCodeBuffer(), curPC, offset);
	PrintAddr(static_cast<int>(((fromPC - GetStartPC()) + offset) * sizeof(word)));
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
	case ARGS_PRIMFUN: {
	  GET_2I(GetCodeBuffer(), curPC, interpreterAddr, cFunctionAddr);
	  out << " " << reinterpret_cast<Interpreter*>(interpreterAddr)->Identify();
	  break;
	}
	case ARGS_PRIMFUN_1R: {
	  PrintArgs(ARGS_PRIMFUN);
	  PrintArgs(ARGS_1R);
	  break;
	}
	case ARGS_PRIMFUN_2R: {
	  PrintArgs(ARGS_PRIMFUN);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_PRIMFUN_3R: {
	  PrintArgs(ARGS_PRIMFUN);
	  PrintArgs(ARGS_3R);
	  break;
	}
	case ARGS_PRIMFUN_DYNR: {
	  PrintArgs(ARGS_PRIMFUN);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_ALICEFUN: {
	  GET_1I(codeBuffer, curPC, closureAddr);
	  Closure *c = Closure::FromWordDirect(immediateEnv->Sel(closureAddr));
	  TagVal *abstractCode = AbstractCodeInterpreter::ConcreteToAbstractCode(c->GetConcreteCode());
	  String *name = AbstractCodeInterpreter::MakeProfileName(abstractCode);
	  PrintImmediate(closureAddr);
	  out << "[" << name << "]";
	  break;
	}
	case ARGS_ALICEFUN_1R: {
	  PrintArgs(ARGS_ALICEFUN);
	  PrintArgs(ARGS_1R);
	  break;
	}
	case ARGS_ALICEFUN_2R: {
	  PrintArgs(ARGS_ALICEFUN);
	  PrintArgs(ARGS_2R);
	  break;
	}
	case ARGS_ALICEFUN_3R: {
	  PrintArgs(ARGS_ALICEFUN);
	  PrintArgs(ARGS_3R);
	  break;
	}
	case ARGS_ALICEFUN_DYNR: {
	  PrintArgs(ARGS_ALICEFUN);
	  PrintArgs(ARGS_DYNR);
	  break;
	}
	case ARGS_DYNI: {
	  PrintImmediateList();
	  break;
	}
	case ARGS_JUMP: {
	  GET_1I(GetCodeBuffer(), curPC, offset);
          out << " ";
	  PrintAddr(static_cast<int>(((curPC - GetStartPC()) + offset) * sizeof(word)));
	  break;
	}
	default: {
	  Assert(false);
	}
      }
    }


  public:
    
    ByteCodeDisassembler(std::ostream &f, Chunk *code, Tuple *immediateEnv)
        : out(f), code(code), immediateEnv(immediateEnv), printedInstrs(0) {
      curPC = GetStartPC();
    }
    
    
    ByteCodeDisassembler(std::ostream &f, Chunk *code, Tuple *immediateEnv, ProgramCounter curPC)
      : out(f), code(code), immediateEnv(immediateEnv), curPC(curPC), printedInstrs(0) {}
    
    
    void PrintInstr() {
      PrintAddr(static_cast<int>((curPC - GetStartPC()) * sizeof(word)));
      out << ": ";
      u_int instr;
      GET_INSTR(GetCodeBuffer(), curPC, instr);

#ifdef THREADED
      instr = ByteCode::ThreadedToNumber(reinterpret_cast<void*>(instr));
#endif
      
      switch(instr) {
#define INSTR(name, args)			\
	case ByteCodeInstr::name: {		\
          out << #name;				\
          PrintArgs(args);			\
          out << "\n";				\
          break;				\
        }
#include "alice/ByteCodeInstrs.hh"
        default: {
	  Assert(false);
	}
      }
      printedInstrs++;
    }
    
    
    void PrintAllInstrs() {
      while (GetCurPC() < GetEndPC()) {
	PrintInstr();
      }
    }


    ProgramCounter GetCurPC() {
      return curPC;
    }


    u_int PrintedInstrs() {
      return printedInstrs;
    }

  };

}


const char *ByteCode::LookupName(ByteCodeInstr::instr ins) {
  switch(ins) {
#undef INSTR
#define INSTR(name, args)			\
    case ByteCodeInstr::name:			\
      return #name;
#include "alice/ByteCodeInstrs.hh"
    default: {
      Assert(false);
    }
  }
}


u_int ByteCode::NumInstrs(Chunk *code, Tuple *imEnv) {
  std::stringstream ss;
  ByteCodeDisassembler bd(ss, code, imEnv);
  bd.PrintAllInstrs();
  return bd.PrintedInstrs();
}


ProgramCounter ByteCode::DisassembleOne(std::FILE *f, ProgramCounter pc, Chunk *code, Tuple *imEnv) {
  std::stringstream ss;
  ByteCodeDisassembler bd(ss, code, imEnv, pc);
  bd.PrintInstr();
  std::fprintf(f, "%s", ss.str().c_str());
  return bd.GetCurPC();
}


void ByteCode::Disassemble(std::FILE *f, ProgramCounter pc, Chunk *code, Tuple *imEnv, u_int nRegisters) {
  std::stringstream ss;
  ss << "(* registers: " << nRegisters << ", code size: " << code->GetSize() << " bytes *)\n";
  ByteCodeDisassembler bd(ss, code, imEnv);
  bd.PrintAllInstrs();
  std::fprintf(f, "%s", ss.str().c_str());
}
