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


static void PrintRegister(std::FILE *f, u_int r) {
  std::fprintf(f, " R%"U_INTF, r);
}


static void PrintImmediate(std::FILE *f, u_int i) {
  std::fprintf(f, " I%"U_INTF, i);
}


static void PrintRegisterList(std::FILE *f, ReadBuffer *codeBuffer, ProgramCounter &PC, u_int len = 0) {
  if (len == 0) { // list length is dynamic
    GET_1I(codeBuffer, PC, n);
    len = n;
    std::fprintf(f, " %"U_INTF"#[", len);
  }
  else {
    std::fprintf(f, " [");
  }
  for (u_int i=0; i<len; i++) {
    if (i > 0) {
      std::fprintf(f, " ");
    }
    GET_1R(codeBuffer, PC, r);
    std::fprintf(f, "R%"U_INTF, r);
  }
  std::fprintf(f, "]");
}


static void PrintImmediateList(std::FILE *f, ReadBuffer *codeBuffer, ProgramCounter &PC, u_int len = 0) {
  if (len == 0) { // list length is dynamic
    GET_1I(codeBuffer, PC, n);
    len = n;
    std::fprintf(f, " %"U_INTF"#[", len);
  }
  else {
    std::fprintf(f, " [");
  }
  for (u_int i=0; i<len; i++) {
    if (i > 0) {
      std::fprintf(f, " ");
    }
    GET_1I(codeBuffer, PC, im);
    std::fprintf(f, "I%"U_INTF, im);
  }
  std::fprintf(f, "]");
}


static void PrintArgs(std::FILE *f, ReadBuffer *codeBuffer, ProgramCounter &PC, args args) {
  switch(args) {
    case ARGS_NONE: {
      break;
    }
    case ARGS_1R: {
      GET_1R(codeBuffer, PC, r);
      PrintRegister(f, r);
      break;
    }
    case ARGS_1R_1I: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      break;
    }
    case ARGS_1R_1I_DYNR: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNR);
      break;
    }
    case ARGS_1R_1I_DYNI: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNI);
      break;
    }
    case ARGS_1R_2I: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      break;
    }
    case ARGS_1R_1I_STATR1: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintRegisterList(f, codeBuffer, PC, 1);
      break;
    }
    case ARGS_1R_1I_STATR2: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintRegisterList(f, codeBuffer, PC, 2);
      break;
    }
    case ARGS_1R_1I_STATR3: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintRegisterList(f, codeBuffer, PC, 3);
      break;
    }
    case ARGS_1R_1I_STATR4: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R_1I);
      PrintRegisterList(f, codeBuffer, PC, 4);
      break;
    }
    case ARGS_1R_DYNR: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNR);
      break;
    }
    case ARGS_1R_DYNI: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNI);
      break;
    }
    case ARGS_2R: {
      GET_2R(codeBuffer, PC, r0, r1);
      PrintRegister(f, r0);
      PrintRegister(f, r1);
      break;
    }
    case ARGS_2R_1I: {
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      break;
    }
    case ARGS_2R_2I: {
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      break;
    }
    case ARGS_3R: {
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      break;
    }
    case ARGS_3R_1I: {
      PrintArgs(f, codeBuffer, PC, ARGS_3R);
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      break;
    }
    case ARGS_4R: {
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      break;
    }
    case ARGS_DYNR: {
      PrintRegisterList(f, codeBuffer, PC);
      break;
    }
    case ARGS_1I: {
      GET_1I(codeBuffer, PC, i);
      PrintImmediate(f, i);
      break;
    }
    case ARGS_1I_1R: {
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      break;
    }
    case ARGS_1I_2R: {
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      break;
    }
    case ARGS_1I_3R: {
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_3R);
      break;
    }
    case ARGS_1I_DYNR: {
      PrintArgs(f, codeBuffer, PC, ARGS_1I);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNR);
      break;
    }
    case ARGS_2I: {
      GET_2I(codeBuffer, PC, i0, i1);
      PrintImmediate(f, i0);
      PrintImmediate(f, i1);
      break;
    }
    case ARGS_2I_1R: {
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      PrintArgs(f, codeBuffer, PC, ARGS_1R);
      break;
    }
    case ARGS_2I_2R: {
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      PrintArgs(f, codeBuffer, PC, ARGS_2R);
      break;
    }
    case ARGS_2I_3R: {
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      PrintArgs(f, codeBuffer, PC, ARGS_3R);
      break;
    }
    case ARGS_2I_DYNR: {
      PrintArgs(f, codeBuffer, PC, ARGS_2I);
      PrintArgs(f, codeBuffer, PC, ARGS_DYNR);
      break;
    }
    case ARGS_DYNI: {
      PrintImmediateList(f, codeBuffer, PC);
      break;
    }
    default: {
      Assert(false);
    }
  }
}


ProgramCounter ByteCode::DisassembleOne(std::FILE *f, ProgramCounter startPC, ProgramCounter curPC, 
					Chunk *code, Tuple *imEnv) {
  ReadBuffer *codeBuffer = ReadBuffer::New(code);

  std::fprintf(f, "0x%04x: ", static_cast<int>((curPC - startPC) * sizeof(word)));
  u_int instr;
  GET_INSTR(codeBuffer, curPC, instr);

#ifdef THREADED			
  instr = ThreadedToNumber(reinterpret_cast<void*>(instr));
#endif

  switch(instr) {
#define INSTR(name, args)		\
  case name: {				\
    std::fprintf(f, #name);		\
    PrintArgs(f, codeBuffer, curPC, args);	\
    std::fprintf(f, "\n");		\
    return curPC;			\
  }
#include "alice/ByteCodeInstrs.hh"
    default: {
      Assert(false);
    }
  }
}


void ByteCode::Disassemble(std::FILE *f, ProgramCounter pc, 
			   Chunk *code, Tuple *imEnv) {
#ifdef THREADED
  ProgramCounter start = reinterpret_cast<ProgramCounter>(code->GetBase());
  ProgramCounter end = reinterpret_cast<ProgramCounter>(code->GetBase() + code->GetSize());
  while(pc < end) {
#else
  ProgramCounter start = 0;
  u_int max = code->GetSize();
  while(4 * pc < max) {
#endif
    pc = ByteCode::DisassembleOne(f, start, pc, code, imEnv);
  }
}
