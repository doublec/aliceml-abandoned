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

#ifndef __ALICE__JITTER_GENERIC_DATA_HH__
#define __ALICE__JITTER_GENERIC_DATA_HH__

#if defined(INTERFACE)
#pragma interface "alice/JitterGenericData.hh"
#endif

#include "store/JITStore.hh"
#include "generic/Scheduler.hh"
#include "alice/JitterImmediateEnv.hh"

namespace Generic {
  class StackFrame {
  protected:
    static const u_int INTERPRETER_POS = 0;
    static const u_int BASE_SIZE       = 1;
  public:
    static void Sel(u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, JIT_V2, BASE_SIZE + pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V2, BASE_SIZE + pos, Value);
    }
  };

  class TaskStack {
  protected:
    static const u_int TOP_POS = 0;
    static const u_int ARR_POS = 1;
    static const u_int SIZE    = 2;
  public:
    static void PopFrames(u_int Stack, u_int nbFrames) {
      JITStore::GetArg(JIT_R0, Stack, TOP_POS);
      // nbFrames << 1 = Store::IntToWord(nbFrames) - 1
      jit_subi_p(JIT_R0, JIT_R0, nbFrames << 1);
      JITStore::InitArg(Stack, TOP_POS, JIT_R0);
    }
  };

  class Scheduler {
  protected:
    static void Sel(void *addr, u_int Dest) {
      jit_ldi_p(Dest, addr);
    }
    static void Put(void *addr, u_int Value) {
      jit_sti_p(addr, Value);
    }
  public:
    static void GetNArgs(u_int Dest) {
      Sel(&::Scheduler::nArgs, Dest);
    }
    static void PutNArgs(u_int Value) {
      Put(&::Scheduler::nArgs, Value);
    }
    static void GetCurrentArgs() {
      jit_movi_p(JIT_V1, &::Scheduler::currentArgs);
    }
    static void SelArg(u_int Dest, u_int pos) {
      jit_ldxi_p(Dest, JIT_V1, pos * sizeof(word));
    }
    static void PutArg(u_int pos, u_int Value) {
      jit_stxi_p(pos * sizeof(word), JIT_V1, Value);
    }
    static void GetZeroArg(u_int Dest) {
      Sel(&::Scheduler::currentArgs, Dest);
    }
    static void GetOneArg(u_int Dest) {
      Sel(((char *) &::Scheduler::currentArgs + sizeof(word)), Dest);
    }
    static void PutZeroArg(u_int Value) {
      Put(&::Scheduler::currentArgs, Value);
    }
    static void GetCurrentData(u_int Dest) {
      Sel(&::Scheduler::currentData, Dest);
    }
    static void SetCurrentData(u_int Value) {
      Put(&::Scheduler::currentData, Value);
    }
    static void GetCurrentBacktrace(u_int Dest) {
      Sel(&::Scheduler::currentBacktrace, Dest);
    }
    static void SetCurrentBacktrace(u_int Value) {
      Put(&::Scheduler::currentBacktrace, Value);
    }
  };

  class Transform {
  protected:
    static const u_int NAME_POS     = 0;
    static const u_int ARGUMENT_POS = 1;
    static const u_int SIZE         = 2;
  public:
    static void New() {
      JITStore::AllocBlock(TRANSFORM_LABEL, SIZE);
    }
    static void PutName(u_int Value) {
      JITStore::InitArg(JIT_V1, NAME_POS, Value);
    }
    static void PutArgument(u_int Value) {
      JITStore::InitArg(JIT_V1, ARGUMENT_POS, Value);
    }
  };

  class Tuple {
  public:
    static void New(u_int size) {
      JITStore::AllocBlock(TUPLE_LABEL, size);
    }
    static void Sel(u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, JIT_V1, pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V1, pos, Value);
    }
    static void IndexSel(u_int Dest, u_int Pos) {
      jit_addi_ui(Pos, Pos, 1);
      // to be done: compute log_2 sizeof(word)
      jit_lshi_ui(Pos, Pos, 2);
      jit_ldxr_p(Dest, JIT_V1, Pos);
    }
  };

  class Closure {
  protected:
    static const u_int CONCRETE_CODE_POS = 0;
    static const u_int BASE_SIZE         = 1;
  public:
    static void New(u_int ConcreteCode, u_int size) {
      JITStore::AllocBlock(CLOSURE_LABEL, BASE_SIZE + size);
      JITStore::InitArg(JIT_V1, CONCRETE_CODE_POS, ConcreteCode);
    }
    static void New(u_int size) {
      JITStore::AllocBlock(CLOSURE_LABEL, BASE_SIZE + size);
    }
    static void InitCC(u_int Value) {
      JITStore::InitArg(JIT_V1, CONCRETE_CODE_POS, Value);
    }
    static void Sel(u_int Ptr, u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V1, BASE_SIZE + pos, Value);
    }
  };

  class ConcreteCode {
  protected:
    static const u_int HANDLER_POS = 0;
    static const u_int BASE_SIZE   = 1;
  public:
    static void New(Interpreter *interpreter, u_int size) {
      JITStore::AllocBlock(CONCRETE_LABEL, BASE_SIZE + size);
      jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(interpreter));
      JITStore::InitArg(JIT_V1, HANDLER_POS, JIT_R0);
    }
    static void Sel(u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, JIT_V1, BASE_SIZE + pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V1, BASE_SIZE + pos, Value);
    }
  };
};

#endif
