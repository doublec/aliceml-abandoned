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

#ifndef __GENERIC__JITTER_GENERIC_DATA_HH__
#define __GENERIC__JITTER_GENERIC_DATA_HH__

#if defined(INTERFACE)
#pragma interface "generic/JitterGenericData.hh"
#endif

#include "store/JITStore.hh"
#include "generic/Scheduler.hh"
#include "alice/JitterImmediateEnv.hh"

namespace Generic {
  class StackFrame : public ::StackFrame {
  public:
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + pos, Value);
    }
    static void Replace(u_int Ptr, u_int pos, u_int Value) {
      JITStore::ReplaceArg(Ptr, BASE_SIZE + pos, Value);
    }
  };

  class TaskStack : public ::Stack {
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
    static void GetCurrentArgs(u_int Ptr) {
      jit_movi_p(Ptr, &::Scheduler::currentArgs);
    }
    static void SelArg(u_int Dest, u_int Ptr, u_int pos) {
      jit_ldxi_p(Dest, Ptr, pos * sizeof(word));
    }
    static void PutArg(u_int Ptr, u_int pos, u_int Value) {
      JITStore::LogSetArg(pos, Value);
      jit_stxi_p(pos * sizeof(word), Ptr, Value);
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

  class Transform : public ::Transform {
  public:
    static void New(u_int Ptr) {
      JITStore::AllocBlock(Ptr, TRANSFORM_LABEL, SIZE);
    }
    static void PutName(u_int Ptr, u_int Value) {
      JITStore::InitArg(Ptr, NAME_POS, Value);
    }
    static void PutArgument(u_int Ptr, u_int Value) {
      JITStore::InitArg(Ptr, ARGUMENT_POS, Value);
    }
  };

  class Tuple {
  public:
    static void New(u_int Ptr, u_int size) {
      JITStore::AllocBlock(Ptr, TUPLE_LABEL, size);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, pos, Value);
    }
    static void IndexSel(u_int Dest, u_int Ptr, u_int Pos) {
      jit_addi_ui(Pos, Pos, 1);
      // to be done: compute log_2 sizeof(word)
      jit_lshi_ui(Pos, Pos, 2);
      jit_ldxr_p(Dest, Ptr, Pos);
    }
  };

  class Closure : public ::Closure {
  public:
    static void New(u_int Ptr, u_int size) {
      JITStore::AllocBlock(Ptr, CLOSURE_LABEL, BASE_SIZE + size);
    }
    static void InitCC(u_int Ptr, u_int Value) {
      JITStore::InitArg(Ptr, CONCRETE_CODE_POS, Value);
    }
    static void Sel(u_int Ptr, u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + pos, Value);
    }
  };

  class ConcreteCode : public ::ConcreteRepresentation {
  public:
    static void New(u_int Ptr, Interpreter *interpreter, u_int size) {
      JITStore::AllocBlock(Ptr, CONCRETE_LABEL, BASE_SIZE + size);
      jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(interpreter));
      JITStore::InitArg(Ptr, HANDLER_POS, JIT_R0);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + pos, Value);
    }
  };

  class Byneed : public ::Transient {
  public:
    static void New(u_int Ptr) {
      JITStore::AllocTransient(Ptr, BYNEED_LABEL);
    }
    static void InitClosure(u_int Ptr, u_int Closure) {
      JITStore::InitArg(Ptr, REF_POS, Closure);
    }
  };

  class Primitive {
  public:
    static void Return1(u_int Value) {
      Assert(Value != JIT_R0);
      Scheduler::PutZeroArg(Value);
      jit_movi_ui(JIT_R0, ::Scheduler::ONE_ARG);
      Scheduler::PutNArgs(JIT_R0);
    }
  };
};

#endif
