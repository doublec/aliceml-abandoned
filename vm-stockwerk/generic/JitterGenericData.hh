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
    static void New(u_int This,
		    FrameLabel label, u_int size,
		    Interpreter *interpreter) {
      JITStore::AllocBlock(This, (BlockLabel) label, BASE_SIZE + size);
      jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(interpreter));
      JITStore::InitArg(This, INTERPRETER_POS, JIT_R0);
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, BASE_SIZE + pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + pos, Value);
    }
    static void Replace(u_int This, u_int pos, u_int Value) {
      JITStore::ReplaceArg(This, BASE_SIZE + pos, Value);
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
      Sel(&::Scheduler::currentArgs[0], Dest);
    }
    static void GetOneArg(u_int Dest) {
      Sel(&::Scheduler::currentArgs[1], Dest);
    }
    static void PutZeroArg(u_int Value) {
      Put(&::Scheduler::currentArgs[0], Value);
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
    // Side-Effect: Scratches JIT_R0
    static void PopFrames(u_int n) {
      jit_ldi_ui(JIT_R0, &::Scheduler::nFrames);
      jit_subi_ui(JIT_R0, JIT_R0, n);
      jit_sti_ui(&::Scheduler::nFrames, JIT_R0);
    }
  };

  class Transform : public ::Transform {
  public:
    // Side-Effect: Scratches JIT_R0, JIT_FP
    static void New(u_int This) {
      JITStore::AllocBlock(This, TRANSFORM_LABEL, SIZE);
    }
    static void PutName(u_int This, u_int Value) {
      JITStore::InitArg(This, NAME_POS, Value);
    }
    static void PutArgument(u_int This, u_int Value) {
      JITStore::InitArg(This, ARGUMENT_POS, Value);
    }
  };

  class Tuple {
  public:
    // Side-Effect: Scratches JIT_R0, JIT_FP
    static void New(u_int This, u_int size) {
      JITStore::AllocBlock(This, TUPLE_LABEL, size);
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, pos, Value);
    }
    static void IndexSel(u_int Dest, u_int This, u_int Pos) {
      jit_addi_ui(Pos, Pos, 1);
      // to be done: compute log_2 sizeof(word)
      jit_lshi_ui(Pos, Pos, 2);
      jit_ldxr_p(Dest, This, Pos);
    }
  };

  class Closure : public ::Closure {
  public:
    // Side-Effect: Scratches JIT_R0, JIT_FP
    static void New(u_int This, u_int size) {
      JITStore::AllocBlock(This, CLOSURE_LABEL, BASE_SIZE + size);
    }
    static void InitConcreteCode(u_int This, u_int Value) {
      JITStore::InitArg(This, CONCRETE_CODE_POS, Value);
    }
    static void Sel(u_int This, u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, This, BASE_SIZE + pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + pos, Value);
    }
  };

  class ConcreteCode : public ::ConcreteRepresentation {
  public:
    // Side-Effect: Scratches JIT_R0, JIT_FP
    static void New(u_int This, Interpreter *interpreter, u_int size) {
      JITStore::AllocBlock(This, CONCRETE_LABEL, BASE_SIZE + size);
      jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(interpreter));
      JITStore::InitArg(This, HANDLER_POS, JIT_R0);
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, BASE_SIZE + pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + pos, Value);
    }
  };

  class Byneed : public ::Transient {
  public:
    // Side-Effect: Scratches JIT_R0, JIT_FP
    static void New(u_int This) {
      JITStore::AllocTransient(This, BYNEED_LABEL);
    }
    static void InitClosure(u_int This, u_int Closure) {
      JITStore::InitArg(This, REF_POS, Closure);
    }
  };

  class Primitive {
  public:
    // Side-Effect: Scratches JIT_R0
    static void Return1(u_int Value) {
      Scheduler::PutZeroArg(Value);
      jit_movi_ui(JIT_R0, ::Scheduler::ONE_ARG);
      Scheduler::PutNArgs(JIT_R0);
      jit_movi_ui(JIT_R0, Interpreter::CONTINUE);
      jit_ret();
    }
  };
};

#endif
