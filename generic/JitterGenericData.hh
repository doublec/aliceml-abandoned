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

#if HAVE_LIGHTNING

#if defined(INTERFACE)
#pragma interface "generic/JitterGenericData.hh"
#endif

#include "store/JITStore.hh"
#include "generic/Scheduler.hh"

class JITGeneric : public JITStore {
protected:
  void Scheduler_Sel(void *addr, u_int Dest) {
    jit_ldi_p(Dest, addr);
  }
  void Scheduler_Put(void *addr, u_int Value) {
    jit_sti_p(addr, Value);
  }
  void StackFrame_InitArg(u_int This, u_int index, u_int Value) {
    s_int offset = (0 - (s_int) index) * sizeof(word);
    jit_stxi_p(offset, This, Value);
  }
  void StackFrame_GetArg(u_int Dest, u_int This, u_int index) {
    s_int offset = (0 - (s_int) index) * sizeof(word);
    jit_ldxi_p(Dest, This, offset);
  }

  enum {STACKFRAME_WORKER_POS, STACKFRAME_BASE_SIZE};
  enum { TRANSFORM_NAME_POS, TRANSFORM_ARGUMENT_POS, TRANSFORM_SIZE };
  enum { CLOSURE_CONCRETE_CODE_POS, CLOSURE_BASE_SIZE };
  enum { CONCRETEREPRESENTATION_HANDLER_POS, CONCRETEREPRESENTATION_BASE_SIZE };
  enum { TRANSIENT_REF_POS };
public:
  void Scheduler_GetNArgs(u_int Dest) {
    Scheduler_Sel(&Scheduler::nArgs, Dest);
  }
  void Scheduler_PutNArgs(u_int Value) {
    Scheduler_Put(&Scheduler::nArgs, Value);
  }
  void Scheduler_GetCurrentArgs(u_int Ptr) {
    jit_movi_p(Ptr, &Scheduler::currentArgs);
  }
  void Scheduler_SelArg(u_int Dest, u_int Ptr, u_int pos) {
    jit_ldxi_p(Dest, Ptr, pos * sizeof(word));
  }
  void Scheduler_PutArg(u_int Ptr, u_int pos, u_int Value) {
#if defined(JIT_STORE_DEBUG)
    JITStore::LogSetArg(pos, Value);
#endif
    jit_stxi_p(pos * sizeof(word), Ptr, Value);
  }
  void *Scheduler_GetZeroArg() {
    return (void *) &Scheduler::currentArgs[0];
  }
  void Scheduler_GetZeroArg(u_int Dest) {
    Scheduler_Sel(&Scheduler::currentArgs[0], Dest);
  }
  void *Scheduler_GetOneArg() {
    return (void *) &Scheduler::currentArgs[1];
  }
  void Scheduler_GetOneArg(u_int Dest) {
    Scheduler_Sel(&Scheduler::currentArgs[1], Dest);
  }
  void Scheduler_PutZeroArg(u_int Value) {
    Scheduler_Put(&Scheduler::currentArgs[0], Value);
  }
  void Scheduler_GetCurrentData(u_int Dest) {
    Scheduler_Sel(&Scheduler::currentData, Dest);
  }
  void Scheduler_SetCurrentData(u_int Value) {
    Scheduler_Put(&Scheduler::currentData, Value);
  }
  void Scheduler_GetCurrentBacktrace(u_int Dest) {
    Scheduler_Sel(&Scheduler::currentBacktrace, Dest);
  }
  void Scheduler_SetCurrentBacktrace(u_int Value) {
    Scheduler_Put(&Scheduler::currentBacktrace, Value);
  }
  // Side Effect: Scratches JIT_R0
  void Scheduler_PushFrame(u_int Dest, u_int size) {
#if defined(JIT_ASSERT_INDEX)
    JITStore::SaveAllRegs();
    JITStore::Prepare(1);
    jit_movi_ui(JIT_R0, size);
    JITStore::PushArg(JIT_R0);
    JITStore::Finish((void *) Scheduler::PushFrame);
    jit_sti_p(&JITStore::loadedWord, JIT_R0);
    JITStore::RestoreAllRegs();
    jit_ldi_p(Dest, &JITStore::loadedWord);
#else
    Assert(Dest != JIT_R0);
    jit_insn *loop = jit_get_label();
    jit_ldi_p(Dest, &Scheduler::stackTop);
    jit_addi_p(Dest, Dest, size * sizeof(word));
    jit_ldi_p(JIT_R0, &Scheduler::stackMax);
    jit_insn *succeeded = jit_bltr_p(jit_forward(), Dest, JIT_R0);
    JITStore::Prepare(0);
    JITStore::Finish((void *) Scheduler::EnlargeTaskStack);
    (void) jit_jmpi(loop);
    jit_patch(succeeded);
    jit_sti_p(&Scheduler::stackTop, Dest);
#endif
  }
  void Scheduler_PushFrameNoCheck(u_int Dest, u_int size) {
#if defined(JIT_ASSERT_INDEX)
    JITStore::SaveAllRegs();
    JITStore::Prepare(1);
    jit_movi_ui(JIT_R0, size);
    JITStore::PushArg(JIT_R0);
    JITStore::Finish((void *) Scheduler::PushFrame);
    jit_sti_p(&JITStore::loadedWord, JIT_R0);
    JITStore::RestoreAllRegs();
    jit_ldi_p(Dest, &JITStore::loadedWord);
#else
    jit_ldi_p(Dest, &Scheduler::stackTop);
    jit_addi_p(Dest, Dest, size * sizeof(word));
    jit_sti_p(&Scheduler::stackTop, Dest);
#endif
  }
  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Scheduler_GetFrame(u_int This) {
#if defined(JIT_ASSERT_INDEX)
    JITStore::SaveAllRegs();
    JITStore::Prepare(0);
    JITStore::Finish((void *) Scheduler::GetFrame);
    jit_sti_p(&JITStore::loadedWord, JIT_R0);
    JITStore::RestoreAllRegs();
    jit_ldi_p(This, &JITStore::loadedWord);
#else
    jit_ldi_p(This, &Scheduler::stackTop);
#endif
  }
  void Scheduler_PopFrame(u_int size) {
    jit_ldi_p(JIT_R0, &Scheduler::stackTop);
    jit_subi_p(JIT_R0, JIT_R0, size * sizeof(word));
    jit_sti_p(&Scheduler::stackTop, JIT_R0);
  }
  void Scheduler_PopFrameReg(u_int Reg) {
    jit_ldi_p(JIT_R0, &Scheduler::stackTop);
    jit_subr_p(JIT_R0, JIT_R0, Reg);
    jit_sti_p(&Scheduler::stackTop, JIT_R0);
  }
  void Scheduler_PopAndPushFrame(u_int Dest, u_int oldSize, u_int newSize) {
    if (oldSize == newSize) {
      jit_ldi_p(Dest, &Scheduler::stackTop);
    } else if (oldSize > newSize) {
      jit_ldi_p(Dest, &Scheduler::stackTop);
      jit_subi_p(Dest, Dest, ((oldSize - newSize) * sizeof(word)));
      jit_sti_p(&Scheduler::stackTop, Dest);
    } else {
      jit_ldi_p(Dest, &Scheduler::stackTop);
      jit_addi_p(Dest, Dest, ((newSize - oldSize) * sizeof(word)));
      jit_sti_p(&Scheduler::stackTop, Dest);
    }
  }

  // Side Effect: Scratches JIT_R0, JIT_FP
  void StackFrame_New(u_int This, u_int size, Worker *worker) {
    u_int frSize = STACKFRAME_BASE_SIZE + size;
    Scheduler_PushFrame(This, frSize);
    (void) jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(worker));
    StackFrame_InitArg(This, STACKFRAME_WORKER_POS, JIT_R0);
  }
  // Side Effect: Scratches JIT_R0
  void StackFrame_NewNoCheck(u_int This, u_int size, Worker *worker) {
    u_int frSize = STACKFRAME_BASE_SIZE + size;
    Scheduler_PushFrameNoCheck(This, frSize);
    (void) jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(worker));
    StackFrame_InitArg(This, STACKFRAME_WORKER_POS, JIT_R0);
  }
  // Side Effect: Scratches JIT_R0
  void StackFrame_NewPopAndPush(u_int This,
				u_int newSize, u_int oldSize, Worker *worker) {
    Scheduler_PopAndPushFrame(This,
			      STACKFRAME_BASE_SIZE + oldSize,
			      STACKFRAME_BASE_SIZE + newSize);
    (void) jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(worker));
    StackFrame_InitArg(This, STACKFRAME_WORKER_POS, JIT_R0);
  }
  void StackFrame_GetWorkerW(u_int Dest, u_int This) {
    StackFrame_GetArg(Dest, This, STACKFRAME_WORKER_POS);
  }
  void StackFrame_PutWorker(u_int Dest, Worker *worker) {
    (void) jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(worker));
    StackFrame_InitArg(Dest, STACKFRAME_WORKER_POS, JIT_R0);
  }
  void StackFrame_Sel(u_int Dest, u_int This, u_int pos) {
    StackFrame_GetArg(Dest, This, STACKFRAME_BASE_SIZE + pos);
  }
  void StackFrame_Put(u_int This, u_int pos, u_int Value) {
    StackFrame_InitArg(This, STACKFRAME_BASE_SIZE + pos, Value);
  }
  void StackFrame_Replace(u_int This, u_int pos, u_int Value) {
    StackFrame_InitArg(This, STACKFRAME_BASE_SIZE + pos, Value);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Transform_New(u_int This) {
    JITStore::AllocBlock(This, TRANSFORM_LABEL, TRANSFORM_SIZE);
  }
  void Transform_PutName(u_int This, u_int Value) {
    JITStore::InitArg(This, TRANSFORM_NAME_POS, Value);
  }
  void Transform_PutArgument(u_int This, u_int Value) {
    JITStore::InitArg(This, TRANSFORM_ARGUMENT_POS, Value);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Tuple_New(u_int This, u_int size) {
    JITStore::AllocBlock(This, TUPLE_LABEL, size);
  }
  u_int Tuple_Sel() {
    return 0;
  }
  void Tuple_Sel(u_int Dest, u_int This, u_int pos) {
    JITStore::GetArg(Dest, This, pos);
  }
  void Tuple_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, pos, Value);
  }
  void Tuple_IndexSel(u_int Dest, u_int This, u_int Pos) {
    jit_addi_ui(Pos, Pos, 1);
    // to be done: compute log_2 sizeof(word)
    jit_lshi_ui(Pos, Pos, 2);
    jit_ldxr_p(Dest, This, Pos);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Closure_New(u_int This, u_int size) {
    JITStore::AllocBlock(This, CLOSURE_LABEL, CLOSURE_BASE_SIZE + size);
  }
  void Closure_InitConcreteCode(u_int This, u_int Value) {
    JITStore::InitArg(This, CLOSURE_CONCRETE_CODE_POS, Value);
  }
  void Closure_GetConcreteCode(u_int Dest, u_int This) {
    JITStore::GetArg(Dest, This, CLOSURE_CONCRETE_CODE_POS);
  }
  void Closure_Sel(u_int This, u_int Dest, u_int pos) {
    JITStore::GetArg(Dest, This, CLOSURE_BASE_SIZE + pos);
  }
  void Closure_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, CLOSURE_BASE_SIZE + pos, Value);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void ConcreteCode_New(u_int This, Interpreter *interpreter, u_int size) {
    JITStore::AllocBlock(This, CONCRETE_LABEL,
			 CONCRETEREPRESENTATION_BASE_SIZE + size);
    ConcreteRepresentationHandler *handler = interpreter;
    (void) jit_movi_p(JIT_R0, Store::UnmanagedPointerToWord(handler));
    JITStore::InitArg(This, CONCRETEREPRESENTATION_HANDLER_POS, JIT_R0);
  }
  void ConcreteCode_Sel(u_int Dest, u_int This, u_int pos) {
    JITStore::GetArg(Dest, This, CONCRETEREPRESENTATION_BASE_SIZE + pos);
  }
  void ConcreteCode_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, CONCRETEREPRESENTATION_BASE_SIZE + pos, Value);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Hole_New(u_int This) {
    JITStore::AllocTransient(This, HOLE_LABEL);
    // no associated future
    (void) jit_movi_p(JIT_R0, Store::IntToWord(0));
    JITStore::InitArg(This, TRANSIENT_REF_POS, JIT_R0);
  }

  // Side-Effect: Scratches JIT_R0, JIT_FP
  void Byneed_New(u_int This) {
    JITStore::AllocTransient(This, BYNEED_LABEL);
  }
  void Byneed_InitClosure(u_int This, u_int Closure) {
    JITStore::InitArg(This, TRANSIENT_REF_POS, Closure);
  }

  // Side-Effect: Scratches JIT_R0
  void Primitive_Return1(u_int Value) {
    Scheduler_PutZeroArg(Value);
    jit_movi_ui(JIT_R0, 1);
    Scheduler_PutNArgs(JIT_R0);
  }
};

#endif

#endif
