//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2002
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__JIT_STORE_HH__
#define __STORE__JIT_STORE_HH__

#if HAVE_LIGHTNING

#if defined(INTERFACE)
#pragma interface "store/JITStore.hh"
#endif

#include <cstdio>
#include "store/Store.hh"

#define SEAM_LIGHTNING
#include <lightning.h>

#ifdef JIT_FP
# define HAVE_JIT_FP 1
# define JIT_MYFP    JIT_FP
#else
# define HAVE_JIT_FP 0
# define JIT_MYFP    JIT_R1
#endif

// Encapsulate Lightning State
class SeamDll LightningState {
public:
  jit_state lightning;
  LightningState() {}
};

// We have to redefine _jit - make compiler happy
inline void _jit_make_compiler_happy(void) { _jit = _jit; }

// Lightning macros refer to the defined state
#define _jit this->lightning

extern "C" {
  void disassemble(std::FILE *, char *, char *);
};
typedef void (*value_plotter)(word);

class JITStore : protected LightningState {
protected:
  u_int nbArgs;
  bool saveCallerSavedRegs;

  // Position independent procedure call and restore caller-saved registers
  // Side-Effect: Result is returned in JIT_R0
  void Call(u_int nbArgs, void *proc) {
    (void) jit_movi_p(JIT_R0, proc);
    if (nbArgs != 0) {
      jit_callr(JIT_R0);
      // TODO: Find correct platform test
#if HAVE_JIT_FP
      // Remove arguments from the stack
      jit_addi_ui(JIT_SP, JIT_SP, nbArgs * sizeof(word));
#endif
    }
    else {
      jit_callr(JIT_R0);
    }
    // Move the return value to JIT_R0
    jit_retval(JIT_R0);
  }
public:
  // Save caller-saved registers and initialize argument bank
  void Prepare(u_int nbArgs, bool saveCallerSavedRegs = true) {
    JITStore::saveCallerSavedRegs = saveCallerSavedRegs;
    if (saveCallerSavedRegs) {
      jit_pushr_ui(JIT_R1);
      jit_pushr_ui(JIT_R2);
    }
    JITStore::nbArgs = nbArgs;
    if (nbArgs != 0) {
      jit_prepare(nbArgs);
    }
  }
  void Finish(void *proc) {
    Call(nbArgs, proc);
    if (saveCallerSavedRegs) {
      jit_popr_ui(JIT_R2);
      jit_popr_ui(JIT_R1);
    }
  }
  // Return from subroutine: Result is taken from JIT_R0
  void Return() {
    if (JIT_RET != JIT_R0) {
      jit_movr_p(JIT_RET, JIT_R0);
    }
    jit_ret();
  }
  void SaveAllRegs() {
    jit_pushr_ui(JIT_R0);
    jit_pushr_ui(JIT_R1);
    jit_pushr_ui(JIT_R2);
    jit_pushr_ui(JIT_V0);
    jit_pushr_ui(JIT_V1);
    jit_pushr_ui(JIT_V2);
#if HAVE_JIT_FP
    jit_pushr_ui(JIT_FP);
#endif
  }
  void RestoreAllRegs() {
#if HAVE_JIT_FP
    jit_popr_ui(JIT_FP);
#endif
    jit_popr_ui(JIT_V2);
    jit_popr_ui(JIT_V1);
    jit_popr_ui(JIT_V0);
    jit_popr_ui(JIT_R2);
    jit_popr_ui(JIT_R1);
    jit_popr_ui(JIT_R0);
  }
protected:
#if defined(JIT_STORE_DEBUG)
  void CompileMessage(const char *info);
  void CompileRegister(u_int Reg);
#endif
  static void AllocHeapChunk() {
    Store::roots[0].Enlarge();
  }
  // Output: Ptr holds Allocated Block
  // Side-Effect: Scratches JIT_R0, JIT_FP
  // to be done: more efficient solution
  void Alloc(u_int Ptr, u_int size, u_int header) {
    Assert(Ptr != JIT_R0);
    Assert(Ptr != JIT_MYFP);
    if (Ptr == JIT_MYFP) {
      Error("JITStore::Alloc: Ptr is JIT_R1\n");
    }
#if !HAVE_JIT_FP
    jit_pushr_ui(JIT_MYFP);
#endif
    // Allocation Loop
    jit_insn *loop = jit_get_label();
    jit_ldi_p(JIT_R0, Store::roots);
    jit_ldr_p(Ptr, JIT_R0);
    jit_movi_ui(JIT_MYFP, header);
    jit_str_ui(Ptr, JIT_MYFP);
    jit_addi_p(Ptr, Ptr, size);
    jit_ldxi_p(JIT_MYFP, JIT_R0, sizeof(word));
    jit_insn *succeeded = jit_bltr_p(jit_forward(), Ptr, JIT_MYFP);
    Prepare(0);
    Finish((void *) JITStore::AllocHeapChunk);
    (void) jit_jmpi(loop);
    jit_patch(succeeded);
    jit_str_p(JIT_R0, Ptr);
    jit_subi_p(Ptr, Ptr, size);
#if !HAVE_JIT_FP
    jit_popr_ui(JIT_MYFP);
#endif
  }
public:
  // Input: word ptr
  // Output: derefed word ptr
  // Side-Effect: Scratches JIT_R0
  void Deref(u_int Ptr) {
    Assert(Ptr != JIT_R0);
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    jit_xori_ui(Ptr, Ptr, TRTAG);
    // R0 = Ptr->GetLabel();
    jit_ldr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    jit_insn *unbound_transient = jit_bnei_ui(jit_forward(), JIT_R0, tag);
    jit_ldxi_p(Ptr, Ptr, 1 * sizeof(word));
    (void) jit_jmpi(loop);
    jit_patch(unbound_transient);
    jit_xori_ui(Ptr, Ptr, TRTAG); // Restore tag
    jit_patch(no_transient);
  }
  void SaveDeref(u_int Ptr) {
    if (Ptr == JIT_R0) {
      jit_pushr_ui(JIT_V1);
      jit_movr_ui(JIT_V1, JIT_R0);
      Deref(JIT_V1);
      jit_movr_ui(JIT_R0, JIT_V1);
      jit_popr_ui(JIT_V1);
    }
    else
      Deref(Ptr);
  }
#if defined(JIT_STORE_DEBUG)
  // Logging Support
  SeamMemberDll static void InitLoggging();
  SeamMemberDll void LogMesg(const char *mesg);
  SeamMemberDll void LogReg(u_int Value);
  SeamMemberDll void DumpReg(u_int Value, value_plotter plotter);
  SeamMemberDll void LogRead(u_int Dest, u_int Ptr, u_int Index);
  SeamMemberDll void LogWrite(u_int Ptr, u_int index, u_int Value);
  SeamMemberDll void LogSetArg(u_int pos, u_int Value);
#endif
  // Input: word ptr
  // Output: derefed word ptr and jmp to the corresponding path
  // Side-Effect: Scratches JIT_R0
  void Deref3(u_int Ptr, jit_insn *ref[2]) {
    Assert(Ptr != JIT_R0);
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    jit_xori_ui(Ptr, Ptr, TRTAG);
    // R0 = V1->GetLabel();
    jit_ldr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    ref[0] = jit_bnei_ui(jit_forward(), JIT_R0, tag); // Transient branch
    jit_ldxi_p(Ptr, Ptr, 1 * sizeof(word));
    (void) jit_jmpi(loop);
    jit_patch(no_transient);
    ref[1] = jit_beqi_ui(jit_forward(), JIT_R0, BLKTAG); // Block Branch
  }
  // Input: word ptr
  // Output: derefed word ptr and jmp to the corresponding path
  // Side-Effect: Scratches JIT_R0
  void DerefItem(u_int Ptr, jit_insn *ref[2]) {
    Assert(Ptr != JIT_R0);
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    ref[1] = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG); // Item Branch
    jit_xori_ui(Ptr, Ptr, TRTAG);
    // R0 = Ptr->GetLabel();
    jit_ldr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    ref[0] = jit_bnei_ui(jit_forward(), JIT_R0, tag); // Transient branch
    jit_ldxi_p(Ptr, Ptr, 1 * sizeof(word));
    (void) jit_jmpi(loop);
  }
  void SetTransientTag(u_int Ptr) {
    jit_xori_ui(Ptr, Ptr, TRTAG);
  }
  //
  // Block Field Access
  //
#if defined(JIT_ASSERT_INDEX)
  word loadedWord;
  void SecureGetArg(u_int pos, ::Block *b) {
    Assert(b != INVALID_POINTER);
    loadedWord = b->GetArg(pos);
  }
  void SecureInitArg(u_int pos, ::Block *b, word value) {
    b->InitArg(pos, value);
  }
  void SecureReplaceArg(u_int pos, ::Block *b, word value) {
    b->ReplaceArg(pos, value);
  }
#endif
  u_int Sel() {
    return 1;
  }
  void Sel(u_int Dest, u_int Ptr, u_int index) {
    jit_ldxi_p(Dest, Ptr, index * sizeof(word));
  }
  void GetArg(u_int Dest, u_int Ptr, u_int index) {
#if defined(JIT_ASSERT_INDEX)
    SaveAllRegs();
    Prepare(2);
    jit_pusharg_ui(Ptr);
    jit_movi_ui(JIT_R0, index);
    jit_pusharg_ui(JIT_R0);
    Finish((void *) SecureGetArg);
    RestoreAllRegs();
    jit_ldi_p(Dest, &JITStore::loadedWord);
#else
    jit_ldxi_p(Dest, Ptr, (index + 1) * sizeof(word));
#endif
  }
  void InitArg(u_int Ptr, u_int index, u_int Value) {
#if defined(JIT_ASSERT_INDEX)
    SaveAllRegs();
    Prepare(3);
    jit_pusharg_ui(Value);
    jit_pusharg_ui(Ptr);
    jit_movi_ui(JIT_R0, index);
    jit_pusharg_ui(JIT_R0);
    Finish((void *) SecureInitArg);
    RestoreAllRegs();
#else
    jit_stxi_p((index + 1) * sizeof(word), Ptr, Value);
#endif
  }
  void ReplaceArg(u_int Ptr, u_int index, u_int Value) {
#if defined(JIT_ASSERT_INDEX)
    SaveAllRegs();
    Prepare(3);
    jit_pusharg_ui(Value);
    jit_pusharg_ui(Ptr);
    jit_movi_ui(JIT_R0, index);
    jit_pusharg_ui(JIT_R0);
    Finish((void *) SecureReplaceArg);
    RestoreAllRegs();
#else
    if (STORE_GENERATION_NUM == 2) {
      jit_stxi_p((index + 1) * sizeof(word), Ptr, Value);
    } 
    else {
      Prepare(3);
      jit_pusharg_ui(Value);
      jit_pusharg_ui(Ptr);
      jit_movi_ui(JIT_R0, index);
      jit_pusharg_ui(JIT_R0);
      Finish((void *) Store::JITReplaceArg);
    }
#endif
  }
  //
  // Store Allocation
  //
  // Output: Ptr = Allocated Block
  void AllocBlock(u_int Ptr, BlockLabel label, u_int size) {
    size = HeaderOp::TranslateSize(size);
    u_int header = HeaderOp::EncodeHeader(label, size, 0);
    Alloc(Ptr, SIZEOF_BLOCK(size), header);
  }
  // Output: Ptr = chunk ptr
  void AllocChunk(u_int Ptr, u_int size) {
    u_int ws = (1 + (((size + sizeof(u_int)) - 1) / sizeof(u_int)));
    AllocBlock(Ptr, CHUNK_LABEL, ws);
    jit_movi_ui(JIT_R0, PointerOp::EncodeInt(size));
    InitArg(Ptr, 0, JIT_R0);
  }
  // Output: Ptr = transient ptr
  void AllocTransient(u_int Ptr, BlockLabel label) {
    AllocBlock(Ptr, label, 1);
  }
  //
  // Store Import/Export
  //
  // Dest = Store::IntToWord(Int);
  void IntToWord(u_int Dest, u_int Int) {
    if (Dest != Int)
      jit_movr_ui(Dest, Int);
    jit_lshi_ui(Dest, Dest, 1);
    jit_ori_ui(Dest, Dest, INTTAG);
  }
  // Input: Ptr word ptr
  // Output: Dest integer
  void DirectWordToInt(u_int Dest, u_int Ptr) {
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
    jit_rshi_i(Dest, Dest, 1); // sign bit is propagated
  }
  // Input: Ptr = word ptr
  // Output: Dest = integer or INVALID_INT
  void WordToInt(u_int Dest, u_int Ptr) {
    Deref(Ptr);
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_int = jit_bnei_ui(jit_forward(), JIT_R0, INTTAG);
    DirectWordToInt(Dest, Ptr);
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_int);
    jit_movi_ui(Dest, INVALID_INT);
    jit_patch(eob);
  }
  // Input: Ptr word ptr
  // Output: Dest block ptr
  void DirectWordToBlock(u_int Dest, u_int Ptr) {
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
  }
  // Input: Ptr = word ptr
  // Output: Dest = block ptr
  void WordToBlock(u_int Dest, u_int Ptr) {
    Deref(Ptr);
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_block = jit_bnei_ui(jit_forward(), JIT_R0, BLKTAG);
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_block);
    jit_movr_ui(Dest, INVALID_POINTER);
    jit_patch(eob);
  }
  // Input: Ptr = word ptr
  // Output: Dest = transient ptr
  void DirectWordToTransient(u_int Dest, u_int Ptr) {
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
    jit_xori_ui(Dest, Ptr, TRTAG);
  }
  // Intput: Ptr = word ptr
  // Output Dest = transient ptr
  void WordToTransient(u_int Dest, u_int Ptr) {
    Deref(Ptr);
    jit_movr_ui(JIT_R0, Ptr);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
    jit_xori_ui(Dest, Dest, TRTAG);
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_transient);
    jit_movr_ui(Dest, INVALID_POINTER);
    jit_patch(eob);
  }
  // Dest = Store::PointerToWord(Ptr);
  void PointerToWord(u_int Dest, u_int Ptr) {
    if (Dest != Ptr)
      jit_movr_ui(Dest, Ptr);
    jit_lshi_ui(Dest, Dest, 1);
    jit_ori_ui(Dest, Dest, INTTAG);
  }
  // Dest = Store::DirectWordToPointer(Ptr);
  void DirectWordToPointer(u_int Dest, u_int Ptr) {
    jit_rshi_ui(Dest, Ptr, 1);
  }
  // Machine Status
  void LoadStatus(u_int Dest) {
    jit_ldi_ui(Dest, &StatusWord::status);
  }
  //
  // Store Values
  //
  // Dest = This->GetSize()
  void Block_GetSize(u_int Dest, u_int This) {
    Assert(Dest != JIT_MYFP);
    if (Dest == JIT_MYFP) {
      Error("JITStore::Block_GetSize: Dest is JIT_R1\n");
    }
#if !HAVE_JIT_FP
    jit_pushr_ui(JIT_MYFP);
#endif
    // Compute Size
    jit_ldr_ui(Dest, This);
    jit_movr_ui(JIT_MYFP, Dest);
    jit_andi_ui(Dest, Dest, SIZE_MASK);
    jit_rshi_ui(Dest, Dest, SIZE_SHIFT);
    jit_andi_ui(JIT_MYFP, JIT_MYFP, SIZESHIFT_MASK);
    jit_lshr_ui(Dest, Dest, JIT_MYFP);
#if !HAVE_JIT_FP
    jit_popr_ui(JIT_MYFP);
#endif
  }
  // Dest = This->GetLabel()
  void Block_GetLabel(u_int Dest, u_int This) {
    jit_ldr_ui(Dest, This);
    jit_andi_ui(Dest, Dest, TAG_MASK);
    jit_rshi_ui(Dest, Dest, TAG_SHIFT);
  }
  // Dest = This->GetSize() (in bytes)
  void Chunk_GetSize(u_int Dest, u_int This) {
    GetArg(Dest, This, 0);
  }
  // Dest = This->GetBase();
  void Chunk_GetBase(u_int Dest, u_int This) {
    jit_movr_ui(Dest, This);
    jit_addi_ui(Dest, Dest, 2 * sizeof(word));
  }
};

#endif

#endif
