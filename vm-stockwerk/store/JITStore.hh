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

#if defined(INTERFACE)
#pragma interface "store/JITStore.hh"
#endif

#include <cstdio>
#include "store/Store.hh"
#include <lightning.h>

// Make sure to have one lightning copy only
extern jit_state lightning;
#define _jit lightning

// Provide own jump/branch macros
// This is to eliminate the "value computed not used" warning
// in case the destination label is already known

// Lightning Jumps
#define drop_jit_jmpi(O1) \
  { jit_insn *dummy = jit_jmpi(O1); dummy = dummy; }

#define drop_jit_jmpr(O1) \
  { jit_insn *dummy = jit_jmpr(O1); dummy = dummy; }

// Lightning Branches (ui part only)
#define drop_jit_blti_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_blti_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_bltr_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bltr_ui(O1, O2, O3); dummy = dummy; }

#define drop_jit_blei_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_blei_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_bler_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bler_ui(O1, O2, O3); dummy = dummy; }

#define drop_jit_bgti_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bgti_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_bgtr_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bgtr_ui(O1, O2, O3); dummy = dummy; }

#define drop_jit_bgei_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bgei_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_bger_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bger_ui(O1, O2, O3); dummy = dummy; }

#define drop_jit_beqi_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_beqi_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_beqr_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_beqr_ui(O1, O2, O3); dummy = dummy; }

#define drop_jit_bnei_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bnei_ui(O1, O2, O3); dummy = dummy; }
#define drop_jit_bner_ui(O1, O2, O3) \
  { jit_insn *dummy = jit_bner_ui(O1, O2, O3); dummy = dummy; }

extern "C" {
  void disassemble(FILE *, char *, char *);
};

class JITStore {
public:
  // The one and only call abstaction
  static void Call(u_int nbArgs, void *proc) {
    jit_movi_p(JIT_R0, proc);
    jit_callr(JIT_R0);
    if (nbArgs != 0) {
      jit_addi_ui(JIT_SP, JIT_SP, nbArgs * sizeof(word));
    }
  }
protected:
  // Output: V1 = Allocated Block
  static void Alloc(u_int size, u_int header) {
    // Preserve Context
    jit_pushr_ui(JIT_R1);
    jit_pushr_ui(JIT_V2);
    // Allocation Loop
    jit_insn *loop = jit_get_label();
    jit_ldi_p(JIT_V2, &Store::curChunk); // V2 = Store::curChunk;
    jit_ldxi_p(JIT_V1, JIT_V2, 4 * sizeof(word)); // V1 = curChunk->GetTop();
    jit_addi_p(JIT_R0, JIT_V1, size); // R0 = V1 + size;
    jit_ldxi_p(JIT_R1, JIT_V2, 5 * sizeof(word)); // R1 = curChunk->GetMax();
    // if R0 < R1 then goto succeeded;
    jit_insn *succeeded = jit_bltr_p(jit_forward(), JIT_R0, JIT_R1); 
    // Alloc new MemChunk and continue loop
    jit_movi_ui(JIT_R0, size); // R0 = size;
    jit_pushr_ui(JIT_R0);
    // Do an indirect call to AllocNewMemChunkStd(size)
    // Necessary to be moveable
    Call(1, (void *) Store::AllocNewMemChunkStd);
    drop_jit_jmpi(loop);
    jit_patch(succeeded);
    jit_stxi_p(4 * sizeof(word), JIT_V2, JIT_R0); // curChunk->SetTop(R0);
    jit_movi_ui(JIT_R0, header); // R0 = header;
    jit_stxi_ui(0, JIT_V1, JIT_R0); // *p = R0;
    // Restore Context
    jit_popr_ui(JIT_V2);
    jit_popr_ui(JIT_R1);
  }
  // Input: V1 = word ptr
  // Output: V1 = derefed word ptr
  static void Deref() {
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    jit_xori_ui(JIT_V1, JIT_V1, TRTAG);
    // R0 = V1->GetLabel();
    jit_ldr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    jit_insn *unbound_transient = jit_bnei_ui(jit_forward(), JIT_R0, tag);
    jit_ldxi_p(JIT_V1, JIT_V1, 1 * sizeof(word));
    drop_jit_jmpi(loop); // Continue deref
    jit_patch(unbound_transient);
    jit_xori_ui(JIT_V1, JIT_V1, TRTAG); // Restore tag
    jit_patch(no_transient);
  }
public:
  // Logging Support
  static void InitLoggging();
  static void LogMesg(const char *mesg);
  static void LogReg(u_int Value);
  static void LogRead(u_int Dest, u_int Ptr, u_int Index);
  static void LogWrite(u_int Ptr, u_int Index, u_int Value);
  // Input: V1 = word ptr
  // Output: V1 = derefed word ptr and jmp to the corresponding path
  static void Deref3(jit_insn *ref[2]) {
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    jit_xori_ui(JIT_V1, JIT_V1, TRTAG);
    // R0 = V1->GetLabel();
    jit_ldr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    ref[0] = jit_bnei_ui(jit_forward(), JIT_R0, tag); // Transient branch
    jit_ldxi_p(JIT_V1, JIT_V1, 1 * sizeof(word));
    drop_jit_jmpi(loop); // Continue deref
    jit_patch(no_transient);
    ref[1] = jit_beqi_ui(jit_forward(), JIT_R0, BLKTAG); // Block Branch
  }
  // Input: V1 = word ptr
  // Output: V1 = derefed word ptr and jmp to the corresponding path
  static void DerefItem(jit_insn *ref[2]) {
    // Main deref loop
    jit_insn *loop = jit_get_label();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    ref[1] = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG); // Item Branch
    jit_xori_ui(JIT_V1, JIT_V1, TRTAG);
    // R0 = V1->GetLabel();
    jit_ldr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK);
    u_int tag = REF_LABEL << TAG_SHIFT;
    ref[0] = jit_bnei_ui(jit_forward(), JIT_R0, tag); // Transient branch
    jit_ldxi_p(JIT_V1, JIT_V1, 1 * sizeof(word));
    drop_jit_jmpi(loop); // Continue deref
  }
  static void PatchTag() {
    jit_xori_ui(JIT_V1, JIT_V1, TRTAG);
  }
  //
  // Block Field Access
  //
  static void GetArg(u_int Dest, u_int Ptr, u_int Index) {
    LogRead(Dest, Ptr, Index);
    jit_ldxi_p(Dest, Ptr, (Index + 1) * sizeof(word));
    LogMesg("passed\n");
  }
  static void InitArg(u_int Ptr, u_int Index, u_int Value) {
    LogWrite(Ptr, Index, Value);
    jit_stxi_p((Index + 1) * sizeof(word), Ptr, Value);
    LogMesg("passed\n");
  }
  // To be done
  static void ReplaceArg(u_int Ptr, u_int Index, u_int Value) {
    jit_stxi_p((Index + 1) * sizeof(word), Ptr, Value);
  }
  //
  // Store Allocation
  //
  // Output: V1 = Allocated Block
  static void AllocBlock(BlockLabel label, u_int size) {
    size = HeaderOp::TranslateSize(size);
    Alloc(Store::BlockMemSize(size), HeaderOp::EncodeHeader(label, size, 0));
  }
  // Output: V1 = chunk ptr
  static void AllocChunk(u_int size) {
    u_int ws = (1 + (((size + sizeof(u_int)) - 1) / sizeof(u_int)));
    AllocBlock(CHUNK_LABEL, ws);
    jit_movi_ui(JIT_R0, PointerOp::EncodeInt(size));
    InitArg(JIT_V1, 0, JIT_R0);
  }
  // Output: V1 = transient ptr
  static void AllocTransient(BlockLabel label) {
    AllocBlock(label, 1);
  }
  //
  // Store Import/Export
  //
  // Dest = Store::IntToWord(Int);
  static void IntToWord(u_int Dest, u_int Int) {
    jit_movr_ui(Dest, Int);
    jit_lshi_ui(Dest, Dest, 1);
    jit_ori_ui(Dest, Dest, INTTAG);
  }
  // Input: V1 = word ptr
  // Output: R0 = integer
  static void DirectWordToInt() {
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_rshi_i(JIT_R0, JIT_R0, 1); // sign bit is propagated
  }
  static void DirectWordToInt(u_int Ptr) {
    jit_rshi_i(Ptr, Ptr, 1); // sign bit is propagated
  }
  // Input: V1 = word ptr
  // Output: R0 = integer or INVALID_INT
  static void WordToInt() {
    Deref();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_int = jit_bnei_ui(jit_forward(), JIT_R0, INTTAG);
    DirectWordToInt();
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_int);
    jit_movi_ui(JIT_R0, INVALID_INT);
    jit_patch(eob);
  }
  // Input: V1 = word ptr
  // Output: R0 = block ptr
  static void DirectWordToBlock() {
    jit_movr_ui(JIT_R0, JIT_V1);
  }
  // Input: V1 = word ptr
  // Output: R0 = block ptr
  static void WordToBlock() {
    Deref();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_block = jit_bnei_ui(jit_forward(), JIT_R0, BLKTAG);
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_block);
    jit_movr_ui(JIT_R0, INVALID_POINTER);
    jit_patch(eob);
  }
  // Input: V1 = word ptr
  // Output: R0 = transient ptr
  static void DirectWordToTransient() {
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_xori_ui(JIT_R0, JIT_R0, TRTAG);
  }
  // Intput: V1 = word ptr
  // Output R0 = transient ptr
  static void WordToTransient() {
    Deref();
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_andi_ui(JIT_R0, JIT_R0, TAGMASK);
    jit_insn *no_transient = jit_bnei_ui(jit_forward(), JIT_R0, TRTAG);
    jit_movr_ui(JIT_R0, JIT_V1);
    jit_xori_ui(JIT_R0, JIT_R0, TRTAG);
    jit_insn *eob = jit_jmpi(jit_forward());
    jit_patch(no_transient);
    jit_movr_ui(JIT_R0, INVALID_POINTER);
    jit_patch(eob);
  }
  // Dest = Store::PointerToWord(Ptr);
  static void PointerToWord(u_int Dest, u_int Ptr) {
    jit_movr_ui(Dest, Ptr);
    jit_lshi_ui(Dest, Dest, 1);
    jit_ori_ui(Dest, Dest, INTTAG);
  }
  // Dest = Store::DirectWordToPointer(Word);
  static void DirectWordToPointer(u_int Dest, u_int Word) {
    jit_rshi_ui(Dest, Word, 1);
  }
  // Dynamic Alloc
  // Input: V1 = size
  // Output: V1 = tuple ptr
  static void DynamicAlloc() {
    // Preserve Context
    jit_pushr_ui(JIT_R1);
    jit_pushr_ui(JIT_V2);
    // Save the real size
    jit_movi_ui(JIT_R1, JIT_V1); 
    // Compute BlockMemSize
    jit_addi_ui(JIT_V1, JIT_V1, 1);
    jit_muli_ui(JIT_V1, JIT_V1, sizeof(u_int));
    // Create dummy header
    jit_movi_ui(JIT_R0, HeaderOp::EncodeHeader(TUPLE_LABEL, 1, 0));
    // Enter appropriate size
    jit_andi_ui(JIT_R0, JIT_R0, ~SIZE_MASK);
    jit_lshi_ui(JIT_R1, JIT_R1, SIZE_SHIFT);
    jit_orr_ui(JIT_R0, JIT_R0, JIT_R1);
    jit_movi_ui(JIT_R1, JIT_V1);
    // R0 = header, R1 = BlockMemSize
    // Allocation Loop
    jit_insn *loop = jit_get_label();
    jit_pushr_ui(JIT_R1); // Save BlockMemSize
    jit_pushr_ui(JIT_R0); // Save Header
    jit_ldi_p(JIT_V2, &Store::curChunk); // V2 = Store::curChunk;
    jit_ldxi_p(JIT_V1, JIT_V2, 4 * sizeof(word)); // V1 = curChunk->GetTop();
    jit_addr_p(JIT_R0, JIT_V1, JIT_R1); // R0 = V1 + size;
    jit_ldxi_p(JIT_R2, JIT_V2, 5 * sizeof(word)); // R2 = curChunk->GetMax();
    // if R0 < R2 then goto ref;
    jit_insn *ref = jit_bltr_p(jit_forward(), JIT_R0, JIT_R2); 
    // Alloc new MemChunk and continue loop
    jit_pushr_ui(JIT_R1);
    // Do an indirect call to AllocNewMemChunkStd (necessary to be movable)
    Call(1, (void *) Store::AllocNewMemChunkStd);
    jit_popr_ui(JIT_R0);
    jit_popr_ui(JIT_R1);
    drop_jit_jmpi(loop);
    jit_patch(ref);
    jit_stxi_p(4 * sizeof(word), JIT_V2, JIT_R0); // curChunk->SetTop(R0);
    jit_popr_ui(JIT_R0); // R0 = header;
    jit_stxi_ui(0, JIT_V1, JIT_R0); // *p = R0;
    // Restore Context
    jit_addi_ui(JIT_SP, JIT_SP, sizeof(word));
    jit_popr_ui(JIT_V2);
    jit_popr_ui(JIT_R1);
  }
  // NeedGC
  static void NeedGC(u_int Dest) {
    jit_ldi_ui(Dest, &Store::needGC);
  }
  //
  // Store Values
  //
  class Block {
  public:
    // Input: V1 = Block Ptr
    // Output: R0 = Block Size
    static void GetSize() {
      // Save Context
      jit_pushr_ui(JIT_R1);
      // Compute Size
      jit_ldr_ui(JIT_R0, JIT_V1); // R0 = *V1;
      jit_movr_ui(JIT_R1, JIT_R0); // R1 = R0;
      jit_andi_ui(JIT_R0, JIT_R0, SIZE_MASK); // R0 = R0 & SIZE_MASK;
      jit_rshi_ui(JIT_R0, JIT_R0, SIZE_SHIFT); // R0 = R0 >> SIZE_SHIFT;
      jit_andi_ui(JIT_R1, JIT_R1, SIZESHIFT_MASK); // R1 = R1 & SIZESHIFT_MASK;
      jit_lshr_ui(JIT_R0, JIT_R0, JIT_R1); // R0 = R0 << R1;
      // Restore Context
      jit_popr_ui(JIT_R1);
    }
    // Input: V1 = Block Ptr
    // Output: R0 = BlockLabel
    static void GetLabel() {
      jit_ldr_ui(JIT_R0, JIT_V1); // R0 = *V1;
      jit_andi_ui(JIT_R0, JIT_R0, TAG_MASK); // R0 = R0 & TAG_MASK;
      jit_rshi_ui(JIT_R0, JIT_R0, TAG_SHIFT); // R0 = R0 >> TAG_SHIFT;
    }
  };

  class Chunk {
  public:
    // Input: V1 = Chunk Ptr
    // Output: R0 = Chunk byte size (word)
    static void GetSize(u_int Dest) {
      GetArg(Dest, JIT_V1, 0);
    }
    // Dest = V1->GetBase();
    static void GetBase(u_int Dest) {
      jit_movr_ui(Dest, JIT_V1);
      jit_addi_ui(Dest, Dest, 2 * sizeof(word));
    }
  };
};

#endif
