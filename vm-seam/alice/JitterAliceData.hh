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

#ifndef __ALICE__JITTER_ALICE_DATA_HH__
#define __ALICE__JITTER_ALICE_DATA_HH__

#if HAVE_LIGHTNING

#include "generic/JitterGenericData.hh"
#include "alice/Data.hh"

class JITAlice : public JITGeneric {
protected:
  enum { CELL_VALUE_POS, CELL_SIZE };
  enum { CONVAL_CON_POS, CONVAL_BASE_SIZE };
  enum { VECTOR_LENGTH_POS, VECTOR_BASE_SIZE };
  enum { RECORD_WIDTH_POS, RECORD_BASE_SIZE };
public:
  void Cell_New(u_int This) {
    JITStore::AllocBlock(This, Alice::Cell, CELL_SIZE);
  }
  void Cell_Put(u_int This, u_int Value) {
    JITStore::InitArg(This, CELL_VALUE_POS, Value);
  }
  u_int Cell_Sel() {
    return CELL_VALUE_POS;
  }
  void Cell_Sel(u_int Dest, u_int This) {
    JITStore::GetArg(Dest, This, CELL_VALUE_POS);
  }

  void ConVal_New(u_int This, u_int size) {
    JITStore::AllocBlock(This, Alice::ConVal, CONVAL_BASE_SIZE + size);
  }
  void ConVal_InitConstr(u_int This, u_int Value) {
    JITStore::InitArg(This, CONVAL_CON_POS, Value);
  }
  void ConVal_GetConstructor(u_int Dest, u_int This) {
    JITStore::GetArg(Dest, This, CONVAL_CON_POS);
  }
  u_int ConVal_Sel() {
    return CONVAL_BASE_SIZE;
  }
  void ConVal_Sel(u_int Dest, u_int This, u_int pos) {
    JITStore::GetArg(Dest, This, CONVAL_BASE_SIZE + pos);
  }
  void ConVal_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, CONVAL_BASE_SIZE + pos, Value);
  }

  void TagVal_New(u_int This, u_int tag, u_int size) {
    JITStore::AllocBlock(This, Alice::TagToLabel(tag), size);
  }
  u_int TagVal_Sel() {
    return 0;
  }
  void TagVal_Sel(u_int Dest, u_int This, u_int pos) {
    JITStore::GetArg(Dest, This, pos);
  }
  void TagVal_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, pos, Value);
  }
  void TagVal_GetTag(u_int Dest, u_int This) {
    JITStore::Block_GetLabel(Dest, This);
    jit_subi_ui(Dest, Dest, Alice::MIN_TAG);
  }

  void Vector_New(u_int This, u_int size) {
    JITStore::AllocBlock(This, Alice::Vector, VECTOR_BASE_SIZE + size);
    jit_movi_p(JIT_R0, Store::IntToWord(size));
    JITStore::InitArg(This, VECTOR_LENGTH_POS, JIT_R0);
  }
  void Vector_GetLength(u_int Dest, u_int This) {
    JITStore::GetArg(Dest, This, VECTOR_LENGTH_POS);
  }
  u_int Vector_Sel() {
    return VECTOR_BASE_SIZE;
  }
  void Vector_Sel(u_int Dest, u_int This, u_int pos) {
    JITStore::GetArg(Dest, This, VECTOR_BASE_SIZE + pos);
  }
  void Vector_Put(u_int This, u_int pos, u_int Value) {
    JITStore::InitArg(This, VECTOR_BASE_SIZE + pos, Value);
  }

  void Record_New(u_int This, u_int n) {
    JITStore::AllocBlock(This, Alice::Record, RECORD_BASE_SIZE + n * 2);
    jit_movi_p(JIT_R0, Store::IntToWord(n));
    JITStore::InitArg(This, RECORD_WIDTH_POS, JIT_R0);
  }
  void Record_GetWidth(u_int Dest, u_int This) {
    JITStore::GetArg(Dest, This, RECORD_WIDTH_POS);
  }
  void Record_InitLabel(u_int This, u_int i, u_int Label) {
    JITStore::InitArg(This, RECORD_BASE_SIZE + i * 2, Label);
  }
  void Record_InitValue(u_int This, u_int i, u_int Value) {
    JITStore::InitArg(This, RECORD_BASE_SIZE + i * 2 + 1, Value);
  }
};

#endif

#endif
