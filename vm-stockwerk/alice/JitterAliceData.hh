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

#include "store/JITStore.hh"
#include "alice/Data.hh"

namespace JITAlice {
  class Cell {
  protected:
    static const u_int VALUE_POS = 0;
    static const u_int SIZE      = 1;
  public:
    static void New() {
      JITStore::AllocBlock(Alice::Cell, SIZE);
    }
    static void Put(u_int Value) {
      JITStore::InitArg(JIT_V1, VALUE_POS, Value);
    }
    static void Sel(u_int Dest) {
      JITStore::GetArg(Dest, JIT_V1, VALUE_POS);
    }
  };

  class ConVal {
  protected:
    static const u_int CONSTRUCTOR_POS = 0;
    static const u_int BASE_SIZE       = 1;
  public:
    static void New(u_int Constr, u_int size) {
      JITStore::AllocBlock(Alice::ConVal, BASE_SIZE + size);
      JITStore::InitArg(JIT_V1, CONSTRUCTOR_POS, Constr);
    }
    static void GetConstructor(u_int Dest) {
      JITStore::GetArg(Dest, JIT_V1, CONSTRUCTOR_POS);
    }
    static void Sel(u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, JIT_V1, BASE_SIZE + pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V1, BASE_SIZE + pos, Value);
    }
  };

  class TagVal {
  public:
    static void New(u_int tag, u_int size) {
      JITStore::AllocBlock(Alice::TagToLabel(tag), size);
    }
    static void Sel(u_int Dest, u_int pos) {
      JITStore::GetArg(Dest, JIT_V1, pos);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, pos);
    }
    static void Put(u_int pos, u_int Value) {
      JITStore::InitArg(JIT_V1, pos, Value);
    }
    static void GetTag() {
      JITStore::Block::GetLabel();
      jit_subi_ui(JIT_R0, JIT_R0, Alice::MIN_TAG);
    }
  };

  class Vector {
  protected:
    static const u_int SIZE_POS  = 0;
    static const u_int BASE_SIZE = 1;
  public:
    static void New(u_int size) {
      JITStore::AllocBlock(Alice::Vector, BASE_SIZE + size);
      jit_movi_ui(JIT_R0, size);
      JITStore::IntToWord(JIT_R0, JIT_R0);
      JITStore::InitArg(JIT_V1, SIZE_POS, JIT_R0);
    }
    static void GetLength(u_int Dest) {
      JITStore::GetArg(Dest, JIT_V1, SIZE_POS);
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
