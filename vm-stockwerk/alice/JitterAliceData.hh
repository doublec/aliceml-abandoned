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
  class Cell : public ::Cell {
  public:
    static void New(u_int Ptr) {
      JITStore::AllocBlock(Ptr, Alice::Cell, SIZE);
    }
    static void Put(u_int Ptr, u_int Value) {
      JITStore::InitArg(Ptr, VAL_POS, Value);
    }
    static void Sel(u_int Dest, u_int Ptr) {
      JITStore::GetArg(Dest, Ptr, VAL_POS);
    }
  };

  class ConVal : public ::ConVal {
  public:
    static void New(u_int Ptr, u_int size) {
      JITStore::AllocBlock(Ptr, Alice::ConVal, BASE_SIZE + size);
    }
    static void InitConstr(u_int Ptr, u_int Value) {
      JITStore::InitArg(Ptr, CON_POS, Value);
    }
    static void GetConstructor(u_int Dest, u_int Ptr) {
      JITStore::GetArg(Dest, Ptr, CON_POS);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + pos, Value);
    }
  };

  class TagVal {
  public:
    static void New(u_int Ptr, u_int tag, u_int size) {
      JITStore::AllocBlock(Ptr, Alice::TagToLabel(tag), size);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, pos, Value);
    }
    static void GetTag(u_int Ptr) {
      JITStore::Block::GetLabel(JIT_R0, Ptr);
      jit_subi_ui(JIT_R0, JIT_R0, Alice::MIN_TAG);
    }
  };

  class Vector : public ::Vector {
  public:
    static void New(u_int Ptr, u_int size) {
      JITStore::AllocBlock(Ptr, Alice::Vector, BASE_SIZE + size);
      jit_movi_ui(JIT_R0, size);
      JITStore::IntToWord(JIT_R0, JIT_R0);
      JITStore::InitArg(Ptr, LENGTH_POS, JIT_R0);
    }
    static void GetLength(u_int Dest, u_int Ptr) {
      JITStore::GetArg(Dest, Ptr, LENGTH_POS);
    }
    static void Sel(u_int Dest, u_int Ptr, u_int pos) {
      JITStore::GetArg(Dest, Ptr, BASE_SIZE + pos);
    }
    static void Put(u_int Ptr, u_int pos, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + pos, Value);
    }
  };

  class Record : public ::Record {
  public:
    static void New(u_int Ptr, u_int n) {
      JITStore::AllocBlock(Ptr, Alice::Record, BASE_SIZE + n * 2);
      jit_movi_ui(JIT_R0, Store::IntToWord(n));
      JITStore::InitArg(Ptr, WIDTH_POS, JIT_R0);
    }
    static void InitLabel(u_int Ptr, u_int i, u_int Label) {
      JITStore::InitArg(Ptr, BASE_SIZE + i * 2, Label);
    }
    static void InitValue(u_int Ptr, u_int i, u_int Value) {
      JITStore::InitArg(Ptr, BASE_SIZE + i * 2 + 1, Value);
    }
  };

};

#endif
