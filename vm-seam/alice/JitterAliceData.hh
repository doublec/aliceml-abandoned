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
    static void New(u_int This) {
      JITStore::AllocBlock(This, Alice::Cell, SIZE);
    }
    static void Put(u_int This, u_int Value) {
      JITStore::InitArg(This, VALUE_POS, Value);
    }
    static u_int Sel() {
      return VALUE_POS;
    }
    static void Sel(u_int Dest, u_int This) {
      JITStore::GetArg(Dest, This, VALUE_POS);
    }
  };

  class ConVal : public ::ConVal {
  public:
    static void New(u_int This, u_int size) {
      JITStore::AllocBlock(This, Alice::ConVal, BASE_SIZE + size);
    }
    static void InitConstr(u_int This, u_int Value) {
      JITStore::InitArg(This, CON_POS, Value);
    }
    static void GetConstructor(u_int Dest, u_int This) {
      JITStore::GetArg(Dest, This, CON_POS);
    }
    static u_int Sel() {
      return BASE_SIZE;
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, BASE_SIZE + pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + pos, Value);
    }
  };

  class TagVal {
  public:
    static void New(u_int This, u_int tag, u_int size) {
      JITStore::AllocBlock(This, Alice::TagToLabel(tag), size);
    }
    static u_int Sel() {
      return 0;
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, pos, Value);
    }
    static void GetTag(u_int Dest, u_int This) {
      JITStore::Block::GetLabel(Dest, This);
      jit_subi_ui(Dest, Dest, Alice::MIN_TAG);
    }
  };

  class Vector : public ::Vector {
  public:
    static void New(u_int This, u_int size) {
      JITStore::AllocBlock(This, Alice::Vector, BASE_SIZE + size);
      jit_movi_p(JIT_R0, Store::IntToWord(size));
      JITStore::InitArg(This, LENGTH_POS, JIT_R0);
    }
    static void GetLength(u_int Dest, u_int This) {
      JITStore::GetArg(Dest, This, LENGTH_POS);
    }
    static u_int Sel() {
      return BASE_SIZE;
    }
    static void Sel(u_int Dest, u_int This, u_int pos) {
      JITStore::GetArg(Dest, This, BASE_SIZE + pos);
    }
    static void Put(u_int This, u_int pos, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + pos, Value);
    }
  };

  class Record : public ::Record {
  public:
    static void New(u_int This, u_int n) {
      JITStore::AllocBlock(This, Alice::Record, BASE_SIZE + n * 2);
      jit_movi_p(JIT_R0, Store::IntToWord(n));
      JITStore::InitArg(This, WIDTH_POS, JIT_R0);
    }
    static void GetWidth(u_int Dest, u_int This) {
      JITStore::GetArg(Dest, This, WIDTH_POS);
    }
    static void InitLabel(u_int This, u_int i, u_int Label) {
      JITStore::InitArg(This, BASE_SIZE + i * 2, Label);
    }
    static void InitValue(u_int This, u_int i, u_int Value) {
      JITStore::InitArg(This, BASE_SIZE + i * 2 + 1, Value);
    }
  };
};

#endif
