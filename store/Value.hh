//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000
//
// Last Change:
//   $Date$ by $Author$
//   $Revision$
//
#ifndef __STORE__VALUE_HH__
#define __STORE__VALUE_HH__

#if defined(INTERFACE)
#pragma interface "store/Value.hh"
#endif

class Block {
private:
  static const u_int HANDLER_POS = 1;
  word ar[2];
public:
  word *GetBase() {
    return (ar + 1);
  }
  BlockLabel GetLabel() {
    return HeaderOp::DecodeLabel(this);
  }
  u_int GetSize() {
    return HeaderOp::DecodeSize(this);
  }
  word GetArg(u_int f) {
    AssertStore(f > INVALID_FIELD);
    AssertStore(f <= GetSize());
    return ar[f];
  }
  void InitArg(u_int f, word v) {
    AssertStore(f > INVALID_FIELD);
    AssertStore(f <= GetSize());
    ar[f] = v;
  }
  void InitArg(u_int f, int v) {
    InitArg(f, Store::IntToWord(v));
  }
  void ReplaceArg(u_int f, word v) {
    AssertStore(f > INVALID_FIELD);
    AssertStore(f <= GetSize());
    if (!PointerOp::IsInt(v)) {
      u_int valgen = HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v));
      u_int mygen  = HeaderOp::DecodeGeneration(this);
      
      if ((valgen < mygen) && (!HeaderOp::HasIntgenMark(this))) {
	Store::AddToIntgenSet(this);
      }
    }
    ar[f] = v;
  }
  void ReplaceArg(u_int f, int v) {
    InitArg(f, Store::IntToWord(v));
  }
  Handler *GetHandler() {
    return (Handler *) ((GetLabel() != HANDLER_BLOCK_LABEL) ? INVALID_POINTER :
			Store::DirectWordToUnmanagedPointer(GetArg(HANDLER_POS)));
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
private:
  static const u_int REF_POS = 1;
public:
  using Block::GetLabel;

  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  word GetArg() {
    return Block::GetArg(1);
  }
  void InitArg(word w) {
    Block::InitArg(REF_POS, w);
  }
  void InitArg(int w) {
    Block::InitArg(REF_POS, w);
  }
  void ReplaceArg(word w) {
    Block::ReplaceArg(REF_POS, w);
  }
  void ReplaceArg(int w) {
    Block::InitArg(REF_POS, w);
  }
  void Become(BlockLabel l, word w) {
    AssertStore((GetLabel() >= MIN_TRANSIENT_LABEL) &&
		(GetLabel() <= MAX_TRANSIENT_LABEL) &&
		(GetLabel() != REF_LABEL));
    AssertStore(l >= MIN_TRANSIENT_LABEL && l <= MAX_TRANSIENT_LABEL);
    HeaderOp::EncodeLabel(this, l);
    Block::ReplaceArg(REF_POS, w);
  }
  void Become(BlockLabel l, int i) {
    AssertStore((GetLabel() >= MIN_TRANSIENT_LABEL) &&
		(GetLabel() <= MAX_TRANSIENT_LABEL) &&
		(GetLabel() != REF_LABEL));
    AssertStore(l >= MIN_TRANSIENT_LABEL && l <= MAX_TRANSIENT_LABEL);
    HeaderOp::EncodeLabel(this, l);
    Block::ReplaceArg(REF_POS, i);
  }
};

#endif __STORE__VALUE_HH__
