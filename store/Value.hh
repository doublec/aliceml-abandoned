//
// Author:
//   Thorsten Brunklaus <brunklaus@ps.uni-sb.de>
//
// Copyright:
//   Thorsten Brunklaus, 2000-2001
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
  static const u_int HANDLER_POS = 0;
public:
  word *GetBase() {
    return (word *) this;
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
    return (word) ((u_int *) this)[f];
  }
  void InitArg(u_int f, word v) {
    AssertStore(f > INVALID_FIELD);
    AssertStore(f <= GetSize());
    ((word *) this)[f] = v;
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
      
      if ((valgen < mygen) && (!HeaderOp::IsChildish(this))) {
	Store::AddToIntgenSet(this);
      }
    }
    ((word *) this)[f] = v;
  }
  void ReplaceArg(u_int f, int v) {
    InitArg(f, Store::IntToWord(v));
  }
  Handler *GetHandler() {
    return ((GetLabel() == HANDLERBLOCK_LABEL) ?
	    PointerOp::DecodeHandler(this) : (Handler *) INVALID_POINTER);
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
private:
  static const u_int REF_POS = 0;
public:
  using Block::GetLabel;

  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  word GetArg() {
    return Block::GetArg(REF_POS);
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

class Chunk : private Block {
private:
  static const u_int BYTESIZE_POS = 0;
public:
  using Block::GetLabel;
  using Block::ToWord;

  char *GetBase() {
    return (char *) ((char *) this + sizeof(u_int));
  }
  u_int GetSize() {
    return Store::DirectWordToInt(GetArg(BYTESIZE_POS));
  }

  static Chunk *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == CHUNK_LABEL));
    return (Chunk *) p;
  }
  static Chunk *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    
    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == CHUNK_LABEL));
    return (Chunk *) p;
  }
};

#endif __STORE__VALUE_HH__
