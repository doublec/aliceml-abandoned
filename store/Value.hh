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

class SeamDll Block {
public:
  word *GetBase() {
    return (word *) this + 1;
  }
  BlockLabel GetLabel() {
    return HeaderOp::DecodeLabel(this);
  }
  u_int IsMutable() {
    return HeaderOp::DecodeMutableFlag(this);
  }
  void SetMutable(u_int flag) {
    AssertStore((flag == 0) || (flag == 1));
    HeaderOp::EncodeMutableFlag(this, flag);
  }
  u_int GetSize() {
    return HeaderOp::DecodeSize(this);
  }
  word GetArg(u_int f) {
    AssertStore(f < GetSize());
    return ((word *) this)[f + 1];
  }
  void InitArg(u_int f, word v) {
    AssertStore(f < GetSize());
    AssertStore(v != NULL);
    ((word *) this)[f + 1] = v;
  }
  void InitArg(u_int f, s_int v) {
    InitArg(f, Store::IntToWord(v));
  }
  void ReplaceArg(u_int f, word v) {
    AssertStore(f < GetSize());
    AssertStore(v != NULL);
    AssertStore(this->IsMutable());
    ((word *) this)[f + 1] = v;
    if (!PointerOp::IsInt(v)) {
      u_int valgen = HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v));
      u_int mygen  = HeaderOp::DecodeGeneration(this);
      
      if ((valgen < mygen) && (!HeaderOp::IsChildish(this))) {
	Store::AddToIntgenSet(this);
      }
    }
  }
  void ReplaceArg(u_int f, s_int v) {
    AssertStore(this->IsMutable());
    InitArg(f, Store::IntToWord(v));
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class SeamDll Transient : private Block {
protected:
  enum { REF_POS };
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
  void InitArg(s_int w) {
    Block::InitArg(REF_POS, w);
  }
  void ReplaceArg(word w) {
    Block::ReplaceArg(REF_POS, w);
  }
  void ReplaceArg(s_int w) {
    Block::InitArg(REF_POS, w);
  }
  void Become(BlockLabel l, word w) {
    AssertStore((GetLabel() >= MIN_TRANSIENT_LABEL) &&
		(GetLabel() <= MAX_TRANSIENT_LABEL) &&
		(GetLabel() != REF_LABEL));
    AssertStore(l >= MIN_TRANSIENT_LABEL && l <= MAX_TRANSIENT_LABEL);
    HeaderOp::EncodeLabel(this, l);
    Block::ReplaceArg(REF_POS, PointerOp::Deref(w));
  }
  void Become(BlockLabel l, s_int i) {
    AssertStore((GetLabel() >= MIN_TRANSIENT_LABEL) &&
		(GetLabel() <= MAX_TRANSIENT_LABEL) &&
		(GetLabel() != REF_LABEL));
    AssertStore(l >= MIN_TRANSIENT_LABEL && l <= MAX_TRANSIENT_LABEL);
    HeaderOp::EncodeLabel(this, l);
    Block::ReplaceArg(REF_POS, i);
  }
};

class SeamDll Chunk : private Block {
private:
  enum { BYTESIZE_POS, BASE_SIZE };
public:
  static const u_int maxSize = (MAX_BIGBLOCKSIZE - BASE_SIZE) * sizeof(u_int);

  using Block::GetLabel;
  using Block::IsMutable;
  using Block::SetMutable;
  using Block::ToWord;

  char *GetBase() {
    return (char *) this + 2 * sizeof(u_int);
  }
  u_int GetSize() {
    return Store::DirectWordToInt(GetArg(BYTESIZE_POS));
  }
  // String hashing function is taken from
  // 'Aho, Sethi, Ullman: Compilers..., page 436
  u_int Hash() {
    u_int len      = this->GetSize();
    const char *s  = this->GetBase();
    const char *sm = (s + len);
    unsigned h = 0, g;
    for (const char *p = s; p < sm; p++) {
      h = (h << 4) + (*p);
      if ((g = h & 0xf0000000)) {
	h = h ^ (g >> 24);
	h = h ^ g;
      }
    }
    return h;
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

class SeamDll DynamicBlock : protected Block {
protected:
  enum {ACTIVE_SIZE, BASE_SIZE };
public:
  using Block::GetBase;
  using Block::GetLabel;
  using Block::ToWord;
  u_int GetSize() {
    return (Block::GetSize() - BASE_SIZE);
  }
  u_int GetActiveSize() {
    return Store::DirectWordToInt(Block::GetArg(ACTIVE_SIZE));
  }
  u_int GetScanSize() {
    return (GetActiveSize() + BASE_SIZE);
  }
  void SetActiveSize(u_int size) {
    AssertStore(size <= DynamicBlock::GetSize());
    Block::ReplaceArg(ACTIVE_SIZE, size);
  }
  word GetArgUnchecked(u_int f) {
    return Block::GetArg(BASE_SIZE + f);
  }
  word GetArg(u_int f) {
    AssertStore(f < GetActiveSize());
    return GetArgUnchecked(f);
  }
  void InitArg(u_int f, word v) {
    AssertStore(f < GetActiveSize());
    Block::InitArg(BASE_SIZE + f, v);
  }
  void InitArg(u_int f, s_int v) {
    DynamicBlock::InitArg(f, Store::IntToWord(v));
  }
  void ReplaceArg(u_int f, word v) {
    AssertStore(f < GetActiveSize());
    Block::ReplaceArg(BASE_SIZE + f, v);
  }
  void ReplaceArg(u_int f, s_int v) {
    DynamicBlock::ReplaceArg(f, Store::IntToWord(v));
  }

  static DynamicBlock *FromWord(word x) {
    Block *p = Store::WordToBlock(x);
    AssertStore((p == INVALID_POINTER) || (p->GetLabel() == DYNAMIC_LABEL));
    return STATIC_CAST(DynamicBlock *, p);
  }
  static DynamicBlock *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);
    AssertStore(p->GetLabel() == DYNAMIC_LABEL);
    return STATIC_CAST(DynamicBlock *, p);
  }
};

#endif
