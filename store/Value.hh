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
#ifndef __VALUE_HH__
#define __VALUE_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include "base.hh"
#include "headerop.hh"
#include "pointerop.hh"

#define MAX(a, b) (((a) < (b)) ? (b) : (a))

class Block {
private:
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
    Assert(f > INVALID_FIELD);
    Assert(f <= GetSize());
    return ar[f];
  }
  void InitArg(u_int f, word v) {
    Assert(f > INVALID_FIELD);
    Assert(f <= GetSize());
    ar[f] = v;
  }
  void ReplaceArg(u_int f, word v) {
    Assert(f > INVALID_FIELD);
    Assert(f <= GetSize());
    if (!PointerOp::IsInt(v)) {
      u_int valgen = HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v));
      u_int mygen  = HeaderOp::DecodeGeneration(this);
      
      if ((valgen < mygen) && (!HeaderOp::HasIntgenMark(this))) {
	Store::AddToIntgenSet(this);
      }
    }
    ar[f] = v;
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
public:
  using Block::GetLabel;

  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  word GetArg() {
    return Block::GetArg(1);
  }
  void InitArg(word w) {
    Block::InitArg(1, w);
  }
  void ReplaceArg(word w) {
    Block::ReplaceArg(1, w);
  }
  void Become(BlockLabel l, word w) {
    Assert(GetLabel() >= MIN_TRANSIENT && GetLabel() <= MAX_TRANSIENT &&
	   GetLabel() != REF);
    Assert(l >= MIN_TRANSIENT && l <= MAX_TRANSIENT);
    HeaderOp::EncodeLabel(this, l);
    Block::ReplaceArg(1, w);
  }
};

class Stack : private Block {
private:
  static const u_int TOP_POS = 1;
protected:
  Stack *Enlarge(u_int oldsize, u_int newsize) {
    Block *p = Store::AllocStack(newsize);

    std::memcpy(p->GetBase(), GetBase(), oldsize * sizeof(word));
    return (Stack *) p;
  }
  void ConvertToReference(Stack *s) {
    HeaderOp::EncodeLabel((Transient *) this, REF);
    InitArg(TOP_POS, s->ToWord());
  }
public:
  using Block::ToWord;

  u_int GetSize() {
    return (u_int) (Store::WordToInt(GetArg(TOP_POS)) - 1);
  }
  void AllocArgFrame(u_int fsize) {
    u_int top  = (u_int) Store::WordToInt(GetArg(TOP_POS));
    u_int max  = Block::GetSize();
    u_int size = (top + fsize);

    InitArg(TOP_POS, Store::IntToWord(size));
    if (size > max) {
      ConvertToReference(Stack::Enlarge(max, (size << 1)));
    }
  }
  void ClearArgFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(TOP_POS));
    int newtop = (top - fsize);

    InitArg(TOP_POS, Store::IntToWord(newtop));
    InitArg((newtop - 1), GetArg(top - 1));
  }
  void AllocFrame(u_int fsize) {
    u_int top  = (u_int) Store::WordToInt(GetArg(TOP_POS));
    u_int max  = Block::GetSize();
    u_int size = (top + fsize);

    if (size > max) {
      ConvertToReference(Stack::Enlarge(max, (size << 1)));
    }
  }
  void ClearFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(TOP_POS));
    int newtop = (top - fsize);

    InitArg(TOP_POS, Store::IntToWord(newtop));
  }
  void Push(word v) {
    int top = Store::WordToInt(GetArg(TOP_POS));
    
    Assert(top <= (int) Block::GetSize());
    InitArg(TOP_POS, Store::IntToWord(top + 1));
    ReplaceArg((u_int) top, v);
  }
  void SlowPush(word v) {
    u_int top = (u_int) Store::WordToInt(GetArg(TOP_POS));
    u_int max = Block::GetSize();

    InitArg(TOP_POS, Store::IntToWord((int) (top + 1)));
    if (top <= max) {
      ReplaceArg(top, v);
    }
    else {
      Stack *s = Stack::Enlarge(max, (max << 1));

      s->ReplaceArg(top, v);
      ConvertToReference(s);
    }
  }
  word Top() {
    return GetArg((u_int) Store::WordToInt(GetArg(TOP_POS)) - 1);
  }
  word GetFrameArg(u_int f) {
    u_int top = (u_int) Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(pos >= 2);
    return GetArg(pos);
  }
  word Pop() {
    u_int top  = (Store::WordToInt(GetArg(TOP_POS)) - 1);
    word value = GetArg(top);

    InitArg(TOP_POS, Store::IntToWord(top));
    return value;
  }
  int IsEmpty() {
    return (Store::WordToInt(GetArg(TOP_POS)) == 2);
  }

  static Stack *FromBlock(Block *x) {
    return (Stack *) x;
  }
  static Stack *New(u_int s) {
    Block *p = Store::AllocStack((s + 1));

    p->InitArg(TOP_POS, Store::IntToWord(2));
    return FromBlock(p);
  }
  static Stack *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == STACK));
    return FromBlock(p);
  }
};

class Set : public Stack {
private:
  static const u_int TOP_POS = 1;
public:
  word GetArg(u_int f) {
    return ((Block *) this)->GetArg(f);
  }
  void InitArg(u_int f, word v) {
    ((Block *) this)->InitArg(f, v);
  }
  void MakeEmpty() {
    ((Block *) this)->InitArg(TOP_POS, Store::IntToWord(2));
  }

  static Set *New(u_int s) {
    return (Set *) Stack::New(s);
  }
  static Set *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == STACK));
    return (Set *) p;
  }
};

#endif
