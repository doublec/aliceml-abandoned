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

#include <cstring>
#include "base.hh"
#include "headerop.hh"
#include "pointerop.hh"

#define MAX(a, b) (((a) < (b)) ? (b) : (a))

class Block {
protected:
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
    return (word) ar[f];
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
  Block *Enlarge() {
    u_int size    = GetSize();
    u_int newsize = ((size * 3) >> 1);
    Block *b   = Store::AllocBlock(GetLabel(), newsize);

    std::memcpy(b + 1, ar + 1, size * sizeof(word));
    std::memset(b + (size + 1), 1, (newsize - size) * sizeof(word));

    return b;
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
protected:
  void PerformBind(word v, BlockLabel l) {
    BlockLabel label = GetLabel();

    if ((label == PROMISE) || (label == FUTURE)) {
      ReplaceArg(1, v);
      HeaderOp::EncodeLabel(this, REF);
    }
    else {
      Assert(0);
    }
  }
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::GetArg;
  using Block::InitArg;

  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  void Bind(word v) {
    PerformBind(v, REF);
  }
  void Cancel(word ex) {
    PerformBind(ex, CANCELLED);
  }
};

class Stack : private Block {
protected:
  Stack *Enlarge() {
    u_int size = Block::GetSize();
    Stack *s   = (Stack *) Store::AllocBlock(STACK, (size << 1));
    
    std::memcpy(s + 1, ar + 1, size * sizeof(word));
    std::memset(s + (size + 1), 1, size * sizeof(word));

    return s;
  }
  void ConvertToReference(Stack *s) {
    HeaderOp::EncodeLabel((Transient *) this, REF);
    InitArg(1, s->ToWord());
  }
public:
  using Block::GetLabel;
  using Block::ToWord;

  // to be determined
  void InitStack() {
    u_int size = GetSize();

    InitArg(1, Store::IntToWord(2));
    std::memset(ar + 2, 1,(size - 2) * sizeof(word));
  }
  void AllocArgFrame(u_int fsize) {
    u_int top = (u_int) Store::WordToInt(GetArg(1));
    u_int max = GetSize();

    InitArg(1, Store::IntToWord(top + fsize));
    if ((top + fsize) > max) {
      ConvertToReference(Stack::Enlarge());
    }
  }
  void ClearArgFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(1));
    int newtop = top - fsize;

    InitArg(1, Store::IntToWord(newtop));
    InitArg((newtop - 1), GetArg(top - 1));
  }
  void AllocFrame(u_int fsize) {
    u_int top = (u_int) Store::WordToInt(GetArg(1));
    u_int max = Block::GetSize();
    
    if ((top + fsize) > max) {
      ConvertToReference(Stack::Enlarge());
    }
  }
  void ClearFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(1));
    int newtop = top - fsize;

    InitArg(1, Store::IntToWord(newtop));
  }
  void Push(word v) {
    int top = Store::WordToInt(GetArg(1));
    
    Assert(top <= (int) GetSize());
    InitArg(1, Store::IntToWord(top + 1));
    ReplaceArg((u_int) top, v);
  }
  void SlowPush(word v) {
    u_int top = (u_int) Store::WordToInt(GetArg(1));
    u_int max = GetSize();
    
    InitArg(1, Store::IntToWord((int) (top + 1)));
    if (top <= max) {
      ReplaceArg(top, v);
    }
    else {
      Stack *s = Stack::Enlarge();

      s->ReplaceArg(top, v);
      ConvertToReference(s);
    }
  }
  word Top() {
    return GetArg((u_int) Store::WordToInt(GetArg(1)) - 1);
  }
  word GetFrameArg(u_int f) {
    return GetArg(((u_int) Store::WordToInt(GetArg(1)) - 1 - f));
  }
  word Pop() {
    static word zero = Store::IntToWord(0);
    int top          = Store::WordToInt(GetArg(1)) - 1;
    word value       = GetArg((u_int) top);

    InitArg(1, Store::IntToWord(top));
    InitArg(top, zero);
    return value;
  }
  void Clear() {
   int top = Store::WordToInt(GetArg(1));

    InitArg(1, Store::IntToWord(2));
    std::memset(ar + 2, 1, (top - 2) * sizeof(word));
  }
  int IsEmpty() {
    return (Store::WordToInt(GetArg(1)) == 2);
  }

  static Stack *New(u_int s) {
    Stack *gs = (Stack *) Store::AllocBlock(STACK, (s + 1));

    gs->InitStack();
    return gs;
  }
};

class Set : public Stack {
public:
  word GetArg(u_int f) {
    return ((Block *) this)->GetArg(f);
  }
  void InitArg(u_int f, word v) {
    ((Block *) this)->InitArg(f, v);
  }
  u_int GetSize() {
    return (u_int) (Store::WordToInt(((Block *) this)->GetArg(1)) - 1);
  }
  void MakeEmpty() {
    ((Block *) this)->InitArg(1, Store::IntToWord(2));
  }
  void Push(word v) {
    int top = Store::WordToInt(GetArg(1));
    
    Assert(top <= (int) GetSize());
    InitArg(1, Store::IntToWord(top + 1));
    InitArg((u_int) top, v);
  }

  static Set *New(u_int s) {
    return (Set *) Stack::New(s);
  }
};

#endif
