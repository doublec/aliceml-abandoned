#ifndef __VALUE_HH__
#define __VALUE_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include <string.h>
#include "base.hh"
#include "headerop.hh"
#include "pointerop.hh"

class Block {
protected:
  word ar[2];
public:
  word *GetBase() {
    return ar;
  }
  t_label GetLabel() {
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
    if (!PointerOp::IsInt(v) &&
	(HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v)) < HeaderOp::DecodeGeneration(this))) {
      Store::AddToIntgenSet(ToWord()); // Attention: static binding
    }
    ar[f] = v;
  }
  Block *Enlarge() {
    u_int size    = GetSize();
    u_int newsize = ((size * 3) >> 1);
    Block *b   = Store::AllocBlock(GetLabel(), newsize);

    memcpy(b + 1, ar + 1, size * sizeof(word));
    memset(b + (size + 1), 1, (newsize - size) * sizeof(word));

    return b;
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
protected:
  void PerformBind(word v, t_label l) {
    t_label label = GetLabel();

    if ((label == BlockLabel::PROMISE) || (label == BlockLabel::FUTURE)) {
      ReplaceArg(1, v);
      HeaderOp::EncodeLabel(this, BlockLabel::REF);
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
  void ReplaceArg(u_int f, word v) {
    Assert(f >INVALID_FIELD);
    Assert(f <= GetSize());
    if (!PointerOp::IsInt(v) &&
	(HeaderOp::DecodeGeneration(PointerOp::RemoveTag(v)) < HeaderOp::DecodeGeneration(this))) {
      Store::AddToIntgenSet(ToWord()); // Attention: static binding
    }
    ar[f] = v;
  }
  word ToWord() {
    return PointerOp::EncodeTransient(this);
  }
  void Bind(word v) {
    PerformBind(v, BlockLabel::REF);
  }
  void Cancel(word ex) {
    PerformBind(ex, BlockLabel::CANCELLED);
  }
};

class Stack : private Block {
protected:
  Stack *Enlarge() {
    u_int size = Block::GetSize();
    Stack *s   = (Stack *) Store::AllocBlock(BlockLabel::STACK, (size << 1));
    
    memcpy(s + 1, ar + 1, size * sizeof(word));
    memset(s + (size + 1), 1, size * sizeof(word));

    return s;
  }
public:
  using Block::GetLabel;
  using Block::GetSize;
  using Block::GetArg;
  using Block::InitArg;
  using Block::ReplaceArg;
  using Block::ToWord;
  void InitStack() {
    u_int size = Block::GetSize();

    InitArg(1, Store::IntToWord(2));
    memset(ar + 2, 1,(size - 2) * sizeof(word));
  }
  Stack *AllocArgFrame(u_int fsize) {
    int top   = Store::WordToInt(GetArg(1));
    u_int max = Block::GetSize();

    InitArg(1, Store::IntToWord(top + fsize));
    if ((top + fsize)  > (int) max) {
      return Stack::Enlarge();
    }
    else {
      return this;
    }
  }
  void ClearArgFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(1));
    int newtop = top - fsize;

    InitArg(1, Store::IntToWord(newtop));
    InitArg((newtop - 1), GetArg(top - 1));
  }
  Stack *AllocFrame(u_int fsize) {
    int top   = Store::WordToInt(GetArg(1));
    u_int max = Block::GetSize();

    if ((top + fsize)  > (int) max) {
      return Stack::Enlarge();
    }
    else {
      return this;
    }
  }
  void ClearFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(1));
    int newtop = top - fsize;

    InitArg(1, Store::IntToWord(newtop));
  }
  // Suitable for every Stack
  void Push(word v) {
    int top = Store::WordToInt(GetArg(1));
    
    Assert(top <= (int) Block::GetSize());
    InitArg(1, Store::IntToWord(top + 1));
    ReplaceArg((u_int) top, v);
  }
  // Only suitable for ROOTSTACK(!)
  void RootPush(word v) {
    int top = Store::WordToInt(GetArg(1));
    
    Assert(top <= (int) Block::GetSize());
    InitArg(1, Store::IntToWord(top + 1));
    InitArg((u_int) top, v);
  }
  Stack *SlowPush(word v) {
    int top   = Store::WordToInt(GetArg(1));
    u_int max = Block::GetSize();
    
    InitArg(1, Store::IntToWord(top + 1));
    if (top <= max) {
      ReplaceArg((u_int) top, v);
      return this;
    }
    else {
      Stack *s = Stack::Enlarge();

      s->ReplaceArg((u_int) top, v);
      return s;
    }
  }
  word Top() {
    return GetArg((u_int) Store::WordToInt(GetArg(1)) - 1);
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
    memset(ar + 2, 1, (top - 2) * sizeof(word));
  }

  static Stack *New(u_int s) {
    Stack *gs = (Stack *) Store::AllocBlock(BlockLabel::STACK, (s + 1));

    gs->InitStack();
    return gs;
  }
};

#endif
