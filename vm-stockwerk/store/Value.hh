#ifndef __value_hh__
#define __value_hh__

#include <string.h>
#include "base.hh"
#include "headerop.hh"
#include "pointerop.hh"

class Block {
protected:
  word ar[2];
public:
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
	(HeaderOp::GetHeader(PointerOp::RemoveTag(v)) < HeaderOp::GetHeader(this))) {
      Store::AddToIntgenSet(ToWord()); // Attention: static binding
    }
    ar[f] = v;
  }
  word ToWord() {
    return PointerOp::EncodeBlock(this);
  }
};

class Transient : private Block {
protected:
  void PerformBind(word v, t_label l) {
    switch (GetLabel()) {
    case PROMISE:
    case FUTURE:
      ReplaceArg(1, v);
      HeaderOp::EncodeLabel(this, REF);
      break;
    default:
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
	(HeaderOp::GetHeader(PointerOp::RemoveTag(v)) < HeaderOp::GetHeader(this))) {
      Store::AddToIntgenSet(ToWord()); // Attention: static binding
    }
    ar[f] = v;
  }
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
    
    memcpy(s + 1, ar + 1, size * sizeof(word));
    memset(s + (size + 1), 1, size * sizeof(word));

    return s;
  }
public:
  using Block::GetLabel;
  using Block::GetArg;
  using Block::InitArg;
  using Block::ReplaceArg;
  using Block::ToWord;
  u_int GetSize() { // GC Safe Size Assumption
    return (u_int) Store::WordToInt(GetArg(1));
  }
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
      return Enlarge();
    }
    else {
      return this;
    }
  }
  void ClearArgFrame(u_int fsize) {
    int top    = Store::WordToInt(GetArg(1));
    int newtop = top - fsize - 1;

    InitArg(1, Store::IntToWord(newtop));
    InitArg(newtop, GetArg(top - 1));
  }
  Stack *AllocFrame(u_int fsize) {
    int top   = Store::WordToInt(GetArg(1));
    u_int max = Block::GetSize();

    if ((top + fsize)  > (int) max) {
      return Enlarge();
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
      Stack *s = Enlarge();

      s->ReplaceArg((u_int) top, v);
      return s;
    }
  }
  word Top() {
    return GetArg((u_int) Store::WordToInt(GetArg(1)) - 1);
  }
  word Pop() {
    int top = Store::WordToInt(GetArg(1)) - 1;
 
    InitArg(1, Store::IntToWord(top));
    return GetArg((u_int) top);
  }
  void Clear() {
    InitArg(1, Store::IntToWord(2));
  }

  static Stack *New(u_int s) {
    Stack *gs = (Stack *) Store::AllocBlock(STACK, (s + 1));

    gs->InitStack();
    return gs;
  }
};

#endif
