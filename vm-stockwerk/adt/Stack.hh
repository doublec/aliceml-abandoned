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
#ifndef __STACK_HH__
#define __STACK_HH__

#if defined(INTERFACE)
#pragma interface
#endif

#include <cstring>

class Stack : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int TOP_POS = 1;
  static const u_int ARR_POS = 2;
protected:
  enum labels {
    STACK_LABEL      = MIN_HELPER_LABEL,
    STACKARRAY_LABEL = (MIN_HELPER_LABEL + 1)
  };

  void Enlarge(u_int oldsize, u_int newsize) {
    Block *oa = Store::UnsafeWordToBlock(GetArg(ARR_POS));
    Block *na = Store::AllocBlock((BlockLabel) STACKARRAY, newsize);

    std::memcpy(na->GetBase(), oa->GetBase(), oldsize * sizeof(word));
    ReplaceArg(ARR_POS, na->ToWord());
  }
public:
  using Block::ToWord;

  u_int GetStackSize() {
    return (Store::UnsafeWordToInt(GetArg(TOP_POS)) - 1);
  }
  void AllocArgFrame(u_int fsize) {
    u_int top  = Store::UnsafeWordToInt(GetArg(TOP_POS));
    u_int max  = Store::UnsafeWordToBlock(GetArg(ARR_POS))->GetSize();
    u_int size = (top + fsize);

    InitArg(TOP_POS, Store::IntToWord(size));
    if (size > max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearArgFrame(u_int fsize) {
    int top    = Store::UnsafeWordToInt(GetArg(TOP_POS));
    int newtop = (top - fsize);
    Block *a   = Store::UnsafeWordToBlock(GetArg(ARR_POS));

    InitArg(TOP_POS, Store::IntToWord(newtop));
    a->InitArg((newtop - 1), a->GetArg(top - 1));
  }
  void AllocFrame(u_int fsize) {
    u_int top  = Store::UnsafeWordToInt(GetArg(TOP_POS));
    u_int max  = Store::UnsafeWordToBlock(GetArg(ARR_POS))->GetSize();
    u_int size = (top + fsize);

    if (size > max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearFrame(u_int fsize) {
    int top    = Store::UnsafeWordToInt(GetArg(TOP_POS));
    int newtop = (top - fsize);

    InitArg(TOP_POS, Store::IntToWord(newtop));
  }
  void Push(word v) {
    int top = Store::UnsafeWordToInt(GetArg(TOP_POS));

    Assert(top <= Store::UnsafeWordToBlock(GetArg(ARR_POS))->GetSize());
    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    Store::UnsafeWordToBlock(GetArg(ARR_POS))->ReplaceArg(top, v);
  }
  void SlowPush(word v) {
    u_int top = Store::UnsafeWordToInt(GetArg(TOP_POS));
    Block *a  = Store::UnsafeWordToBlock(GetArg(ARR_POS));
    u_int max = a->GetSize();

    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    if (top <= max) {
      a->ReplaceArg(top, v);
    }
    else {
      Stack::Enlarge(max, ((max * 3) >> 1));
      Store::UnsafeWordToBlock(GetArg(ARR_POS))->ReplaceArg(top, v);
    }
  }
  word Top() {
    return Store::UnsafeWordToBlock(GetArg(ARR_POS))->
      GetArg(Store::WordToInt(GetArg(TOP_POS)) - 1);
  }
  word GetFrameArg(u_int f) {
    u_int top = Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(pos >= 1);
    return Store::UnsafeWordToBlock(GetArg(ARR_POS))->GetArg(pos);
  }
  void PutFrameArg(u_int f, word v) {
    u_int top = Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(pos >= 1);
    return Store::UnsafeWordToBlock(GetArg(ARR_POS))->ReplaceArg(pos, v);
  }
  word Pop() {
    u_int top  = (Store::WordToInt(GetArg(TOP_POS)) - 1);
    word value = Store::UnsafeWordToBlock(GetArg(ARR_POS))->GetArg(top);

    InitArg(TOP_POS, Store::IntToWord(top));
    return value;
  }
  int IsEmpty() {
    return (Store::WordToInt(GetArg(TOP_POS)) == 1);
  }
  void Blank(u_int threshold) {
    u_int top    = Store::UnsafeWordToInt(GetArg(TOP_POS));
    Block *a     = Store::UnsafeWordToBlock(GetArg(ARR_POS));
    u_int max    = a->GetSize();
    u_int newmax = (top + threshold); 

    newmax = ((newmax <= max) ? newmax : max);

    for (u_int i = top; i <= newmax; i++) {
      a->InitArg(i, Store::IntToWord(0));
    }
    HeaderOp::EncodeSize(a, newmax);
  }
  static Stack *FromBlock(Block *x) {
    return (Stack *) x;
  }
  static Stack *New(u_int s) {
    Block *p = Store::AllocBlock((BlockLabel) STACK_LABEL, SIZE);
    Block *a = Store::AllocBlock((BlockLabel) STACKARRAY_LABEL, s);
    
    p->InitArg(TOP_POS, Store::IntToWord(1));
    p->InitArg(ARR_POS, a->ToWord());

    return FromBlock(p);
  }
  static Stack *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == (BlockLabel) STACK_LABEL));
    return FromBlock(p);
  }
};

#endif
