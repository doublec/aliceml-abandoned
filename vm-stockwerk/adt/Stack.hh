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
#ifndef __ADT__STACK_HH__
#define __ADT__STACK_HH__

#if defined(INTERFACE)
#pragma interface "adt/Stack.hh"
#endif

#include "store/Store.hh"
#include <cstring>

class Stack : private Block {
private:
  static const u_int SIZE    = 2;
  static const u_int TOP_POS = 1;
  static const u_int ARR_POS = 2;
protected:
  void Enlarge(u_int oldsize, u_int newsize) {
    Block *oa = Store::DirectWordToBlock(GetArg(ARR_POS));
    Block *na = Store::AllocBlock(STACKARRAY_LABEL, newsize);

    std::memcpy(na->GetBase(), oa->GetBase(), oldsize * sizeof(word));
    ReplaceArg(ARR_POS, na->ToWord());
  }
public:
  using Block::ToWord;

  u_int GetStackSize() {
    return (Store::DirectWordToInt(GetArg(TOP_POS)) - 1);
  }
  void AllocArgFrame(u_int fsize) {
    u_int top  = Store::DirectWordToInt(GetArg(TOP_POS));
    u_int max  = Store::DirectWordToBlock(GetArg(ARR_POS))->GetSize();
    u_int size = (top + fsize);

    InitArg(TOP_POS, Store::IntToWord(size));
    if (size > max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearArgFrame(u_int fsize) {
    u_int top    = Store::DirectWordToInt(GetArg(TOP_POS));
    u_int newtop = (top - fsize);
    Block *a     = Store::DirectWordToBlock(GetArg(ARR_POS));

    Assert(top > fsize);
    InitArg(TOP_POS, Store::IntToWord(newtop));
    a->InitArg((newtop - 1), a->GetArg(top - 1));
  }
  void AllocFrame(u_int fsize) {
    u_int top  = Store::DirectWordToInt(GetArg(TOP_POS));
    u_int max  = Store::DirectWordToBlock(GetArg(ARR_POS))->GetSize();
    u_int size = (top + fsize);

    if (size > max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearFrame(u_int fsize) {
    u_int top    = Store::DirectWordToInt(GetArg(TOP_POS));
    u_int newtop = (top - fsize);

    Assert(top > fsize);
    InitArg(TOP_POS, Store::IntToWord(newtop));
  }
  void Push(word v) {
    u_int top = Store::DirectWordToInt(GetArg(TOP_POS));

    Assert(top <= Store::DirectWordToBlock(GetArg(ARR_POS))->GetSize());
    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    Store::DirectWordToBlock(GetArg(ARR_POS))->ReplaceArg(top, v);
  }
  void Push(int v) {
    u_int top = Store::DirectWordToInt(GetArg(TOP_POS));

    Assert(top <= Store::DirectWordToBlock(GetArg(ARR_POS))->GetSize());
    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    Store::DirectWordToBlock(GetArg(ARR_POS))->InitArg(top, Store::IntToWord(v));
  }
  void SlowPush(word v) {
    u_int top = Store::DirectWordToInt(GetArg(TOP_POS));
    Block *a  = Store::DirectWordToBlock(GetArg(ARR_POS));
    u_int max = a->GetSize();

    Assert(a->GetLabel() == STACKARRAY_LABEL);

    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    if (top <= max) {
      a->ReplaceArg(top, v);
    }
    else {
      Stack::Enlarge(max, ((max * 3) >> 1));
      Store::DirectWordToBlock(GetArg(ARR_POS))->ReplaceArg(top, v);
    }
  }
  void SlowPush(int v) {
    u_int top = Store::DirectWordToInt(GetArg(TOP_POS));
    Block *a  = Store::DirectWordToBlock(GetArg(ARR_POS));
    u_int max = a->GetSize();

    Assert(a->GetLabel() == STACKARRAY_LABEL);

    InitArg(TOP_POS, Store::IntToWord((top + 1)));
    if (top <= max) {
      a->InitArg(top, Store::IntToWord(v));
    }
    else {
      Stack::Enlarge(max, ((max * 3) >> 1));
      Store::DirectWordToBlock(GetArg(ARR_POS))->InitArg(top, Store::IntToWord(v));
    }
  }
  word Top() {
    return Store::DirectWordToBlock(GetArg(ARR_POS))->
      GetArg(Store::WordToInt(GetArg(TOP_POS)) - 1);
  }
  word GetFrameArg(u_int f) {
    u_int top = Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(top > f + 1);
    return Store::DirectWordToBlock(GetArg(ARR_POS))->GetArg(pos);
  }
  void PutFrameArg(u_int f, word v) {
    u_int top = Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(top > f + 1);
    return Store::DirectWordToBlock(GetArg(ARR_POS))->ReplaceArg(pos, v);
  }
  void PutFrameArg(u_int f, int v) {
    u_int top = Store::WordToInt(GetArg(TOP_POS));
    u_int pos = (top - 1 - f);
    
    Assert(top > f + 1);
    return Store::DirectWordToBlock(GetArg(ARR_POS))->InitArg(pos, Store::IntToWord(v));
  }
  word Pop() {
    u_int top  = (Store::WordToInt(GetArg(TOP_POS)) - 1);
    Block *a   = Store::DirectWordToBlock(GetArg(ARR_POS));
    Assert(top >= 1);
    Assert(a->GetLabel() == STACKARRAY_LABEL);
    word value = a->GetArg(top);

    InitArg(TOP_POS, Store::IntToWord(top));
    return value;
  }
  int IsEmpty() {
    return (Store::WordToInt(GetArg(TOP_POS)) == 1);
  }
  void Blank(u_int threshold) {
    u_int top    = Store::DirectWordToInt(GetArg(TOP_POS));
    Block *a     = Store::DirectWordToBlock(GetArg(ARR_POS));
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
    Block *p = Store::AllocBlock(STACK_LABEL, SIZE);
    Block *a = Store::AllocBlock(STACKARRAY_LABEL, s);
    
    p->InitArg(TOP_POS, Store::IntToWord(1));
    p->InitArg(ARR_POS, a->ToWord());

    return FromBlock(p);
  }
  static Stack *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == STACK_LABEL));
    return FromBlock(p);
  }
};

#endif __ADT__STACK_HH__
