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

class SeamDll Stack: private Block {
protected:
  enum { TOP_POS, ARR_POS, SIZE };
protected:
  u_int GetTop() {
    return Store::DirectWordToInt(GetArg(TOP_POS));
  }
  void SetTop(u_int top) {
    InitArg(TOP_POS, top);
  }
  Block *GetArray() {
    return Store::DirectWordToBlock(GetArg(ARR_POS));
  }
  void Enlarge(u_int oldsize, u_int newsize) {
    Block *oa = GetArray();
    Block *na = Store::AllocBlock(STACKARRAY_LABEL, newsize);

    std::memcpy(na->GetBase(), oa->GetBase(), oldsize * sizeof(u_int));
    ReplaceArg(ARR_POS, na->ToWord());
  }
public:
  using Block::ToWord;

  u_int GetStackSize() {
    return GetTop();
  }
  void AllocArgFrame(u_int fsize) {
    u_int top  = GetTop();
    u_int max  = GetArray()->GetSize();
    u_int size = (top + fsize);

    SetTop(size);
    if (size >= max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearArgFrame(u_int fsize) {
    u_int top    = GetTop();
    u_int newtop = (top - fsize);
    Block *a     = GetArray();

    Assert(top >= fsize);
    SetTop(newtop);
    // Needs to move more than one argument
    a->InitArg((newtop - 1), a->GetArg(top - 1));
  }
  void ClearArgFrameZero(u_int fsize) {
    u_int top    = GetTop();
    u_int newtop = (top - fsize);

    Assert(top >= fsize);
    SetTop(newtop);
  }
  void AllocFrame(u_int fsize) {
    u_int top  = GetTop();
    u_int max  = GetArray()->GetSize();
    u_int size = (top + fsize);

    if (size >= max) {
      Stack::Enlarge(max, ((size * 3) >> 1));
    }
  }
  void ClearFrame(u_int fsize) {
    u_int top    = GetTop();
    u_int newtop = (top - fsize);

    Assert(top > fsize);
    SetTop(newtop);
  }
  void Push(word v) {
    u_int top = GetTop();

    Assert(top < GetArray()->GetSize());
    SetTop((top + 1));
    GetArray()->ReplaceArg(top, v);
  }
  void Push(int v) {
    u_int top = GetTop();

    Assert(top < GetArray()->GetSize());
    SetTop((top + 1));
    GetArray()->InitArg(top, v);
  }
  void SlowPush(word v) {
    u_int top = GetTop();
    Block *a  = GetArray();
    u_int max = a->GetSize();

    Assert(a->GetLabel() == STACKARRAY_LABEL);

    SetTop((top + 1));
    if (top < max) {
      a->ReplaceArg(top, v);
    }
    else {
      Stack::Enlarge(max, ((max * 3) >> 1));
      GetArray()->ReplaceArg(top, v);
    }
  }
  void SlowPush(int v) {
    u_int top = GetTop();
    Block *a  = GetArray();
    u_int max = a->GetSize();

    Assert(a->GetLabel() == STACKARRAY_LABEL);

    SetTop((top + 1));
    if (top < max) {
      a->InitArg(top, v);
    }
    else {
      Stack::Enlarge(max, ((max * 3) >> 1));
      GetArray()->InitArg(top, v);
    }
  }
  word Top() {
    return GetArray()->GetArg(GetTop() - 1);
  }
  word GetFrameArg(u_int f) {
    u_int top = GetTop();
    u_int pos = (top - 1 - f);
    
    Assert(top >= (f + 1));
    return GetArray()->GetArg(pos);
  }
  word GetAbsoluteArg(u_int f) {
    Assert(f < GetTop());
    return GetArray()->GetArg(f);
  }
  void PutFrameArg(u_int f, word v) {
    u_int top = GetTop();
    u_int pos = (top - 1 - f);
    
    Assert(top >= (f + 1));
    GetArray()->ReplaceArg(pos, v);
  }
  void PutFrameArg(u_int f, int v) {
    u_int top = GetTop();
    u_int pos = (top - 1 - f);
    
    Assert(top >= (f + 1));
    GetArray()->InitArg(pos, v);
  }
  word Pop() {
    u_int top = (GetTop() - 1);
    Block *a  = GetArray();

    // Assert(top >= 0);
    Assert(a->GetLabel() == STACKARRAY_LABEL);
    word value = a->GetArg(top);

    SetTop(top);
    return value;
  }
  word SlowPop() {
    u_int top = (GetTop() - 1);
    Block *a  = GetArray();

    // Assert(top >= 0);
    Assert(a->GetLabel() == STACKARRAY_LABEL);
    word value = a->GetArg(top);
    a->InitArg(top, 0);
    SetTop(top);
    return value;
  }
  int IsEmpty() {
    return (GetTop() == 0);
  }
  void Blank(u_int threshold) {
    u_int top    = GetTop();
    Block *a     = GetArray();
    u_int max    = a->GetSize();
    u_int newmax = (top + threshold); 

    newmax = ((newmax <= max) ? newmax : max);

    for (u_int i = top; i < newmax; i++) {
      a->InitArg(i, 0);
    }
    HeaderOp::EncodeSize(a, newmax);
  }
  static Stack *New(u_int s) {
    Block *p = Store::AllocBlock(STACK_LABEL, SIZE);
    Block *a = Store::AllocBlock(STACKARRAY_LABEL, s);
    
    p->InitArg(TOP_POS, 0);
    p->InitArg(ARR_POS, a->ToWord());

    return (Stack *) p;
  }
  static Stack *FromWord(word x) {
    Block *p = Store::WordToBlock(x);

    Assert((p == INVALID_POINTER) || (p->GetLabel() == STACK_LABEL));
    return (Stack *) p;
  }
  static Stack *FromWordDirect(word x) {
    Block *p = Store::DirectWordToBlock(x);

    Assert(p->GetLabel() == STACK_LABEL);
    return (Stack *) p;
  }
};

#endif
